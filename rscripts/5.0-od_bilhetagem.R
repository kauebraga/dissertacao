# abrir bilhetagem
bi <- read_csv("../data/bilhetagem_integrado/2018/bilhetagemintegrado_2018-09-12.csv")

# abrir registros
bu_registro <- fread("../data-raw/registro_bu/cargatotal_novo.csv") %>%
  # so estudantes
  filter(TipoCartao %in% c(2, 11)) %>%
  select(id = NumeroSigom, nasc = DataNascimento) %>%
  mutate(nasc = as.Date(nasc, format = "%d/%m/%Y"))

# so para estudante com no maximo 18 anos
bu_registro_ok <- bu_registro %>%
  filter(nasc > "2000-09-12") %>%
  mutate(id = as.numeric(id)) %>%
  distinct(id, .keep_all = TRUE)

bi_id_ok <- bi %>%
  mutate(loc_ok = ifelse(is.na(lon), "nao", "sim")) %>%
  count(id, loc_ok) %>%
  ungroup() %>%
  filter(loc_ok == "nao")


# tirar inteiros e integracoes e falhas no georreferenciamento
bi_filter <- bi %>%
  filter(id != 0) %>%
  filter(integracao == "N") %>%
  # tirar aquele pessoal que nao pelo menos uma nao-ok
  filter(id %nin% bi_id_ok$id) %>%
  arrange(id, hora) %>%
  mutate(momento = as.ITime(momento)) %>%
  # filtrar so vale transporote e cartao de estudante
  filter(tipo_cartao %in% c("Estudante", "Vale Transporte"))

# ajeitar o pessoal da educacao
bi_filter <- bi_filter %>%
  left_join(bu_registro_ok) %>%
  filter(!(is.na(nasc) & tipo_cartao == "Estudante"))

# quais usuarios realizaram mais que uma viagem?
bi_2viagens <- bi_filter %>%
  count(id) %>%
  filter(n >= 2)

# a origem serao as primeiras viagens realizadas em hora pico e o destino a viagem
# viagens em pico
bi_pico <- bi_filter %>%
  # filtrar usuarios que realizaram mais que duas viagens
  filter(id %in% bi_2viagens$id) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  filter(momento <= as.ITime("08:00:00"))

# pra cima!
bi_od <- bi_filter %>%
  filter(id %in% bi_pico$id) %>%
  # basic check: as duas validacoes tem que ter pelo menos 3h de diferenca
  group_by(id) %>%
  mutate(dif = difftime(hora, lag(hora), units = "hour")) %>%
  ungroup() %>%
  filter(dif >= 3 | is.na(dif)) %>%
  add_count(id) %>%
  filter(n >= 2) %>%
  # selecionar as duas primeiras validacoes
  group_by(id) %>%
  slice(1:2)


# geolocalizar nos hexagonos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hex <- read_rds("../data/hex_municipio/hex_for_09.rds") %>% select(id_hex)
bi_od_sf <- to_spatial(bi_od)

# fazer join espacial
bi_od_sf_hex <- st_join(bi_od_sf, hex) %>% sfc_as_cols()

# transformar para formato "OD"
bi_od_fim <- bi_od_sf_hex %>%
  group_by(id) %>%
  mutate(par_id = paste0(id_hex, "-", lead(id_hex))) %>%
  mutate(dif_viagens = difftime(hora, lead(hora), units = "hour")) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(dif_viagens = abs(as.numeric(dif_viagens))) %>%
  select(id, tipo_cartao, dif_viagens, par_id)


# trazer tempos de viagens
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ttmatrix <- fread("../data/ttmatrix_fim/ttmatrix_forcorrigidocm.csv")
head(ttmatrix)

# juntar bases
bi_od_ttime <- bi_od_fim %>%
  left_join(ttmatrix %>% select(par_id, tt_median), 
            by = "par_id") %>%
  mutate(tt_median = tt_median/60)

# tempos de viagem para cada categoria VALE TRANSPORTE!
bi_od_ttime %>%
  filter(tipo_cartao == "Vale Transporte") %>%
  ggplot()+
  geom_histogram(aes(x = tt_median))+
  labs(x = "Tempo de viagem", y = "Quantidade de viagens")+
  theme_ipsum_rc()+
  theme(plot.margin = unit(c(1,1,1,1), "mm"))


# tempos de viagem para categoria ESTUDANTE!
bi_od_ttime %>%
  filter(tipo_cartao == "Estudante") %>%
  ggplot()+
  geom_histogram(aes(x = tt_median))+
  labs(x = "Tempo de viagem", y = "Quantidade de viagens")+
  theme_ipsum_rc()+
  theme(plot.margin = unit(c(1,1,1,1), "mm"))

# fazendo um grafico so
bi_od_ttime %>%
  ggplot()+
  geom_density(aes(x = tt_median, fill = tipo_cartao), alpha = 0.4)+
  labs(x = "Tempo de viagem", y = "Densidade de viagens", fill = "Tipo de pagamento")+
  scale_fill_brewer(palette = "Set1")+
  theme_ipsum_rc()+
  theme(plot.margin = unit(c(1,1,1,1), "mm"),
        legend.position = "bottom")

ggsave("figure/5-bi_tt_density.png", units = "cm", width = 16, height = 7, dpi = 300)


# fazendo um grafico so
bi_od_ttime %>%
  ggplot()+
  geom_boxplot(aes(y = tt_median, x = tipo_cartao), alpha = 0.4)+
  labs(y = "Distribuição do tempo de viagem", x= "")+
  geom_abline(slope = 0, intercept = 51, color = "red")+
  geom_abline(slope = 0, intercept = 64, color = "red")+
  theme_ipsum_rc()+
  coord_flip()+
  theme(plot.margin = unit(c(1,1,1,1), "mm"),
        legend.position = "bottom")

ggsave("figure/5-bi_tt_bp.png", units = "cm", width = 16, height = 7, dpi = 300)



# tabela final com p50, p90
ttmatrix_od <- bi_od_ttime %>%
  group_by(tipo_cartao) %>%
  summarise(count = n(),
            tt_median_cat = median(tt_median, na.rm = TRUE),
            p75 = quantile(tt_median, 0.75, na.rm = TRUE))
