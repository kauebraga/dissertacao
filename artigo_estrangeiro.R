source("R/setup.R")


# abrir e juntar dados
forpadrao <- read_rds("../data/acess/acess_forpadrao.rds") %>% mutate(tipo = "PR")
forp50 <- read_rds("../data/acess/acess_forcorrigidocm.rds") %>% mutate(tipo = "P50")
forp85 <- read_rds("../data/acess/acess_forcorrigidoce.rds") %>% mutate(tipo = "P85")

juntos_cenario <- rbind(forpadrao, forp50, forp85)


# trazer informacoes de populacao e renda
hex_for <- read_rds("../data/dados2019_v1.0_20200116.rds") %>%
  filter(nome_muni == "Fortaleza")

hex_for_renda <- hex_for %>%
  filter(modo == "tp" & pico == 1) %>%
  select(id_hex, P001, R002, R003) %>%
  st_set_geometry(NULL)

# trazer renda para base de acess
juntos_cenario_renda <- juntos_cenario %>%
  left_join(hex_for_renda, by = c("origin" = "id_hex"))

# calcular diferenca entre cenarios
juntos_cenario_tt <- juntos_cenario_renda %>%
  filter(!is.na(R003)) %>%
  select(origin, tipo, CMA_TT_65, P001, R003) %>%
  spread(tipo, CMA_TT_65) %>%
  # dif 1: PR - 50; dif2: P50 - P85
  mutate(dif_1 = PR - P50) %>%
  mutate(dif_2 = P50 - P85) %>%
  mutate(dif_1_log = log(P50/PR)) %>%
  mutate(dif_1_log_tc = ifelse(dif_1_log > 0.70, 0.70, 
                               ifelse(dif_1_log < -0.70, -0.70, dif_1_log))) %>%
  mutate(dif_2_log = log(P50/P85)) %>%
  mutate(dif_2_log_tc = ifelse(dif_2_log > 0.70, 0.70, 
                               ifelse(dif_2_log < 0, 0, dif_2_log))) %>%
  mutate(dif_2_perc = (P50 - P85)/P50) %>%
  mutate(dif_2_perc = ifelse(dif_2_perc < 0, 0, dif_2_perc)) %>%
  mutate(dif_2_perc_tc = ifelse(dif_2_perc > 0.75, 0.75, dif_2_perc))

juntos_cenario_tt %>% st_set_geometry(NULL) %>% group_by(R002) %>% summarise(median(dif_1_log))


ggplot()+
  geom_boxplot(data = juntos_cenario_tt, aes(x = as.factor(R003), y = dif_1_log, weight=P001))+
  geom_hline(yintercept = 0)+
  labs(y = "P50 - PR", x = "Decis de renda",
       title = "Diferença relativa entre GTFS P50 e GTFS Programado",
       subtitle = "Eixo negativo indica quando o GTFS programado superestima a acessibilidade em relação ao GTFS P50")

ggplot()+
  geom_hline(yintercept = 0)+
  geom_boxplot(data = juntos_cenario_tt, aes(x = as.factor(R003), y = -dif_2_log, weight=P001))+
  labs(y = "P85 - P50", x = "Decis de renda",
       title = "Diferença relativa entre GTFS P85 e GTFS P50",
       subtitle = "Eixo negativo indica quando o GTFS P50 superestima a acessibilidade em relação ao GTFS P85")

