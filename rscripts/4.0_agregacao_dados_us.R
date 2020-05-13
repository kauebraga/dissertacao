# SCRIPT PARA AGREGACAO DOS DADOS DE USO DO SOLO NOS HEX --------------------------------------

# # saude 
# cnes <- read_rds("../data/saude/saude_ativo_2015.rds") %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326)

# educacao
escolas <- read_csv("../data/censo_escolar/censo_escolar_2015.csv") %>%
  dplyr::filter(!is.na(lat)) %>%
  filter(municipio == "Fortaleza") %>%
  filter(rede %in% c("Municipal", "Estadual", "Federal")) %>%
  select(cod_escola, uf, municipio, cod_mun = CO_MUNICIPIO, rede, mat_infantil, mat_fundamental, mat_medio, lon, lat) %>%
  mutate(mat_total = pmap_dbl(list(.$mat_infantil, .$mat_fundamental, .$mat_medio), 
                                  sum)) %>%
  # gather(tipo, mat_n, mat_infantil:mat_medio) %>%
  select(cod_escola, mat_total, lon, lat) %>%
  to_spatial()
# # Ajeitar nome do municipio
# mutate(municipio = tolower(municipio)) %>%
# mutate(municipio = iconv(municipio, to="UTF-8")) %>%
# mutate(municipio = iconv(municipio, to="ASCII//TRANSLIT")) %>%

# empregos  
empregos <- read_rds("../data/rais/rais_2017_corrigido.rds")
# Corte: 1500 empregos
empregos[, qt_vinc_ativos2 := ifelse(qt_vinc_ativos > 1500, 1500, qt_vinc_ativos2)]
empregos <- empregos %>% to_spatial()

# Pegar a populacao do centroide de fortaleza
pop <- read_rds("../data/grade_municipio_com_renda/grade_renda_for.rds") %>%
  dplyr::select(id_grade, pop_total, renda) %>%
  st_centroid()

# Funcao para fazer agregacao pelo tamanho do hexagono
# res <- "09"

agregar_por_hex <- function(res) {
  
  # abrir hexagonos
  path_hex <- sprintf("../data/hex_municipio/hex_for_%s.rds", res)
  hex_muni <- readRDS(path_hex)
  
  # fazer agregacao
  hex_muni_fim <- hex_muni %>%
    # Agrupar populacao e renda
    st_join(pop) %>%
    group_by(id_hex) %>%
    summarise(pop_total = sum(pop_total, na.rm = TRUE), renda_total = sum(renda, na.rm = TRUE)) %>%
    ungroup() %>%
    # Agrupar empregos (agora somando a quantidade de vinculos!)
    st_join(empregos) %>%
    group_by(id_hex, pop_total, renda_total) %>%
    summarise(empregos_total = sum(qt_vinc_ativos2, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(empregos_total = ifelse(is.na(empregos_total), 0, empregos_total)) %>%
    # agrupar educacao
    st_join(escolas) %>%
    group_by(id_hex, pop_total, renda_total, empregos_total) %>%
    summarise(mat_total = sum(mat_total)) %>%
    ungroup() %>%
    mutate(mat_total = ifelse(is.na(mat_total), 0, mat_total)) %>%
    ungroup()
  
  # salvar
  path_out <- sprintf("../data/hex_agregados/hex_agregados_%s.rds", res)
  write_rds(hex_muni_fim, path_out)
  
}

# aplicar funcao
walk(c("08", "09"), agregar_por_hex)

####################################################################################################
# visualizar!!!!!!!!!! -----------------------------------------------------------------------------
hex_agregados <- read_rds("../data/hex_agregados/hex_agregados_08.rds")

hex_agregados_long <- hex_agregados %>%
  gather("variavel", "valor", pop_total:mat_total)

# tem que fazer os graficos separados...

theme_mapa <- function(base_size) {
  
  theme_void(base_family="Roboto Condensed") %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(1.5,"line"),
      legend.key.height = unit(0.1,"cm"),
      # legend.text=element_text(size=rel(0.5)),
      # legend.title=element_text(size=rel(0.5)),
      legend.text=element_text(size=unit(7, "cm")),
      legend.title=element_blank(),
      plot.title = element_text(hjust = 0.5, vjust = 2, size = 12)
      
      
    )
}

library(patchwork)

hex_agregados %>%
  select(id_hex, pop_total) %>%
  # mutate(pop_total = ifelse(pop_total > 2000, 2000, pop_total)) %>%
  ggplot()+
  geom_sf(data = hex_agregados, aes(fill = pop_total), color = NA)+
  viridis::scale_fill_viridis(option = "B", 
                              breaks = c(0, 11000, 22000), 
                              labels = c(0, 11, "22 mil"))+
  # scale_fill_gradientn(colours =  RColorBrewer::brewer.pal(9, "PuRd"), na.value = "black")+
  labs(title = "População")+
  theme_mapa() +
  theme(plot.title = element_text(vjust=-3))+

hex_agregados %>%
  select(id_hex, pop_total, renda_total) %>%
  mutate(renda_capta = renda_total/pop_total) %>%
  mutate(renda_capta = ifelse(renda_capta > 3000, 3000, renda_capta)) %>%
  ggplot()+
  geom_sf(aes(fill = renda_capta), color = NA)+
  viridis::scale_fill_viridis(option = "B", 
                              breaks = c(1, 1500, 3000), 
                              labels = c("0", "1,5", "+3 mil"))+
  labs(title = "Renda per capta")+
  theme_mapa() +
  theme(plot.title = element_text(vjust=-3))+

hex_agregados %>%
  select(id_hex, empregos_total) %>%
  mutate(empregos_total = ifelse(empregos_total > 5000, 5000, empregos_total)) %>%
  ggplot()+
  geom_sf(aes(fill = empregos_total), color = NA)+
  viridis::scale_fill_viridis(option = "B", 
                              breaks = c(0, 2500, 5000), 
                              labels = c("0", "2,5", "+5 mil"))+
  labs(title = "Empregos")+
  theme_mapa() +
  theme(plot.margin = unit(c(1, 0, 0, 0), "mm"),
        plot.title = element_text(vjust=-6))+

# hex_agregados %>%
#   select(id_hex, saude_total) %>%
#   mutate(saude_total = ifelse(saude_total > 3, 3, saude_total)) %>%
#   ggplot()+
#   geom_sf(aes(fill = saude_total), color = NA)+
#   labs(title = "Saúde")+
#   viridis::scale_fill_viridis(option = "B", breaks = c(0, 1, 3), labels = c("0", "1", "3+"))+
#   theme_mapa() +

hex_agregados %>%
  select(id_hex, mat_total) %>%
  mutate(mat_total = ifelse(mat_total > 3000, 3000, mat_total)) %>%
  ggplot()+
  geom_sf(aes(fill = mat_total), color = NA)+
  viridis::scale_fill_viridis(option = "B", 
                              breaks = c(0, 1500, 3000), 
                              labels = c("0", "1,5","+3 mil"))+
  labs(title = "Matrículas")+
  theme_mapa() +
  theme(plot.margin = unit(c(1, 0, 0, 0), "mm"),
        plot.title = element_text(vjust=-6))+
  plot_layout(ncol = 2)

ggsave("figure/4-distribuicao_us.png", dpi = 300, width = 16, height = 16, units = "cm")

      hex_agregados %>%
  ggplot()+
  geom_sf(size = 0.1)+
  theme_mapa()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "mm"))
  
ggsave("figure/4-for_h3.png", dpi = 300, width = 8, height = 6, units = "cm")
  