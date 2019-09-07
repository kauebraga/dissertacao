# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCAO PARA JUNTAR O TTMATRIX EM UM SO! ----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

juntar_tt <- function(cidade) {
  
  cidade_files <- dir("../data/ttmatrix_fim", 
                      pattern = sprintf("^ttmatrix_%s", cidade), 
                      full.names = TRUE)
  
  # Escolher de 6:45 a 7:45
  cidade_files <- cidade_files[c(4:8)]
  
  # Abrir arquivos e juntar
  ttmatrix_allmodes <- map(cidade_files,
                           fread, select = c("city", "mode", "depart_time", "origin", "destination", "travel_time")) %>%
    rbindlist()
  
  # Se a origem e o destino forem o mesmo, adotar o tempo de viagem (para qualquer modo) como 350s
  ttmatrix_allmodes[, travel_time := ifelse(origin == destination, 
                                            700,
                                            travel_time)]
  
  # Calcular a mediana do tempo de viagem entre cada par OD para pico e fora pico ------------------
  
  ttmatrix_allmodes[, par_id := paste0(origin, "-", destination)]
  
  # Calcular a mediana agrupando por cidade, modo, origin, destination, pico
  ttmatrix_median <- ttmatrix_allmodes[, .(origin = first(origin),
                                           destination = first(destination),
                                           tt_median = median(travel_time, na.rm = TRUE),
                                           sd = sd(travel_time, na.rm = TRUE)),
                                       by = .(city, par_id)]
  
  # salvar
  path_out <- sprintf("../data/ttmatrix_fim/ttmatrix_%s.csv", cidade)
  fwrite(ttmatrix_median, path_out)
  
}


# Aplicar
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

juntar_tt("forpadrao")
juntar_tt("forcorrigidocm")
juntar_tt("forcorrigidoce")


# Analisar
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

teste <- fread("../data/ttmatrix_fim/ttmatrix_forcorrigidocm.csv")

teste1 <- fread("../data/ttmatrix_fim/ttmatrix_forpadrao.csv")


vai <- teste %>% select(origin, destination, par_id, sd, tt_median) %>%
  full_join(teste1 %>% select(par_id, sd, tt_median), by = c("par_id"),
            suffix = c(".corrigido", ".padrao")) %>%
  mutate(tt_median.corrigido = tt_median.corrigido/60) %>%
  mutate(tt_median.padrao = tt_median.padrao/60)


# Plot
ggplot(data = vai)+
  geom_hex(aes(x = tt_median.corrigido , y = tt_median.padrao))+
  geom_abline(intercept = 0, slope = 1, color = "red")+
  scale_fill_viridis_c(option = "B")+
  theme_ipsum_rc()+
  labs(x = "Tempo de viagem corrigido (min)", y = "Tempo de viagem programado (min)")+
  theme(legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "mm"))

ggsave("figure/5-resultado_otp.png", dpi = 300, width = 16, height = 10, units = "cm")




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULAR ACESS SEPARADO PARA CADA TEMPO DE VIAGEM! -----------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# cidade <- "forcorrigidocm"
# hora <- "7-15"

calcular_acess <- function(cidade, hora) {
  
  cidade_files <- dir("../data/ttmatrix_fim", 
                      pattern = sprintf("^ttmatrix_%s_%s", cidade, hora), 
                      full.names = TRUE)
  
  # Abrir arquivo
  ttmatrix_allmodes <- fread(cidade_files)
  
  
  # # Abrir arquivos e juntar
  # ttmatrix_allmodes <- map(cidade_files, 
  #                          fread, select = c("city", "mode", "depart_time", "origin", "destination", "travel_time")) %>%
  #   rbindlist()
  
  # Se a origem e o destino forem o mesmo, adotar o tempo de viagem (para qualquer modo) como 350s
  ttmatrix_allmodes[, travel_time := ifelse(origin == destination, 
                                            700,
                                            travel_time)]
  
  # Calcular a mediana do tempo de viagem entre cada par OD para pico e fora pico ------------------
  
  ttmatrix_allmodes[, par_id := .GRP, by = .(origin, destination)]
  
  
  # Pegar arquivo com os hexagonos com as atividades
  dir_hex <- sprintf("../data/hex_agregados/hex_agregados_09.rds")
  
  # abrir oportunidades com hexagonos
  hexagonos_sf <- read_rds(dir_hex) %>%
    ungroup()
  
  # so populacao e renda
  hexagonos_pop <- hexagonos_sf %>%
    st_set_geometry(NULL) %>%
    select(id_hex, pop_total, renda_total) %>%
    setDT()
  
  # outras variaveis
  hexagonos_vars <- hexagonos_sf %>%
    st_set_geometry(NULL) %>%
    select(-pop_total, -renda_total) %>%
    setDT()
  
  # Juntar as variaveis de uso do solo com os tempos de viagem
  # Trazer a populacao e renda (juncao pela ORIGEM!)
  ttmatrix_variaveis <- ttmatrix_allmodes[hexagonos_pop, on = c("origin" = "id_hex"),  
                                        c('pop_total', 'renda_total') := list(i.pop_total, i.renda_total)]
  
  
  # Trazer as demais variaveis (juncao pelo DESTINO!)
  ttmatrix_variaveis <- ttmatrix_allmodes[hexagonos_vars, on = c("destination" = "id_hex"),  
                                        c('empregos_total', 'mat_total') := 
                                          list(i.empregos_total, i.mat_total)]
  
  # Transformar o traveltime para minutos
  ttmatrix_variaveis[, travel_time := travel_time/60]
  
  setorder(ttmatrix_variaveis, par_id)
  
  # Dicionario de variaveis:
  # - CMA = Acessibilidade Cumulativa Ativa
  # - CMP = Acessibilidade Cumulativa Passiva
  # - CPT = 
  # - TMI = Acessibilidade de Tempo Mínimo à Oportunidade
  
  # 1 - All accessible activities from each ORIGIN across they day
  acess <- ttmatrix_variaveis[, 
                               .(CMA_ET_15 = sum( mat_total[which( travel_time <= 15)], na.rm=T)
                                 , CMA_ET_30 = sum( mat_total[which( travel_time <= 30)], na.rm=T)
                                 , CMA_ET_45 = sum( mat_total[which( travel_time <= 45)], na.rm=T)
                                 , CMA_ET_50 = sum( mat_total[which( travel_time <= 50)], na.rm=T)
                                 , CMA_ET_60 = sum( mat_total[which( travel_time <= 60)], na.rm=T)
                                 
                                 
                                 , CMA_TT_15 = sum( empregos_total[which( travel_time <= 15)], na.rm=T)
                                 , CMA_TT_30 = sum( empregos_total[which( travel_time <= 30)], na.rm=T)
                                 , CMA_TT_45 = sum( empregos_total[which( travel_time <= 45)], na.rm=T)
                                 , CMA_TT_60 = sum( empregos_total[which( travel_time <= 60)], na.rm=T)
                                 , CMA_TT_65 = sum( empregos_total[which( travel_time <= 65)], na.rm=T)
                               ),
                               by=.(city, origin) ]
  
  
  acess_sf <- merge(acess, setDT(hexagonos_sf)[, .(id_hex, geometry)],
                     by.x = "origin",
                     by.y = "id_hex",
                     all.x = TRUE) %>%
    # Transformar para sf
    st_sf()
  
  # Salvar
  path_out <- sprintf("../data/acess/acess_%s_%s.rds", cidade, hora)
  write_rds(acess_sf, path_out)
  
  
}

calcular_acess("forpadrao", "6-30")
calcular_acess("forpadrao", "6-45")
calcular_acess("forpadrao", "7-0")
calcular_acess("forpadrao", "7-15")
calcular_acess("forpadrao", "7-30")
calcular_acess("forpadrao", "7-45")

calcular_acess("forcorrigidocm", "6-30")
calcular_acess("forcorrigidocm", "6-45")
calcular_acess("forcorrigidocm", "7-0")
calcular_acess("forcorrigidocm", "7-15")
calcular_acess("forcorrigidocm", "7-30")
calcular_acess("forcorrigidocm", "7-45")
calcular_acess("forcorrigidoce", "6-30")
calcular_acess("forcorrigidoce", "6-45")
calcular_acess("forcorrigidoce", "7-0")
calcular_acess("forcorrigidoce", "7-15")
calcular_acess("forcorrigidoce", "7-30")
calcular_acess("forcorrigidoce", "7-45")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULAR ACESS FAZENDO A MEDIANA DOS TEMPOS DE VIAGEM --------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# cidade <- "forcorrigidocm"
# cidade <- "forpadrao"

calcular_acess_med <- function(cidade) {
  
  cidade_files <- sprintf("../data/ttmatrix_fim/ttmatrix_%s.csv", cidade)
  
  ttmatrix_median <- fread(cidade_files)
  
  # Pegar arquivo com os hexagonos com as atividades
  dir_hex <- sprintf("../data/hex_agregados/hex_agregados_09.rds")
  
  # abrir oportunidades com hexagonos
  hexagonos_sf <- read_rds(dir_hex) %>%
    ungroup()
  
  # so populacao e renda
  hexagonos_pop <- hexagonos_sf %>%
    st_set_geometry(NULL) %>%
    select(id_hex, pop_total, renda_total) %>%
    setDT()
  
  # outras variaveis
  hexagonos_vars <- hexagonos_sf %>%
    st_set_geometry(NULL) %>%
    select(-pop_total, -renda_total) %>%
    setDT()
  
  # Juntar as variaveis de uso do solo com os tempos de viagem
  # Trazer a populacao e renda (juncao pela ORIGEM!)
  ttmatrix_median <- ttmatrix_median[hexagonos_pop, on = c("origin" = "id_hex"),  
                                          c('pop_total', 'renda_total') := list(i.pop_total, i.renda_total)]
  
  
  # Trazer as demais variaveis (juncao pelo DESTINO!)
  ttmatrix_median <- ttmatrix_median[hexagonos_vars, on = c("destination" = "id_hex"),  
                                          c('empregos_total', 'mat_total') := 
                                            list(i.empregos_total, i.mat_total)]
  # Transformar o traveltime para minutos
  ttmatrix_median[, tt_median := tt_median/60]
  
  setorder(ttmatrix_median, par_id)
  
  acess <- ttmatrix_median[, 
                              .(CMA_ET_15 = sum( mat_total[which( tt_median <= 15)], na.rm=T)
                                , CMA_ET_30 = sum( mat_total[which( tt_median <= 30)], na.rm=T)
                                , CMA_ET_45 = sum( mat_total[which( tt_median <= 45)], na.rm=T)
                                , CMA_ET_50 = sum( mat_total[which( tt_median <= 50)], na.rm=T)
                                , CMA_ET_60 = sum( mat_total[which( tt_median <= 60)], na.rm=T)
                                
                                , CMA_TT_15 = sum( empregos_total[which( tt_median <= 15)], na.rm=T)
                                , CMA_TT_30 = sum( empregos_total[which( tt_median <= 30)], na.rm=T)
                                , CMA_TT_45 = sum( empregos_total[which( tt_median <= 45)], na.rm=T)
                                , CMA_TT_60 = sum( empregos_total[which( tt_median <= 60)], na.rm=T)
                                , CMA_TT_65 = sum( empregos_total[which( tt_median <= 65)], na.rm=T)
                              ),
                              by=.(city, origin) ]
  
  acess_sf <- merge(acess, setDT(hexagonos_sf)[, .(id_hex, geometry)],
                    by.x = "origin",
                    by.y = "id_hex",
                    all.x = TRUE) %>%
    # Transformar para sf
    st_sf()
  
  # Salvar
  path_out <- sprintf("../data/acess/acess_%s.rds", cidade)
  write_rds(acess_sf, path_out)
  
  
}

calcular_acess_med("forpadrao")
calcular_acess_med("forcorrigidocm")
calcular_acess_med("forcorrigidoce")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ANÁLISE ------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


forpadrao <- read_rds("../data/acess/acess_forpadrao.rds")
forcorrigido <- read_rds("../data/acess/acess_forcorrigidocm.rds")

acess_dif <- forcorrigido %>%
  left_join(forpadrao %>% st_set_geometry(NULL) %>% select(origin, CMA_ET_15:CMA_TT_65), 
            by = c("origin"), 
            suffix = c(".corrigido", ".padrao"))

# PLOT

total_empregos <- 550000
total_matriculas <- 281000

# abrir limits
limits <- read_rds("../data/muni/municipios_ce.rds") %>%
  filter(NM_MUNICIP == "FORTALEZA")

theme_mapa <- function(base_size) {
  
  theme_void(base_family="Roboto Condensed") %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(1.0,"line"),
      legend.key.height = unit(0.1,"cm"),
      # legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.7)),
      legend.text=element_text(size=unit(7, "cm")),
      # legend.title=element_blank(),
      plot.title = element_text(hjust = 0.5, vjust = 2, size = 12)
      
      
    )
}

# Comparar trabalho 65 minutos para TP
for_comparacao_tt <- acess_dif %>%
  select(origin, starts_with("CMA_TT_65")) %>%
  mutate(dif = CMA_TT_65.corrigido - CMA_TT_65.padrao) %>%
  mutate(dif_perc = dif/CMA_TT_65.padrao) %>%
  mutate(dif_log = log(CMA_TT_65.corrigido/CMA_TT_65.padrao)) %>%
  mutate(dif_log_tc = ifelse(dif_log > 0.5, 0.5, 
                          ifelse(dif_log < -0.5, -0.5, dif_log))) %>%
  mutate(dif_perc_total = dif/total_empregos)

# hist(for_comparacao_tt$df_perc)
boxplot(for_comparacao_tt$dif_log)

# comparacao nao espacial
for_comparacao_tt %>%
  ggplot()+
  geom_point(aes(x = CMA_TT_65.corrigido, y = CMA_TT_65.padrao), alpha = 0.2)+
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  labs(x = "Acessibilidade corrigida", y = "Acessibilidade programada")+
  theme_ipsum_rc()+
  theme(legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "mm"))

ggsave("figure/5-resultado_acess_tt_corrxprogr_sp.png", dpi = 300, width = 16, height = 7, units = "cm")


library(patchwork)
boxplot.stats(for_comparacao_tt$dif_log)$stats[2:4]
# comparacao espacial
for_comparacao_tt %>%
  ggplot()+
  geom_sf(aes(fill = dif_log_tc), color = NA)+
  geom_sf(data= limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*max(abs(for_comparacao_tt$dif_log_tc)))+
  labs(fill = "Diferença relativa de oportunidades\n acessíveis")+
  theme_mapa()+

for_comparacao_tt %>%
  # mutate(corta = cut(dif_log_tc, 
  #                    breaks = seq(-0.5, 0.5, by = 0.1), 
  #                    labels = c("<0.5", "-0.4", "-0.3", "-0.2", "-0.1", "0", "0.1", "0.2", "0.3", "0.4", ">0.5"))) %>%
  ggplot()+
  # geom_histogram(aes(dif_log_tc), binwidth = 0.1)+
  geom_jitter(aes(x = 1, y = dif_log, color = dif_log), alpha = 1)+
  geom_boxplot(aes(x = 1, y = dif_log), alpha = 0.1)+
  scale_color_distiller(palette = "RdBu", direction = 1,
                        limits = c(-1,1)*max(abs(for_comparacao_tt$dif_log)))+
  # scale_fill_distiller(palette = "RdBu", direction = 1)+
  #                      # limits = c(-1,1)*max(abs(for_comparacao_tt$dif_log_tc)),
  #                      breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
  #                      labels = c("<-0.5", "-0.25", "0", "0.25", ">0.5")
  #                      )+
  # scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5), labels = c("<-0.5", "-0.25", "0", "0.25", ">0.5"))+
  theme_ipsum_rc()+
  theme(plot.margin = unit(c(1, 1, 1, 1), "mm"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
  plot_layout(widths = c(2, 1))

ggsave("figure/5-comparacao_gtfs_tt_65.png", dpi = 300, units = "cm", width = 16, height = 10)

mapview(for_comparacao_tt, zcol = "dif_log_tc")

# Comparar educacao 50 minutos para ET -------------------------------------------------------------
for_comparacao_et <- acess_dif %>%
  select(origin, starts_with("CMA_ET_50")) %>%
  mutate(dif = CMA_ET_50.corrigido - CMA_ET_50.padrao) %>%
  mutate(dif_log = log(CMA_ET_50.corrigido/CMA_ET_50.padrao)) %>%
  mutate(dif_log_tc = ifelse(dif_log > 0.5, 0.5, 
                             ifelse(dif_log < -0.5, -0.5, dif_log))) %>%
  mutate(dif_perc_total = dif/total_matriculas)

# comparacao nao espacial
for_comparacao_et %>%
  ggplot()+
  geom_point(aes(x = CMA_ET_50.corrigido, y = CMA_ET_50.padrao), alpha = 0.2)+
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  labs(x = "Acessibilidade corrigida", y = "Acessibilidade programada")+
  theme_ipsum_rc()+
  theme(legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "mm"))

ggsave("figure/5-resultado_acess_et_corrxprogr_sp.png", dpi = 300, width = 16, height = 7, units = "cm")

boxplot.stats(for_comparacao_et$dif_log)$stats[2:4]

for_comparacao_et %>%
  ggplot()+
  geom_sf(aes(fill = dif_log_tc), color = NA)+
  geom_sf(data= limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*max(abs(for_comparacao_tt$dif_log_tc)))+
  labs(fill = "Diferença relativa de oportunidades\n acessíveis")+
  theme_mapa()+

  for_comparacao_et %>%
  # mutate(corta = cut(dif_log_tc, 
  #                    breaks = seq(-0.5, 0.5, by = 0.1), 
  #                    labels = c("<0.5", "-0.4", "-0.3", "-0.2", "-0.1", "0", "0.1", "0.2", "0.3", "0.4", ">0.5"))) %>%
  ggplot()+
  # geom_histogram(aes(dif_log_tc), binwidth = 0.1)+
  geom_jitter(aes(x = 1, y = dif_log, color = dif_log), alpha = 1)+
  geom_boxplot(aes(x = 1, y = dif_log), alpha = 0.1)+
  scale_color_distiller(palette = "RdBu", direction = 1,
                        limits = c(-1,1)*max(abs(for_comparacao_et$dif_log)))+
  # scale_fill_distiller(palette = "RdBu", direction = 1)+
  #                      # limits = c(-1,1)*max(abs(for_comparacao_tt$dif_log_tc)),
  #                      breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
  #                      labels = c("<-0.5", "-0.25", "0", "0.25", ">0.5")
  #                      )+
  # scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5), labels = c("<-0.5", "-0.25", "0", "0.25", ">0.5"))+
  theme_ipsum_rc()+
  theme(plot.margin = unit(c(1, 1, 1, 1), "mm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  plot_layout(widths = c(2, 1))

ggsave("figure/5-comparacao_gtfs_et_50.png", dpi = 300, units = "cm", width = 16, height = 10)

mapview(for_comparacao_et, zcol = "dif_log_tc")


# comparar as duas distribuicoes absolutas!
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mean(for_comparacao_tt$dif_perc_total)

boxplot.stats(for_comparacao_tt$dif_perc_total)$stats[2:4]
boxplot.stats(for_comparacao_et$dif)$stats[2:4]

for_comparacao_tt %>%
  ggplot()+
  geom_sf(aes(fill = dif), color = NA)+
  geom_sf(data= limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       breaks = c(-100000, 0, 100000),
                       limits = c(-1,1)*max(abs(for_comparacao_tt$dif)))+
  labs(fill = "Diferença absoluta de oportunidades\n acessíveis para trabalho")+
  theme_mapa()+
  theme(plot.margin = unit(c(1, 3, 1, 1), "mm"))+

for_comparacao_et %>%
  # mutate(dif_tc = ifelse(dif < -30000, 30000, ifelse(dif > 50000, 50000, dif))) %>%
  ggplot()+
  geom_sf(aes(fill = dif), color = NA)+
  geom_sf(data= limits, fill = NA)+
  # scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, )+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*max(abs(for_comparacao_et$dif)))+
  labs(fill = "Diferença absoluta de oportunidades\n acessíveis para educação")+
  theme_mapa() +
  theme(plot.margin = unit(c(1, 1, 3, 1), "mm"))+
  
  for_comparacao_tt %>%
  # mutate(corta = cut(dif_log_tc, 
  #                    breaks = seq(-0.5, 0.5, by = 0.1), 
  #                    labels = c("<0.5", "-0.4", "-0.3", "-0.2", "-0.1", "0", "0.1", "0.2", "0.3", "0.4", ">0.5"))) %>%
  ggplot()+
  # geom_histogram(aes(dif_log_tc), binwidth = 0.1)+
  geom_jitter(aes(x = 1, y = dif_perc_total,  color = dif_perc_total), alpha = 1)+
  geom_boxplot(aes(x = 1, y = dif_perc_total), alpha = 0.1)+
  geom_abline(slope = 0, intercept = 0, color = "red")+
  scale_color_distiller(palette = "RdBu", direction = 1,
                       limits = c(-1,1)*max(abs(for_comparacao_tt$dif_perc_total)))+
  scale_y_percent()+
  theme_ipsum_rc(grid = "X")+
  theme(plot.margin = unit(c(1, 3, 1, 1), "mm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill = NA, size = 1))+
  coord_flip(ylim = c(-0.3, 0.3))+
  
  for_comparacao_et %>%
  # mutate(corta = cut(dif_log_tc, 
  #                    breaks = seq(-0.5, 0.5, by = 0.1), 
  #                    labels = c("<0.5", "-0.4", "-0.3", "-0.2", "-0.1", "0", "0.1", "0.2", "0.3", "0.4", ">0.5"))) %>%
  ggplot()+
  # geom_histogram(aes(dif_log_tc), binwidth = 0.1)+
  geom_jitter(aes(x = 1, y = dif_perc_total,  color = dif_perc_total), alpha = 1)+
  geom_boxplot(aes(x = 1, y = dif_perc_total), alpha = 0.1)+
  geom_abline(slope = 0, intercept = 0, color = "red")+
  scale_color_distiller(palette = "RdBu", direction = 1,
                        limits = c(-1,1)*max(abs(for_comparacao_et$dif_perc_total)))+
  geom_abline(slope = 0, intercept = 0, color = "red")+
  scale_y_percent()+
  theme_ipsum_rc(grid = "X")+
  theme(plot.margin = unit(c(1, 1, 3, 1), "mm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  coord_flip(ylim = c(-0.3, 0.3))+
  plot_layout(ncol = 2, heights = c(3, 1))

ggsave("figure/5-comparacao_gtfs_absoluto.png", dpi = 300, units = "cm", width = 16, height = 12)

mapview(for_comparacao_tt, zcol = "dif")
  
  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT PARA CHECAR A VARIACAO NOS TEMPOS DE VIAGEM
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

forcorrigidocm_630 <- read_rds("../data/acess/acess_forcorrigidocm_6-30.rds")
forcorrigidocm_645 <- read_rds("../data/acess/acess_forcorrigidocm_6-45.rds")
forcorrigidocm_700 <- read_rds("../data/acess/acess_forcorrigidocm_7-0.rds")
forcorrigidocm_715 <- read_rds("../data/acess/acess_forcorrigidocm_7-15.rds")
forcorrigidocm_730 <- read_rds("../data/acess/acess_forcorrigidocm_7-30.rds")
forcorrigidocm_745 <- read_rds("../data/acess/acess_forcorrigidocm_7-45.rds")

# PARA TRABALHO 65
for630tt <-  forcorrigidocm_630 %>%
  select(origin, starts_with("CMA_TT_65")) %>%
  mutate(tipo = "tt630")

for645tt <- forcorrigidocm_645 %>%
  select(origin, starts_with("CMA_TT_65")) %>%
  mutate(tipo = "tt645")

for700tt <- forcorrigidocm_700 %>%
  select(origin, starts_with("CMA_TT_65")) %>%
  mutate(tipo = "tt700")

for715tt <- forcorrigidocm_715 %>%
  select(origin, starts_with("CMA_TT_65")) %>%
  mutate(tipo = "tt715")

for730tt <- forcorrigidocm_730 %>%
  select(origin, starts_with("CMA_TT_65")) %>%
  mutate(tipo = "tt730")

for745tt <- forcorrigidocm_745 %>%
  select(origin, starts_with("CMA_TT_65")) %>%
  mutate(tipo = "tt745")


forbind_tt <- rbind(for645tt, for700tt, for715tt, for730tt, for745tt)


ggplot(forbind_tt)+
  geom_sf(aes(fill = CMA_TT_65))+
  facet_wrap(~tipo, ncol = 2)

# e desvio padrao?
forbind_group_tt <- forbind_tt %>%
  group_by(origin) %>%
  summarise(mean = mean(CMA_TT_65, na.rm = TRUE),
         sd = sd(CMA_TT_65, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cv = sd/mean) %>%
  mutate(cv_tc = ifelse(cv > 0.7, 0.7, cv))

boxplot(forbind_group_tt$cv)
boxplot.stats(forbind_group_tt$cv)

forbind_group_tt %>%
  ggplot()+
  geom_sf(data= limits, fill = NA)+
  geom_sf(aes(fill = cv_tc), color = NA)+
  scale_fill_viridis_c(option = "B")+
  labs(fill = "Coeficiente de Variação\n da Acessibilidade")+
  theme_mapa() +
  
forbind_group_tt %>%
  ggplot()+
  # geom_histogram(aes(dif_log_tc), binwidth = 0.1)+
  geom_jitter(aes(x = 1, y = cv), alpha = 0.1)+
  geom_boxplot(aes(x = 1, y = cv), alpha = 0.5)+
  # scale_fill_distiller(palette = "RdBu", direction = 1)+
  #                      # limits = c(-1,1)*max(abs(for_comparacao_tt$dif_log_tc)),
  #                      breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
  #                      labels = c("<-0.5", "-0.25", "0", "0.25", ">0.5")
  #                      )+
  # scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5), labels = c("<-0.5", "-0.25", "0", "0.25", ">0.5"))+
  theme_ipsum_rc()+
  theme(plot.margin = unit(c(1, 1, 1, 1), "mm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank()) +
  plot_layout(widths = c(2, 1))

ggsave("figure/5-cv_acess_tt_horas.png", dpi = 300, units = "cm", width = 16, height = 10)

# PARA EDUCACAO!

# PARA EDUCACAO 50
for630et <- forcorrigidocm_630 %>% select(origin, starts_with("CMA_ET_50")) %>% mutate(tipo = "et630")
for645et <- forcorrigidocm_645 %>% select(origin, starts_with("CMA_ET_50")) %>% mutate(tipo = "et645")
for700et <- forcorrigidocm_700 %>% select(origin, starts_with("CMA_ET_50")) %>% mutate(tipo = "et700")
for715et <- forcorrigidocm_715 %>% select(origin, starts_with("CMA_ET_50")) %>% mutate(tipo = "et715")
for730et <- forcorrigidocm_730 %>% select(origin, starts_with("CMA_ET_50")) %>% mutate(tipo = "et730")
for745et <- forcorrigidocm_745 %>% select(origin, starts_with("CMA_ET_50")) %>% mutate(tipo = "et745")

forbind_et <- rbind(for645et, for700et, for715et, for730et, for745et)


# e desvio padrao?
forbind_group_et <- forbind_et %>%
  group_by(origin) %>%
  summarise(mean = mean(CMA_ET_50, na.rm = TRUE),
         sd = sd(CMA_ET_50, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cv = sd/mean) %>%
  mutate(cv_tc = ifelse(cv > 0.7, 0.7, cv))

boxplot(forbind_group_et$sd)
boxplot.stats(forbind_group_et$cv)

forbind_group_et %>%
  ggplot()+
  geom_sf(data= limits, fill = NA)+
  geom_sf(aes(fill = cv_tc), color = NA)+
  scale_fill_viridis_c(option = "B")+
  labs(fill = "Coeficiente de Variação\n da Acessibilidade")+
  theme_mapa() +
  
forbind_group_et %>%
  ggplot()+
  # geom_histogram(aes(dif_log_tc), binwidth = 0.1)+
  geom_jitter(aes(x = 1, y = cv), alpha = 0.1)+
  geom_boxplot(aes(x = 1, y = cv), alpha = 0.5)+
  # scale_fill_distiller(palette = "RdBu", direction = 1)+
  #                      # limits = c(-1,1)*max(abs(for_comparacao_tt$dif_log_tc)),
  #                      breaks = c(-0.5, -0.25, 0, 0.25, 0.5),
  #                      labels = c("<-0.5", "-0.25", "0", "0.25", ">0.5")
  #                      )+
  # scale_x_continuous(breaks = c(-0.5, -0.25, 0, 0.25, 0.5), labels = c("<-0.5", "-0.25", "0", "0.25", ">0.5"))+
  theme_ipsum_rc()+
  theme(plot.margin = unit(c(1, 1, 1, 1), "mm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank()) +
  plot_layout(widths = c(2, 1))

ggsave("figure/5-cv_acess_et_horas.png", dpi = 300, units = "cm", width = 16, height = 10)

#  FALTA EDUCACAO!


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PLOT PARA DIFERENTES CENARIOS (H2) --------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(patchwork)

forp50 <- read_rds("../data/acess/acess_forcorrigidocm.rds") %>% mutate(tipo = "P50")
forp85 <- read_rds("../data/acess/acess_forcorrigidoce.rds") %>% mutate(tipo = "P85")

juntos_cenario <- rbind(forp50, forp85)

juntos_cenario_tt <- juntos_cenario %>%
  select(origin, tipo, CMA_TT_65) %>%
  spread(tipo, CMA_TT_65) %>%
  mutate(dif = P50 - P85) %>%
  mutate(dif_perc_total = dif/total_empregos)

juntos_cenario_et <- juntos_cenario %>%
  select(origin, tipo, CMA_ET_50) %>%
  spread(tipo, CMA_ET_50) %>%
  mutate(dif = P50 - P85) %>%
  mutate(dif_perc_total = dif/total_matriculas)

  
  # PARA TRABALHO:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

juntos_cenario %>%
  ggplot()+
  geom_sf(data= limits, fill = NA)+
  geom_sf(aes(fill = CMA_TT_65), color = NA) +
  facet_wrap(~tipo)+
  scale_fill_viridis_c(option = "B", 
                       breaks = c(4, 250000, 500000), 
                       labels = c("0", "250000", "500000"))+
  labs(fill = "Quantidade de empregos\n acessíveis em 65 minutos")+
  theme_mapa() +

juntos_cenario %>%
  ggplot()+
  geom_sf(data= limits, fill = NA)+
  geom_sf(aes(fill = CMA_ET_50), color = NA) +
  facet_wrap(~tipo)+
  scale_fill_viridis_c(option = "B", 
                       breaks = c(4, 75000, 150000),
                       labels = c("0", "75000", "150000+")
                       )+
  labs(fill = "Quantidade de matrículas\n acessíveis em 50 minutos")+
  theme_mapa() +
  plot_layout(ncol = 1)

ggsave("figure/5-acess_cenarios.png", dpi = 300, units = "cm", width = 16, height = 20)


# diferenca absoluta entre cenarios

mean(juntos_cenario_tt$dif_perc_total)
mean(juntos_cenario_et$dif_perc_total)

juntos_cenario_tt %>%
  ggplot()+
  geom_sf(data = limits, fill = NA)+
  geom_sf(aes(fill = dif), color = NA)+
  # scale_fill_distiller(palette = "Reds", direction = 1)+
  scale_fill_viridis_c(option = "B", direction = -1)+
  labs(fill = "Diferença absoluta de\n acessibilidade entre cenários\n(empregos)")+
  theme_mapa()+

juntos_cenario_et %>%
  ggplot()+
  geom_sf(data = limits, fill = NA)+
  geom_sf(aes(fill = dif), color = NA)+
  # scale_fill_distiller(palette = "Reds", direction = 1)+
  scale_fill_viridis_c(option = "B", direction = -1)+
  labs(fill = "Diferença absoluta de\n acessibilidade entre cenários\n(educação)")+
  theme_mapa() +
  theme(plot.margin = unit(c(1, 1, 3, 1), "mm"))+
  
  juntos_cenario_tt %>%
  # mutate(corta = cut(dif_log_tc, 
  #                    breaks = seq(-0.5, 0.5, by = 0.1), 
  #                    labels = c("<0.5", "-0.4", "-0.3", "-0.2", "-0.1", "0", "0.1", "0.2", "0.3", "0.4", ">0.5"))) %>%
  ggplot()+
  # geom_histogram(aes(dif_log_tc), binwidth = 0.1)+
  geom_jitter(aes(x = 1, y = dif_perc_total,  color = dif_perc_total), alpha = 0.8)+
  geom_boxplot(aes(x = 1, y = dif_perc_total), alpha = 0.1)+
  # scale_color_distiller(palette = "Reds", direction = 1)+
  scale_color_viridis_c(option = "B", direction = -1)+
  scale_y_percent()+
  theme_ipsum_rc(grid = "X")+
  theme(plot.margin = unit(c(1, 3, 1, 1), "mm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill = NA, size = 1))+
  coord_flip(ylim = c(0, 0.5))+
  
  juntos_cenario_et %>%
  # mutate(corta = cut(dif_log_tc, 
  #                    breaks = seq(-0.5, 0.5, by = 0.1), 
  #                    labels = c("<0.5", "-0.4", "-0.3", "-0.2", "-0.1", "0", "0.1", "0.2", "0.3", "0.4", ">0.5"))) %>%
  ggplot()+
  # geom_histogram(aes(dif_log_tc), binwidth = 0.1)+
  geom_jitter(aes(x = 1, y = dif_perc_total,  color = dif_perc_total), alpha = 0.8)+
  geom_boxplot(aes(x = 1, y = dif_perc_total), alpha = 0.1)+
  # scale_color_distiller(palette = "Reds", direction = 1)+
  scale_color_viridis_c(option = "B", direction = -1)+
  scale_y_percent()+
  theme_ipsum_rc(grid = "X")+
  theme(plot.margin = unit(c(1, 1, 3, 1), "mm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  coord_flip(ylim = c(0, 0.5))+
  plot_layout(ncol = 2, heights = c(3, 1))
  
  
  # juntos_cenario %>%
#   select(origin, tipo, CMA_TT_65) %>%
#   spread(tipo, CMA_TT_65) %>%
#   mutate(dif = P50 - P85) %>%
#   mutate(dif_perc = dif/P50) %>%
#   mutate(dif_perc = ifelse(dif_perc < 0, 0, dif_perc)) %>%
#   ggplot()+
#   geom_sf(data = limits, fill = NA)+
#   geom_sf(aes(fill = dif_perc), color = NA)+
#   scale_fill_viridis_c(option = "B" 
#                        ,direction = -1
#                        , breaks = c(0, 0.25, 0.5)
#                        , labels = c("0", "0.25", "0.5")
#                        )+
#   labs(fill = "Diferença percentual de\n acessibilidade entre cenários")+
#   theme_mapa()

ggsave("figure/5-acess_dif_cenarios.png", dpi = 300, units = "cm", width = 16, height = 10)



# # PARA EDUCACAO:
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# 
# juntos_cenario %>%
#   ggplot()+
#   geom_sf(data= limits, fill = NA)+
#   geom_sf(aes(fill = CMA_ET_50), color = NA) +
#   facet_wrap(~tipo)+
#   scale_fill_viridis_c(option = "B"
#                        # , breaks = c(4, 250000, 500000) 
#                        # , labels = c("0", "250000", "500000")
#                        )+
#   labs(fill = "Quantidade de matrículas\n acessíveis em 50 minutos")+
#   theme_mapa()
# 
# ggsave("figure/5-acess_et_cenarios.png", dpi = 300, units = "cm", width = 16, height = 10)
# 
# juntos_cenario %>%
#   select(origin, tipo, CMA_ET_50) %>%
#   spread(tipo, CMA_ET_50) %>%
#   mutate(dif = P50 - P85) %>%
#   ggplot()+
#   geom_sf(data = limits, fill = NA)+
#   geom_sf(aes(fill = dif), color = NA)+
#   scale_fill_viridis_c(option = "B" 
#                        , direction = -1
#                        # breaks = c(4, 250000, 500000)
#                        # labels = c("0", "250000", "500000")
#                        )+
#   labs(fill = "Diferença absoluta de\n acessibilidade entre cenários")+
#   theme_mapa()
# 
# ggsave("figure/5-acess_et_dif_cenarios.png", dpi = 300, units = "cm", width = 16, height = 10)













# FAZER TESTE  PARA ZONA EXTREMO DE ACESSIBILIDADE -------------------------------------------------
# teste

hex_prob <- "8980104e203ffff"

tt_padrao <- fread("../data/ttmatrix_fim/ttmatrix_forpadrao.csv") %>% filter(origin == hex_prob)

tt_corrigido <- fread("../data/ttmatrix_fim/ttmatrix_forcorrigidocm.csv")  %>% filter(origin == hex_prob)

# problematico
tt_prob <- full_join(tt_padrao, tt_corrigido %>% select(par_id, tt_median, sd), 
                     by = "par_id",
                     suffix = c(".padrao", ".corrigido"))


hex_teste <- tt_prob %>% mutate(tt_median.corrigido = tt_median.corrigido/60,
                                tt_median.padrao = tt_median.padrao/60)

acess_corri <- hex_teste %>%
  filter(tt_median.corrigido <= 65)

acess_prog <- hex_teste %>%
  filter(tt_median.padrao <= 65)

# hexagonos
hex <- read_rds("../data/hex_municipio/hex_for_09.rds")

# hex %>% filter(id_hex == doido1) %>% mapview() + mapview(hex %>% filter(id_hex == doido2))

acess_prog_sf <- hex %>% right_join(acess_prog, by = c("id_hex" = 'destination'))
acess_corri_sf <- hex %>% right_join(acess_corri, by = c("id_hex" = 'destination'))

library(leaflet)

icons <- awesomeIcons(
  icon = "bus",
  library = "fa")

leaflet() %>%
  addTiles() %>%  
  addAwesomeMarkers(data = hex %>% filter(id_hex == hex_prob) %>% st_centroid(), icon = icons) %>%
  addPolygons(data = acess_prog_sf, color = "blue", stroke = NA, fillOpacity = 0.5) %>%
  addPolygons(data = acess_corri_sf, color = "red", stroke = NA, fillOpacity = 0.5) %>%
  addPolylines(data = limits, color = "black")

# zomm
leaflet() %>%
  addTiles() %>%  
  addAwesomeMarkers(data = hex %>% filter(id_hex == hex_prob) %>% st_centroid(), icon = icons)

# map
ggplot()+
  geom_sf(data = acess_prog_sf, fill = "blue", color = NA, alpha = 0.3)+
  geom_sf(data = acess_prog_sf, fill = "red", color = NA, alpha = 1)


# FAZER TESTE PARA HEX VIZINHOS COM GRANDE DIFERENCA -----------------------------------------------
hex_high <- "89801045263ffff"
hex_low <- "8980104527bffff"


tt_padrao_vizinhos <- fread("../data/ttmatrix_fim/ttmatrix_forpadrao.csv") %>% filter(origin %in% c(hex_high, hex_low))

tt_corrigido_vizinhos <- fread("../data/ttmatrix_fim/ttmatrix_forcorrigidocm.csv")  %>% filter(origin %in% c(hex_high, hex_low))

# problematico
tt_prob_vizinhos <- full_join(tt_padrao_vizinhos, tt_corrigido_vizinhos %>% select(par_id, tt_median), 
                     by = "par_id",
                     suffix = c(".padrao", ".corrigido"))


hex_teste <- tt_prob %>% mutate(tt_median.corrigido = tt_median.corrigido/60,
                                tt_median.padrao = tt_median.padrao/60)

acess_corri <- hex_teste %>%
  filter(tt_median.corrigido <= 65)

acess_prog <- hex_teste %>%
  filter(tt_median.padrao <= 65)




# FAZER TESTE  PARA ZONA EXTREMO DE ACESSIBILIDADE P50 X P85 ---------------------------------------

hex_prob <- "8980104e603ffff"

tt_p50 <- fread("../data/ttmatrix_fim/ttmatrix_forcorrigidocm.csv") %>% filter(origin == hex_prob)

tt_p85 <- fread("../data/ttmatrix_fim/ttmatrix_forcorrigidoce.csv")  %>% filter(origin == hex_prob)

# problematico
tt_prob <- full_join(tt_p50, tt_p85 %>% select(par_id, tt_median, sd), 
                     by = "par_id",
                     suffix = c(".p50", ".p85"))


hex_teste <- tt_prob %>% mutate(tt_median.p50 = tt_median.p50/60,
                                tt_median.p85 = tt_median.p85/60)

acess_corri <- hex_teste %>%
  filter(tt_median.p50 <= 65)

acess_prog <- hex_teste %>%
  filter(tt_median.p85 <= 65)

# hexagonos
hex <- read_rds("../data/hex_municipio/hex_for_09.rds")

# hex %>% filter(id_hex == doido1) %>% mapview() + mapview(hex %>% filter(id_hex == doido2))

acess_prog_sf <- hex %>% right_join(acess_prog, by = c("id_hex" = 'destination'))
acess_corri_sf <- hex %>% right_join(acess_corri, by = c("id_hex" = 'destination'))

library(leaflet)

icons <- awesomeIcons(
  icon = "bus",
  library = "fa")

leaflet() %>%
  addTiles() %>%
  addAwesomeMarkers(data = hex %>% filter(id_hex == hex_prob) %>% st_centroid(), icon = icons) %>%
  addPolygons(data = acess_corri_sf, color = "blue", stroke = NA, fillOpacity = 0.5) %>%
  addPolygons(data = acess_prog_sf, color = "red", stroke = NA, fillOpacity = 0.5) %>%
  addPolylines(data = limits, color = "black")













# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TESTES --------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# TESTAR --------------------------------------------------------------------------------------
forcorrigidocm700 <- read_rds("../data/acess/acess_forcorrigidocm_7-0.rds")

teste1 <- ggplot(data = forcorrigidocm700) + geom_sf(aes(fill = CMA_TT_30), color = NA) + scale_fill_viridis_c(option = "B")

forcorrigidoce700 <- read_rds("../data/acess/acess_forcorrigidoce_7-0.rds")

teste2 <- ggplot(data = forcorrigidoce700) + geom_sf(aes(fill = CMA_TT_30), color = NA) + scale_fill_viridis_c(option = "B")

library(patchwork)

teste1 + teste2 + plot_layout(ncol = 1)

# COMPARAR ------------------------------------------------------------------------------------

for_corrigido <- read_rds("../data/acess/acess_forcorrigidocm_7-15.rds")
for_padrao <- read_rds("../data/acess/acess_forpadrao_7-15.rds")

for_comparacao <- for_corrigido %>%
  left_join(for_padrao %>% st_set_geometry(NULL) %>% select(origin, CMA_ET_15:CMA_TT_60), 
            by = c("origin"), suffix = c(".corrigido", ".padrao"))

# Comparar trabalho 30 minutos para TP
for_comparacao_tt <- for_comparacao %>%
  select(origin, starts_with("CMA_TT_30")) %>%
  mutate(dif = CMA_TT_30.corrigido - CMA_TT_30.padrao)

for_comparacao_tt %>%
  mutate(dif = ifelse(dif > 100000, 100000, dif)) %>%
  ggplot()+
  geom_sf(aes(fill = dif), color = NA)+
  scale_fill_gradient2(low = "red", mid = "white", high = "blue")
  # scale_color_brewer(palette = "RdBu")



# TESTES --------------------------------------------------------------------------------------

acess <- read_rds("../data/acess/acess_for_corrigido.rds")

theme_for_CMA <- function(base_size) {
  
  theme_void(base_family="Roboto Condensed") %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      plot.title = element_text(hjust = 0, vjust = 4)
      
      
    )
}


acess %>%
  filter(mode == "transit") %>%
  select(starts_with("CMA_TT")) %>%
  gather("indicador", "valor", CMA_TT_15:CMA_TT_120) %>%
  mutate(threshold1 = as.integer(str_extract(indicador, "\\d+$"))) %>%
  # Pegar somente esses threshoold
  filter(threshold1 %in% c(30, 60, 90)) %>%
  mutate(threshold_name = paste0(str_extract(indicador, "\\d+$"), " minutos")) %>%
  mutate(threshold_name = forcats::fct_reorder(factor(threshold_name), threshold1)) %>%
  ggplot()+
  geom_sf(aes(fill = valor), color = NA)+
  facet_wrap(~threshold_name, ncol = 1)+
  viridis::scale_fill_viridis(option = "B")+
  theme_for_CMA()
  