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

library(patchwork)

forpadrao <- read_rds("../data/acess/acess_forpadrao.rds") %>% mutate(tipo = "PR")
forp50 <- read_rds("../data/acess/acess_forcorrigidocm.rds") %>% mutate(tipo = "P50")
forp85 <- read_rds("../data/acess/acess_forcorrigidoce.rds") %>% mutate(tipo = "P85")

juntos_cenario <- rbind(forpadrao, forp50, forp85)

juntos_cenario_tt <- juntos_cenario %>%
  select(origin, tipo, CMA_TT_65) %>%
  spread(tipo, CMA_TT_65) %>%
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

juntos_cenario_et <- juntos_cenario %>%
  select(origin, tipo, CMA_ET_50) %>%
  spread(tipo, CMA_ET_50) %>%
  mutate(dif_1 = PR - P50) %>%
  mutate(dif_2 = P50 - P85) %>%
  mutate(dif_1_log = log(P50/PR)) %>%
  mutate(dif_1_log_tc = ifelse(dif_1_log > 0.6, 0.6, 
                               ifelse(dif_1_log < -0.6, -0.6, dif_1_log))) %>%
  mutate(dif_2_log = log(P50/P85)) %>%
  mutate(dif_2_log_tc = ifelse(dif_2_log > 0.6, 0.6, 
                               ifelse(dif_2_log < 0, 0, dif_2_log)))
  
  # mutate(dif = P50 - P85) %>%
  # mutate(dif_perc_total = dif/total_matriculas)


# COMPARA PARA TRABALHO
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# COMPARACAO ABSOLUTA

juntos_cenario %>%
  mutate(tipo = factor(tipo, levels = c("PR", "P50", "P85"))) %>%
  ggplot()+
  geom_sf(data= limits, fill = NA)+
  geom_sf(aes(fill = CMA_TT_65), color = NA) +
  facet_wrap(~tipo)+
  scale_fill_viridis_c(option = "B", 
                       breaks = c(4, 250000, 500000), 
                       labels = c("0", "250", "500 mil"))+
  labs(fill = "Quantidade de empregos\n acessíveis em 65 minutos")+
  theme_mapa() +
  juntos_cenario %>%
  mutate(tipo = factor(tipo, levels = c("P85", "P50", "PR"))) %>%
  ggplot()+
  # geom_jitter(aes(x = tipo, y = CMA_TT_65))+
  geom_boxplot(aes(x = tipo, y= CMA_TT_65), alpha = 0.5) +
  labs(y = "Quantidade de empregos")+
  scale_y_continuous(breaks = c(4, 250000, 500000), 
                     labels = c("0", "250", "500 mil"))+
  coord_flip()+
  theme_ipsum_rc(grid = "X") +
  plot_layout(ncol = 1, heights = c(3, 1))

median(filter(juntos_cenario, tipo == "PR")$CMA_TT_65, na.rm = TRUE)
median(filter(juntos_cenario, tipo == "P50")$CMA_TT_65, na.rm = TRUE)
median(filter(juntos_cenario, tipo == "P85")$CMA_TT_65, na.rm = TRUE)
  
ggsave("figure/artigo_anpet/4-acess_cenarios_tt.png", dpi = 300, units = "cm", width = 16, height = 10)


# COMPARACAO RELATIVA ENTRE CENARIOS

juntos_cenario_tt %>%
  ggplot()+
  geom_sf(data = limits, fill = NA)+
  geom_sf(aes(fill = dif_1_log_tc), color = NA)+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       breaks = c(-0.70, 0, 0.70),
                       labels = c("-70%", "0", "+70%"))+
  labs(fill = "Diferença relativa de empregos\n acessíveis em 65 minutos",
       title = "PR - P50")+
  theme_mapa() +

juntos_cenario_tt %>%
  ggplot()+
  geom_sf(data = limits, fill = NA)+
  geom_sf(aes(fill = dif_2_log_tc), color = NA)+
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       breaks = c(0, 0.35, 0.70),
                       labels = c("0", "-35%", "-70%"))+
  labs(fill = "Diferença relativa de empregos\n acessíveis em 65 minutos",
       title = "P50 - P85")+
  theme_mapa()


ggsave("figure/artigo_anpet/4-acess_cenarios_ind_tt.png", dpi = 300, units = "cm", width = 16, height = 10)





juntos_cenario %>%
  mutate(tipo = factor(tipo, levels = c("PR", "P50", "P85"))) %>%
  ggplot()+
  geom_sf(data= limits, fill = NA)+
  geom_sf(aes(fill = CMA_ET_50), color = NA) +
  facet_wrap(~tipo)+
  scale_fill_viridis_c(option = "B",
                       breaks = c(4, 90000, 180000),
                       labels = c("0", "90", "180 mil"))+
  labs(fill = "Quantidade de matrículas\n acessíveis em 50 minutos")+
  theme_mapa()+
  juntos_cenario %>%
  mutate(tipo = factor(tipo, levels = c("P85", "P50", "PR"))) %>%
  ggplot()+
  # geom_jitter(aes(x = tipo, y = CMA_TT_65))+
  geom_boxplot(aes(x = tipo, y= CMA_ET_50), alpha = 0.5) +
  labs(y = "Quantidade de matrículas")+
  scale_y_continuous(breaks = c(4, 90000, 180000), 
                     labels = c("0", "90", "180 mil"))+
  coord_flip()+
  theme_ipsum_rc(grid = "X") +
  plot_layout(ncol = 1, heights = c(3, 1))

median(filter(juntos_cenario, tipo == "PR")$CMA_ET_50, na.rm = TRUE)
median(filter(juntos_cenario, tipo == "P50")$CMA_ET_50, na.rm = TRUE)

ggsave("figure/artigo_anpet/4-acess_cenarios_et.png", dpi = 300, units = "cm", width = 16, height = 10)


# COMPARACAO RELATIVA ENTRE CENARIOS

juntos_cenario_et %>%
  ggplot()+
  geom_sf(data = limits, fill = NA)+
  geom_sf(aes(fill = dif_1_log_tc), color = NA)+
  scale_fill_distiller(palette = "RdBu", direction = 1,
                       breaks = c(-0.6, 0, 0.6),
                       labels = c("-60%", "0", "+60%"))+
  labs(fill = "Diferença relativa de matrículas\n acessíveis em 50 minutos",
       title = "PR - P50")+
  theme_mapa() +
  
  juntos_cenario_et %>%
  ggplot()+
  geom_sf(data = limits, fill = NA)+
  geom_sf(aes(fill = dif_2_log_tc), color = NA)+
  scale_fill_distiller(palette = "OrRd", direction = 1,
                       breaks = c(0, 0.3, 0.6),
                       labels = c("0", "-30%", "-60%"))+
  labs(fill = "Diferença relativa de matrículas\n acessíveis em 50 minutos",
       title = "P50 - P85")+
  theme_mapa()


ggsave("figure/artigo_anpet/4-acess_cenarios_ind_et.png", dpi = 300, units = "cm", width = 16, height = 10)


# COMPARACAO RELATIVA ENTRE ATIVIDADES

        