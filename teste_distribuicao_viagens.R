# abrir

tviagem <- read_rds("../data/tempos_entre_paradas_2018-09.rds")

# primeiro, analisar as amostras:
tempos_entre_paradas_bind <- rbindlist(tviagem)

# tirar outliers outside 1.5 IQR
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# pegar so hora pico
tempos_entre_paradas_pico <- tempos_entre_paradas_bind[inicio_15 %between% c(as.ITime("06:00:00"), as.ITime("10:00:00"))]

# identificar outliers como NA
tempos_entre_paradas_pico[, tempo_viagem := remove_outliers(tempo_viagem), by = .(a, inicio_15)]

# deletar os outliers
tempos_entre_paradas_bind_soutliers <- tempos_entre_paradas_pico[!is.na(tempo_viagem)]

# selecionar so o trecho e inicio_15
tempos_entre_paradas_dist <- tempos_entre_paradas_bind_soutliers[, .(inicio_15, a, tempo_viagem)]

# pegar os que tem a maior amostra
maiores_amostras <- tempos_entre_paradas_dist[, .N, by = .(inicio_15, a)]

# pegar so trechos e intervalos com amostra maior que 50
setorder(maiores_amostras, -N)

maiores_amostras %>% 
  filter(N >= 10) %>%
  mutate(Nn = ifelse(N > 100, 100, N)) %>% 
  ggplot() + 
  geom_histogram(aes(Nn), bins = 30)+
  facet_wrap(~as.factor(inicio_15), scales = "free_y")

# pegar as 20 maiores
amostras_20_maiores <- maiores_amostras[1:20, ui := paste0(inicio_15, "-", a) ]

Statmean <- ggproto("Statmean", Stat,
                           compute_group = function(data, scales) {
                             mean <- mean(data$x)
                             data.frame(xintercept=mean)
                           },
                           required_aes = c("x")
)

stat_mean <- function(mapping = NULL, data = NULL, geom = "vline",
                              position = "identity", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = Statmean, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


StatPercentileX <- ggproto("StatPercentileX", Stat,
                           compute_group = function(data, scales, probs) {
                             percentiles <- quantile(data$x, probs=probs)
                             data.frame(xintercept=percentiles)
                           },
                           required_aes = c("x")
)

stat_percentile_x <- function(mapping = NULL, data = NULL, geom = "vline",
                              position = "identity", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatPercentileX, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatPercentileXLabels <- ggproto("StatPercentileXLabels", Stat,
                                 compute_group = function(data, scales, probs) {
                                   percentiles <- quantile(data$x, probs=probs)
                                   data.frame(x=percentiles, y=Inf,
                                              label=paste0("P", probs*100))
                                              # label=paste0("p", probs*100, ": ",
                                              #              round(percentiles, digits=3)))
                                 },
                                 required_aes = c("x")
)

stat_percentile_xlab <- function(mapping = NULL, data = NULL, geom = "text",
                                 position = "identity", na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatPercentileXLabels, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


# plotar
tempos_entre_paradas_dist %>%
  mutate(ui = paste0(inicio_15, "-", a)) %>%
  filter(ui %in% amostras_20_maiores$ui) %>%
  ungroup() %>%
  ggplot(aes(x = tempo_viagem))+
  geom_density()+
  stat_mean(linetype = 1, color = "red")+
  stat_percentile_x(probs = c(0.5, 0.85), linetype = 2)+
  stat_percentile_xlab(probs = c(0.5, 0.85), hjust=1, vjust=-0.5, angle=90)+
  facet_wrap(~as.factor(ui), scales = "free")
  
