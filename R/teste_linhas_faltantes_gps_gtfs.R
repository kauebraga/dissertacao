
# bilhetagem <- "../data/bilhetagem_integrado/2018/bilhetagemintegrado_2018-09-03.csv"
# gps <- "../data/gps/2018/novo/09/gps_2018-09-03.csv"

integrar_gps <- function(bilhetagem, gps) {
  
  # ABRIR DADOS -------------------------------------------------------------
    
  # Bilhetagem
  bilhetagem <- read_csv(bilhetagem)

  
  # GPS
  gps <- fread(gps)
  gps[, id_gps := 1:nrow(gps)]
  gps[, vehicleid := as.numeric(vehicle_vehicleid)]
  gps[, hora := fasttime::fastPOSIXct(hora, tz="UTC")]
  gps <- gps[, .(vehicleid, hora, lon = longitude, lat = latitude)]

  # Linhas do renan
  expresos_cjao <- st_read("../data-raw/linhas_fortaleza_renan/SIT-FOR24052017.shp", crs = 4326,
                           options = "ENCODING=WINDOWS-1252") %>%
    mutate(linha = as.character(CDIGO)) %>%
    mutate(linha = as.numeric(linha)) %>%
    filter(TIPO_LINHA == "CJI" | grepl("express(a|o)", NOME, ignore.case = TRUE))
  
  # Tirar expressos e corujoes da base da bilhetagem
  bilhetagem <- bilhetagem %>%
    filter(linha %nin% c(61, 63, 65, 665, 619, expresos_cjao$linha))


  
  setDT(bilhetagem)
  bilhetagem_v1 <- bilhetagem[order(vehicleid, hora)]
  bilhetagem_v1 <- bilhetagem_v1[, .(vehicleid, linha)]
  bilhetagem_v1 <- unique(bilhetagem_v1, by = c("vehicleid", "linha"))

  
  # agora, juntar o gps com a bilhetagem (usar join do data.table)
  # setDT(gps)
  gps_v2 <- merge(gps, bilhetagem_v1, all.x = TRUE, allow.cartesian=TRUE)
  
  # fazer sumario
  fim <- gps_v2 %>%
    mutate(dia = date(hora)) %>%
    mutate(linha1 = ifelse(is.na(linha), "nao-ok", "ok")) %>%
    count(dia, vehicleid, linha1) %>%
    count(dia, linha1)
  
  return(as_tibble(fim))
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# aplicar funcao    
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dir_bi <- dir("../data/bilhetagem_integrado/2018", full.names = TRUE, 
              pattern = "bilhetagemintegrado_2018-09")
dir_gps <- dir("../data/gps/2018/novo/09", full.names = TRUE, pattern = "*.csv")

# aplicar

fim <- map2_dfr(dir_bi, dir_gps, integrar_gps)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Selecionar os dias nao uteis
dias_nuteis <- c("01", "02", "07", "08", "09", "15", '16', '22', '23', '29', '30')
dias_nuteis1 <- paste0("2018-09-", dias_nuteis)

# Fazer plot
fim %>%
  mutate(dia = as.character(dia)) %>%
  filter(dia %nin% dias_nuteis1) %>%
  mutate(dia = date(dia)) %>%
  ggplot()+
  geom_col(aes(x = dia, y = n, fill = linha1))+
  scale_x_date(date_labels = "%d-%m", date_breaks = "1 day")+
  scale_fill_brewer(palette = "Set1")+
  theme_ipsum_rc(grid = "Y") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        plot.margin=unit(c(1,1,1,1),"mm")) +
  labs(x = "Dia", y = "Quantidade de veículos", fill = "Linha estimada?")
# labs(title = "Quantidade de validações georreferenciadas, por dia",
#      subtitle = "Dias úteis de Setembro/2019")

ggsave("figure/3-qualidade_geo_gps.png", units = "cm", width = 16, height = 10, dpi = 300)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# linhas faltantes do gtfs
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

linhas_falt <- read_csv("../data/linhas_faltante.csv", col_names = FALSE) %>%
  mutate(a = str_extract(X1, "^\\d*")) %>%
  distinct(a)

bilhetagem %>% filter(linha %in% linhas_falt$a) %>% View()

# compara os carros do gps totais com gtfs

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# teste aquele snap la 
# (https://stackoverflow.com/questions/57103451/create-sequence-of-values-based-on-multiple-column-values-in-r/57174011#57174011)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gps_viagem_list <- gps$`86`$`33166` %>% filter(viagem == 2)

shapes_desagregados <- read_rds("../data/shapes_desagregados.rds")
shapes_desagregados <- shapes_desagregados %>%
  setDT() %>%
  .[linha_sentido == linha_sentido_chr]

ooi <- RANN::nn2(select(shapes_desagregados, lon, lat), select(gps_viagem_list, lon, lat),
                 k = 6)
# TESTE
search <- cbind(ooi[["nn.idx"]] %>% as.data.frame(), ooi[["nn.dists"]] %>% as.data.frame())
colnames(search) <- c("V1.value", "V2.value", "V3.value", "V1.dist", "V2.dist", "V3.dist")
search <- cbind(ooi[["nn.idx"]] %>% as.data.frame())

# criar pontos ancoras, onde eu sei q o ponto esta ok

points <- st_transform(points, crs)
line <- st_transform(line, crs)
# buffer the points by the tolerance
points_buf <- st_buffer(points, 20)
# intersect the line with the buffer
line_intersect <- st_intersection(line, points_buf)
# convert mutlinestrings (more than one road segment) into linestrings
line_intersect <- do.call(rbind,lapply(1:nrow(line_intersect),function(x){st_cast(line_intersect[x,],"LINESTRING")}))

# for each line intersection, calculate the nearest point on that line to our gps point
nearest_pt <- do.call(rbind,lapply(seq_along(points$id), function(i){
  points[points$id==i,] %>%  st_nearest_points(line_intersect[line_intersect$id==i,]) %>% st_sf %>%
    st_cast('POINT') %>% mutate(id = i)
}))

nearest_pt<- nearest_pt[seq(2, nrow(nearest_pt), by = 2),] %>%
  mutate(option = 1:nrow(.))

# find an unambiguous reference point with only one snap option
unambiguous_pt <- nearest_pt %>%
  group_by(id) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count == 3) %>%
  slice(1)


first_value <- search_result$V1[1]
search$ordered <- c(first_value, apply(search[-1, ], 1, function(x) {
  x <- x[x > first_value]
  x[which.min((x - first_value))]
}))

# vamos la
search$teste1 <- c(0, diff(search$V1))

search <- search %>%
  mutate(result1 = ifelse(teste1 < 0 | teste1 > 100, V2, V1))

# nova interacao

search$teste2 <- c(0, diff(search$result1))

search <- search %>%
  mutate(result2 = ifelse(teste2 < 0 | teste2 > 100, V3, result1))


search$teste3 <- c(0, diff(search$result2))

search <- search %>%
  mutate(result3 = ifelse(teste3 < 0 | teste3 > 100, V4, result2))

search$teste4 <- c(0, diff(search$result3))

search <- search %>%
  mutate(result4 = ifelse(teste4 < 0 | teste4 > 100, V5, result3))

search$teste5 <- c(0, diff(search$result4))

search <- search %>%
  mutate(result5 = ifelse(teste5 < 0 | teste5 > 100, V6, result4))

# ~~~~~~~~~~~
# outro teste

# definir pontos ancoras



# ---------------------------------------------------------------------------------------------

rmd_files: ["01_introducao.Rmd", "02_revisao.Rmd", "03_consolidacao.Rmd", "04_metodo.Rmd", "05_resultados.Rmd", "06_conclusao.Rmd"]

