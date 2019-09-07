# PASSO A PASSO1!!!! --------------------------------------------------------------------------

# 1) Calcular a distancia entre cada ponto de GPS na rede!
# 2) Entao, para cada ponto de GPS, calcular sua distancia acumulada! (o tempo vai ser o x, a dist ac vai ser o y!
# da funcao approx)
# 3) Pegar, entao, a distancia acumulada entre cada parada na rede (vai ser o xout)
# 4) Entao a funcao sera approx(x = tempo, y = dist_ac, xout = dist_ac_paradas)



# PEGAR EXEMPLO -------------------------------------------------------------------------------

# Teste
gps <- read_rds("../data/gps_com_sentido/gps_com_sentido_2018-09-05.rds")

# Funcao

interpolar_tempo_parada <- function(gps_path) {
  
  # TESTE
  # gps_viagem_list <- gps$`71`$`32101` %>% filter(viagem == 1)
  # gps_viagem_list <- gps$`41`$`32053` %>% filter(viagem == 2)
  # gps_viagem_list <- gps$`86`$`33166` %>% filter(viagem == 2)
  # gps_viagem_list <- gps$`82`$`32768` %>% filter(viagem == 7)
  # gps_viagem_list <- gps$`806`$`33285` %>% filter(viagem == 6)
  # gps_viagem_list <- gps$`92`$`32045` %>% filter(viagem == 1)
  # gps_viagem_list <- gps$`73`$`38633` %>% filter(viagem == 2)
  # gps_viagem_list <- gps$`76`$`32724` %>% filter(viagem == 1)
  # gps_viagem_list <- gps$`456`$`32518` %>% filter(viagem == 2)
  # gps_viagem_list <- gps$`13`$`33637` %>% filter(viagem == 1)
  # gps_viagem_list <- gps$`26`$`34094` %>% filter(viagem == 4)
  # gps_viagem_list <- gps$`45`$`32341` %>% filter(viagem == 3)
  # gps_viagem_list <- gps$`42`$`33087` %>% filter(viagem == 3)
  
  
  interpolar_por_viagem <- function(gps_viagem_list) {
    
    # Extrair a linha e sentido
    linha_sentido_chr <- unique(gps_viagem_list$linha_sentido)[1]
    # Extrair viagem
    viagem_chr <- unique(gps_viagem_list$viagem)[1]
    # Extrair carro
    vehicleid_chr <- unique(gps_viagem_list$vehicleid)[1]
    
    # TESTE USANDO RGEOES -------------------------------------------------------------------------
    
    linha_escolhida <- linhas %>%
      filter(linha_sentido == linha_sentido_chr) %>%
      as_Spatial()
    
    gps_viagem_sp <- gps_viagem_list %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326, remove =F) %>%
      st_transform(29194) %>% as_Spatial()
    
    # Aplicar geosphere
    fim2 <- rgeos::gProject(linha_escolhida, gps_viagem_sp)
    
    gps_viagem_list <- gps_viagem_list %>% mutate(dist = fim2) %>%
      # arredondar para o metro
      mutate(dist = round(dist, 0)) %>%
      # checar se as distancias acumuladas estao aumentando mesmo!
      mutate(dif_dist = dist - lag(dist)) %>%
      mutate(dif_time = difftime(hora, lag(hora), units = "secs")) %>%
      mutate(dif_time = as.numeric(dif_time)) %>%
      mutate(vel = (dif_dist / dif_time) * 3.6) %>%
      mutate(vel = ifelse(is.nan(vel), 0, vel))
      # # excluir velocidades maiores que 70km/h e menores que -10km/h, mantendo sempre as duas primeiras
      # # observacoes
      # filter(!(vel < -10 & row_number() %nin% c(1, n()))) %>%
      # filter(!(vel >  70 & row_number() %nin% c(1, n())))
    
    
    # PASSO 3 -------------------------------------------------------------------------------------
    
    distancia_paradas_teste <- distancia_paradas[linha_sentido == linha_sentido_chr]
    # O ponto da primeira e ultima parada da linha nao estao exatamente em cima da primeira e ultima parada
    # Entao, adotar ...
    
    
    # PASSO 4 -------------------------------------------------------------------------------------
    # Garantir hora e distancia unicos
    # vai <- distinct(vai, hora, dist, .keep_all = TRUE)
    # Pegar hora sem a primeira e ultima
    # x <- vai$hora[-c(1, nrow(vai))]
    
    # Se a distancia ate a ultima parada for menor que a distancia ate a penultima, excluir ultima obs
    if (gps_viagem_list$dist[nrow(gps_viagem_list)] < gps_viagem_list$dist[nrow(gps_viagem_list) - 1]) {
      
      x <- gps_viagem_list$hora[-nrow(gps_viagem_list)]
      y <- gps_viagem_list$dist[-nrow(gps_viagem_list)]
      
    } else {
      
      x <- gps_viagem_list$hora
      y <- gps_viagem_list$dist
      
    }
    
    # Pegar a distancia sem a primeira e ultima
    # y <- vai$dist[-c(1, nrow(vai))]
    # Pegar as paradas sem a primeira e ultima (xout representa as distancias em que eu quero estimar
    # a hora, que no caso sao as paradas)
    xout <- distancia_paradas_teste$dist[-c(1, nrow(distancia_paradas_teste))]
    # xout <- distancia_paradas_teste$dist
    
    
    
    # tirar outliers
    a <- cooks.distance(lm(as.integer(x, origin = "1970-01-01") ~y))
    mean_out <- 4*mean(a)
    pontos_fora <- which(a > mean_out)
    # tirar
    if (length(pontos_fora) == 0) {
      
      x_novo <- x
      y_novo <- y
      
    } else {
      x_novo <- x[-pontos_fora]
      y_novo <- y[-pontos_fora]
    }
    
    plot(x = y_novo, y = x_novo)
    
    uhlala <- as.POSIXct(approx(x = y_novo, y = x_novo, xout = xout, ties = mean, rule = 2)$y, 
                         origin = "1970-01-01")
    
    
    fim_ne <- distancia_paradas_teste %>%
      mutate(hora = c(gps_viagem_list$hora[1], uhlala, gps_viagem_list$hora[nrow(gps_viagem_list)])) %>%
      mutate(viagem = viagem_chr,
             vehicleid = vehicleid_chr)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # plot teste
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # exemplo para a linha 86
    # 
    # plot_antes <- ggplot()+
    #   geom_point(aes(x = y, y = x), alpha = 0.3)+
    #   labs(x = "Distância acumulada na linha", y = "Hora") +
    #   theme_ipsum() +
    #   theme(plot.margin = unit(c(1, 1, 1, 1),"mm"),
    #         axis.text.x = element_blank(),
    #         axis.title.x = element_blank())+
    #   coord_cartesian(xlim = c(0, 20300)) 
    # 
    # plot_dps <- ggplot()+
    #   geom_point(aes(x = y_novo, y = x_novo), alpha = 0.3)+
    #   geom_point(data= fim_ne, aes(x = dist, y = hora), color = "red")+
    #   ggrepel::geom_label_repel(data = fim_ne, aes(x = dist, y = hora, label =stop_sequence),
    #                            arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "last"),
    #                            force = 2,
    #                            size = 2,
    #                            show.legend = TRUE)+
    #   labs(x = "Distância acumulada na linha", y = "Hora") +
    #   theme_ipsum() +
    #   theme(plot.margin = unit(c(1, 1, 1, 1),"mm"),
    #         axis.title.y = element_blank())+
    #   coord_cartesian(xlim = c(0, 20300)) 
    # 
    # # exportar plot
    # plot_antes + plot_dps + plot_layout(nrow = 2)
    # ggsave("figure/5-outliers_interpolacao.png", width = 16, height = 10, units = "cm", dpi = 300)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # END plot teste
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    # basic check: quando a distancia da parada para o ponto de gps mais proximo?
    teste <- RANN::nn2(gps_viagem_list %>% select(lon, lat),
                       distancia_paradas_teste %>% select(lon, lat),
                       k = 1)
    
    fim_ne <- fim_ne %>% mutate(dist_nearest_stop = teste$nn.dists*111320) %>%
      filter(dist_nearest_stop < 200)
  
  }

  # Pegar so as viagens q começaram ate as 10 da manha e dividir GPS por viagem
  
  fun_teste <- function(gps) {
    
    setDT(gps)
    
    viagens_pico <- gps[, .(inicio = as.ITime(first(hora))),
                                         keyby = .(viagem)]
    
    viagens_pico <- viagens_pico[inicio < as.ITime("10:00:00")]

    gps_pico <- gps[viagem %in% viagens_pico$viagem]

    gps_fim <- split(gps_pico, by = "viagem")
    
    # gps_fim <- gps_fim[map(gps_fim, length)>0]
    
  }
  
  fun_safe <- possibly(fun_teste, otherwise = NA_real_)
  
  # Abrir GPS   
  gps <- read_rds(gps_path)
  
  teste <- map_depth(gps, 2, fun_safe)
  
  # AGORA VAI
  interpolar_por_viagem_safe <- possibly(interpolar_por_viagem, otherwise = NA_real_)
  
  go <- map_depth(teste, 3, interpolar_por_viagem_safe)
  
  # Tirar as viagens NA e carros vazios
    teste1 <- map_depth(go, 2, function(x) x[!is.na(x)]) %>%
    map(function (x) x[lapply(x,length)>0])
  
  # Tirar linhas vazias
  teste2 <- teste1[lapply(teste1, length) > 0]
  
  # Arrochar rbindlist
  teste3 <- map(map_depth(teste2, 2, rbindlist), rbindlist)
  
  # Salvar
  dia <- str_extract(gps_path, "\\d{4}-\\d{2}-\\d{2}")
  path_out <- sprintf("../data/gps_viagens/gps_viagens_teste_%s.rds", dia)
  write_rds(teste3, path_out)
  
    
}

# APLICAR -------------------------------------------------------------------------------------

# Abrir linhas
linhas <- st_read("../data/linhas/2018/linhas_2018-09.shp") %>%
  mutate(linha = str_extract(shape_id, "\\d{3}")) %>%
  mutate(linha = as.integer(linha)) %>%
  mutate(sentido = str_sub(shape_id, -1, -1)) %>%
  mutate(linha_sentido = paste0(linha, "-", sentido)) %>%
  st_transform(29194)

# Abrir distancia entre paradas
distancia_paradas <- read_rds("../data/distancia_entre_paradas_consolidadas_2019-09.rds")

# Pegar arquivos de gps com viagens
dias_uteis <- c(3, 4, 5, 6, 10, 11, 12, 13, 14, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28)

gps_files_09 <- dir("../data/gps_com_sentido", full.names = TRUE, pattern = "^gps")[dias_uteis]

tictoc::tic()
# invisible(parallel::mclapply(gps_files_09[1:4], interpolar_tempo_parada, mc.cores = 4))
tictoc::toc()
# invisible(parallel::mclapply(gps_files_09[5:8], interpolar_tempo_parada, mc.cores = 4))
.rs.restartR()
# invisible(parallel::mclapply(gps_files_09[9:12], interpolar_tempo_parada, mc.cores = 4))
# invisible(parallel::mclapply(gps_files_09[13:16], interpolar_tempo_parada, mc.cores = 4))
# invisible(parallel::mclapply(gps_files_09[17:19], interpolar_tempo_parada, mc.cores = 4))


# testar
teste_novo <- read_rds("../data/gps_viagens/gps_viagens_teste_2018-09-05.rds")

# TESTAR --------------------------------------------------------------------------------------

teste <- read_rds("../data/gps_viagens/gps_viagens_2018-09-04.rds")

teste$'86' %>% filter(vehicleid == 33166) %>% View()
