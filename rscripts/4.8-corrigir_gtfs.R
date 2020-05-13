source("R/setup.R")

# Funcao para abrir gps
abrir_gps <- function(gps_path) {
  
  gps <- read_rds(gps_path)
  # Tirar linhas que nao tem registro
  vazios <- map_lgl(gps, function (x) length(x) > 0)
  gps <- gps[vazios]
  ui <- map(gps, as.data.table)
  
}

# Abrir GPSs

gps_files <- dir("../data/gps_viagens", full.names = TRUE,
                 pattern = "^gps_viagens_teste")

# Abrir trechos
gtfs_trechos_sf <- read_rds("../data/trechos_entre_paradas/trechos_entre_paradas_2019-09.rds") %>%
  rbindlist() %>%
  # Criar dummy
  mutate(a = paste0(stop_id_inicio, "-", stop_id_fim)) %>%
  filter(!is.na(stop_id_fim)) %>%
  # Selecionar unicos
  distinct(a, .keep_all = TRUE) %>%
  # distinct(stop_id_fim, .keep_all = TRUE) %>%
  select(a, length, geometry) %>%
  mutate(trecho_id = 1:n())





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#+ CALCULAR TEMPO DE VIAGEM ENTRE TODOS OS TRECHOS ---------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Extrair somente os da hora pico (6-9h)
# Depois, agrupar cada trecho de viagens a cada 15 minutos

# teste (pegar um dia)
# gps_linhas <- gps[[3]]

calcular_tempo_entre_paradas <- function(gps_linhas) {
  
  por_linha <- function(linha) {
    
    # tirar DTs com tamanho zero
    setDT(linha)
    linha[, viagem := rleid(viagem)]
    gps_trechos <- setDT(linha)[, .(dia = date(hora),
                                    linha_sentido = first(linha_sentido),
                                    stop_sequence_inicio = lag(stop_sequence),
                                    stop_sequence_fim = stop_sequence,
                                    stop_id_inicio = lag(stop_id),
                                    stop_id_fim = stop_id,
                                    tempo_viagem = difftime(hora, lag(hora), units = "secs"),
                                    inicio = lag(hora)
    ), 
    keyby = .(vehicleid, viagem)]
    
    gps_trechos[, tempo_viagem := as.numeric(tempo_viagem)]
    
    gps_trechos <- gps_trechos[!is.na(stop_id_inicio)]
    gps_trechos[, a:= paste0(stop_id_inicio, "-", stop_id_fim)]
    
    # Juntar os dfs
    gps_trechos_v1 <- merge(setDT(gps_trechos), setDT(gtfs_trechos_sf), by = c("a"))
    
    setorder(gps_trechos_v1, viagem, inicio)
    
    # Arredonda a hora de inicio (comeco de entrada no trecho) para o intervalo de 15 minutos mais
    # proximo
    gps_trechos_v2 <- gps_trechos_v1[, inicio_15 := as.ITime(round_date(inicio, "15 mins"))]
    gps_trechos_v2[, length := as.numeric(length)]
    # Pegar hora pico
    # gps_trechos_v2 <- gps_trechos_v2[inicio_15 %between% 
    #                                    c(as.ITime("04:00:00"), as.ITime("11:00:00"))]
    
    # Escolhar colunas
    gps_trechos_fim <- gps_trechos_v2[, .(dia, linha_sentido, vehicleid, 
                                          viagem, inicio_15, a, tempo_viagem, length, geometry)]
    
    # # E o tempo ENTRE viagens?
    # linha[, linha := str_extract(linha_sentido, "^\\d{1,}")]
    # linha[, sentido := str_extract(linha_sentido, "^[[:upper:]]{1}$")]
    
  }
  
  # Aplicar
  fim <- map(gps_linhas, por_linha) %>%
    rbindlist() %>%
    # Filtrar somente os tempos de viagem positivos
    .[tempo_viagem > 0] %>%
    .[, velocidade := (length/tempo_viagem) * 3.6] %>%
    # Filtrar somente as velocidades menor que 70 km/h
    .[velocidade < 70]
  
  # # Extrair dia
  # dia <- unique(fim_bind$dia)[1]
  
  # # Agrupar TODOS os trechos de viagem, em hora pico
  # tempo_entre_paradas_total <- fim[, .(tempo_mediana = median(tempo_viagem, na.rm = TRUE), 
  #                                           n = .N,
  #                                           sd = sd(tempo_viagem, na.rm = TRUE)), 
  #                                       keyby = .(dia, a, inicio_15)]
  
  # # Salvar
  # path_out <- sprintf("../data/tempo_trechos/tempo_entre_paradas_agreg_%s.rds", dia)
  # write_rds(tempo_entre_paradas_total, path_out)
  
}



# Aplicar para todos os dias -----------------------------------------------------------------------
gps <- map(gps_files, abrir_gps)

# no linux
tempo_entre_parada <- parallel::mclapply(gps, calcular_tempo_entre_paradas,
                   mc.cores = 10)
# no windows 
plan(multiprocess)
tempo_entre_parada <- furrr::future_map(gps, calcular_tempo_entre_paradas)

# salvar
write_rds(tempo_entre_parada, "../data/tempos_entre_paradas_2018-09.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AGRUPAR POR INTERVALO DE 15 MINUTOS E TRECHO ----------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tempo_entre_parada <- read_rds("../data/tempos_entre_paradas_2018-09.rds")

# primeiro, analisar as amostras:
tempos_entre_paradas_bind <- rbindlist(tempo_entre_parada)

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
tempos_entre_paradas_pico <- tempos_entre_paradas_bind[inicio_15 < as.ITime("10:00:00")]

# # tirar outliers
# teste <- tempos_entre_paradas_pico[a == "638-4598"]
# 
# setDT(uiui)[, new := remove_outliers(tempo_viagem),
#                           by = .(a, inicio_15)]


# identificar outliers como NA
tempos_entre_paradas_pico[, tempo_viagem := remove_outliers(tempo_viagem), by = .(a, inicio_15)]

# deletar os outliers
tempos_entre_paradas_bind_soutliers <- tempos_entre_paradas_pico[!is.na(tempo_viagem)]

# fazer agrupamento para o 50 e 0 85 percentil (tirando essas amostras doidas)
tempos_entre_paradas_agreg <- 
  tempos_entre_paradas_bind_soutliers[, 
                            .(geometry = first(geometry),
                              tempo_media = mean(tempo_viagem, na.rm = TRUE),
                              tempo_p50 = median(tempo_viagem, na.rm = TRUE),
                                tempo_p85 = quantile(tempo_viagem, probs = 0.85, na.rm = TRUE),
                                                     n = .N,
                                                     sd = sd(tempo_viagem, na.rm = TRUE)), 
                                keyby = .(a, inicio_15)]

# salvar
write_rds(tempos_entre_paradas_agreg, "../data/tempo_entre_paradas_raw_2019-08.rds")

# quantos trechos foram estimados? o total de trechos eh 5713
trechos_estimados <- length(unique(tempos_entre_paradas_agreg$a))

# deletar trechos e intervalo que tiveram amostra menor que 10
tempos_entre_paradas_agreg_limpo <- tempos_entre_paradas_agreg[n >= 10]

# e os trechos sujos?
tempos_entre_paradas_agreg_sujo <- tempos_entre_paradas_agreg[n < 10]

tempos_entre_paradas_agreg_sujo %>%
  count(inicio_15) %>%
  mutate(inicio_15 = as.character(inicio_15)) %>%
  filter(inicio_15 %in% c("06:30:00", "06:45:00", "07:00:00", "07:30:00")) %>%
  # mutate(inicio_15 = as.POSIXct(inicio_15)) %>%
  ggplot()+
  geom_col(aes(x = inicio_15, y = n))+
  scale_x_datetime(date_labels = "%H:%M")+
  facet_wrap(~inicio_15) %>%
  theme_ipsum()

# calcular coeficiente de variacao
tempos_entre_paradas_agreg_limpo[, cv := sd/tempo_media]

# pegar so trechos com cv menor que 70%
tempo_entre_paradas_fim <- tempos_entre_paradas_agreg_limpo[cv <= 1.0]

# salvar
write_rds(tempo_entre_paradas_fim, "../data/tempo_entre_paradas_fim_2019-08.rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# map ----------------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tempo_entre_paradas_fim <- read_rds("../data/tempo_entre_paradas_fim_2019-08.rds")

source("R/themes.R")
# pegar limites de fortaleza
muni <- read_rds("../data/muni/municipios_ce.rds") %>%
  filter(NM_MUNICIP == "FORTALEZA")

# pegar ceara
uf <- geobr::read_state(23)

# pegar bb
bb_muni <- sf::st_bbox(muni)

# pegar streest
streets <- read_rds("../data/streets_fortaleza.rds")

# pegar ruas
streets <- streets %>%
  # Selecionar so primarias e secundarias
  filter(highway %in% c("primary", "secundary", "residential")) %>%
  filter(osm_id %nin% 43042156)

# fazer mapa
tempo_entre_paradas_fim %>%
  st_sf() %>%
  mutate(inicio_15 = as.character(inicio_15)) %>%
  filter(inicio_15 %in% c("06:45:00", "07:00:00", "07:15:00", "07:30:00")) %>%
  # mutate(inicio_15 = as.POSIXct(inicio_15)) %>%
  ggplot()+
  geom_sf(data = uf, fill = "white", size = 0.3)+
  geom_sf(data= streets, fill="white", color="gray85", size = 0.2)+
  geom_sf(data = muni, fill="transparent", color = gray(.5), size = 0.6)+
  geom_sf(size = 0.4)+
  facet_wrap(~inicio_15)+
  coord_sf(xlim = c(bb_muni[1], bb_muni[3]), ylim = c(bb_muni[2], bb_muni[4]))+
  theme_opts()

ggsave("figure/5-resultado_trechos.png", width = 16, height = 12, units = "cm", dpi = 300)
  


    # # Aplicar para todos os dias
# gps <- map(gps_files, abrir_gps)
# invisible(parallel::mclapply(gps, calcular_tempo_entre_paradas,
#                                                 mc.cores = 2))

# # Aplicar por semana!
# gps <- map(gps_files_s01, abrir_gps)
# parallel::mclapply(gps, calcular_tempo_entre_paradas,
#                    mc.cores = 4) %>%
#   rbindlist() %>%
#   write_rds("../data/tempo_trechos/gps_tempo_trechos_2018-09_S01.rds")
# 
# gps <- map(gps_files_s02, abrir_gps)
# parallel::mclapply(gps, calcular_tempo_entre_paradas,
#                    mc.cores = 4) %>%
#   rbindlist() %>%
#   write_rds("../data/tempo_trechos/gps_tempo_trechos_2018-09_S02.rds")
# 
# gps <- map(gps_files_s03, abrir_gps)
# parallel::mclapply(gps, calcular_tempo_entre_paradas,
#                    mc.cores = 4) %>%
#   rbindlist() %>%
#   write_rds("../data/tempo_trechos/gps_tempo_trechos_2018-09_S03.rds")
# 
# gps <- map(gps_files_s04, abrir_gps)
# parallel::mclapply(gps, calcular_tempo_entre_paradas,
#                    mc.cores = 4) %>%
#   rbindlist() %>%
#   write_rds("../data/tempo_trechos/gps_tempo_trechos_2018-09_S04.rds")



# TESTE SO PARA UM DIA! -----------------------------------------------------------------------

# Agrupar TODOS os trechos de viagem, em hora pico
tempo_entre_paradas_total <- fim_bind[, .(tempo_mediana = median(tempo_viagem, na.rm = TRUE), 
                                          n = .N,
                                          sd = sd(tempo_viagem, na.rm = TRUE)), 
                                      keyby = .(dia, a, inicio_15)]

# Quais trechos nao foram incorporados com tempo de viagem?

trechos_fora <- setdiff(gtfs_trechos_sf$a, unique(fim_bind$a))

  gtfs_trechos_fora <- gtfs_trechos_sf %>%
  filter(a %in% trechos_fora)

gtfs_trechos_fora %>% st_sf() %>% mapview()

# Qual a velocidade em cada trecho?
summary(fim_bind$velocidade)
boxplot(fim_bind$velocidade)

# Pegar as velocidades outliers
velocidades_absurdas <- fim_bind[velocidade > 60]
velocidades_absurdas <- velocidades_absurdas[!is.infinite(velocidade)]

summary(velocidades_absurdas$velocidade)

# Quais velocidades maior que 200kmh?
velocidades_absurdas %>%
  filter(velocidade > 200) %>%
  st_sf() %>%
  mapView()

# Em quais trechos essas velocidades ocorrem mais?
velocidades_absurdas %>%
  count(a, sort = TRUE) %>%
  View()

velocidades_absurdas %>%
  filter(a == "20-21") %>%
  st_sf() %>%
  mapview()

# Em quais trechos estas ocorrem?
velocidades_absurdas %>%
  distinct(a, geometry) %>%
  st_sf() %>%
  mapview()

# Quais trechos com tempo menor que 0?
tempo_menor_zero <- fim_bind %>%
  filter(tempo_viagem < 0)

# Quantas linhas tem algum tempo menor que 0?
unique(tempo_menor_zero$linha_sentido)

# Quais trechos com maior amostra?
fim_bind %>%
  add_count(a) %>%
  filter(n > 100) %>%
  select(inicio_15, a, tempo_viagem) %>%
  ggplot()+

# Quais os trechos com maior coeficiente de variacao?
fim_bind[, cv := sd/tempo_mediana]

tempo_entre_paradas %>%
  arrange(desc(cv)) %>%
  slice(1:200) %>%
  View()

# Quais os trechos com mediana zero?
trechos_mediana_zero <- tempo_entre_paradas %>%
  filter(tempo_mediana == 0) %>%
  distinct(a)

gtfs_trechos_sf %>%
  filter(a %in% trechos_mediana_zero$a) %>%
  st_sf() %>%
  mapview()


tempo_entre_paradas_agreg <- tempo_entre_paradas[, .(tempo_mediana = median(tempo_mediana, na.rm = TRUE), 
                                          n = .N), 
                                          keyby = .(a, inicio_15)]







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#+ CALCULAR TEMPO ENTRE VIAGENS! -------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# linha <- gps[[1]]$`41`

# gps_linhas <- gps[[4]]

calcular_tempo_entre_viagens <- function(gps_linhas) {
  
  por_linha <- function(linha) {
    
    setDT(linha)
    linha[, linha := str_extract(linha_sentido, "^\\d{1,}")]
    linha[, sentido := str_extract(linha_sentido, "^[[:upper:]]{1}$")]
    # Pegar o inicio e o fim de cada viagem
    gps_viagens <- linha[, .(inicio = first(hora), fim = last(hora), linha_sentido = first(linha_sentido)), 
                         keyby = .(linha, vehicleid, viagem)]
    gps_viagens <- gps_viagens[, tempo_entre_viagens := difftime(inicio, lag(fim), units = "secs"),
                               keyby = .(linha, vehicleid)]
    gps_viagens[, inicio_30 := as.ITime(round_date(inicio, "30 mins"))]
    
  }
  
  # Aplicar
  fim <- map(gps_linhas, por_linha)
  
  fim_bind <- rbindlist(fim)
  
  # Agrupar TODOS os trechos de viagem, em hora pico
  tempo_entre_viagens_total <- fim_bind[, .(tempo_entre_viagens_mediana = median(tempo_entre_viagens, na.rm = TRUE), 
                                            n = .N), 
                                        keyby = .(linha, linha_sentido, inicio_30)]
}

# APLICAR ------------------------------------------------------------------------------------------
gps <- map(gps_files, abrir_gps)

# no windows 
plan(multiprocess)

furrr::future_map_dfr(gps, calcular_tempo_entre_viagens) %>%
  .[, .(tempo_entre_viagens_mediana = median(tempo_entre_viagens_mediana, na.rm = TRUE), 
        n = .N), 
    keyby = .(linha, linha_sentido, inicio_30)] %>%
  write_rds("../data/tempo_entre_viagens_fim_2019-08.rds")

# no linux
parallel::mclapply(gps, calcular_tempo_entre_viagens,
                   mc.cores = 2) %>%
  rbindlist() %>%
  .[, .(tempo_entre_viagens_mediana = median(tempo_entre_viagens_mediana, na.rm = TRUE), 
        n = .N), 
    keyby = .(linha, linha_sentido, inicio_30)] %>%
  write_rds("../data/tempo_trechos/tempo_entre_viagens_agreg.rds")

# # Plotar
# gps_trechos_v2 %>%
#   mutate(trecho_id = as.factor(trecho_id)) %>%
#   filter(linha_sentido == "855-I") %>%
#   st_sf() %>%
#   ggplot()+
#   geom_jitter(aes(x = trecho_id, y = tempo_viagem)) +
#   coord_flip()
# 
# # Agrupando
gps_trechos_v2 %>%
  st_sf() %>%
  filter(!is.na(trecho_id)) %>%
  filter(!is.na(tempo_viagem)) %>%
  group_by(linha_sentido, factor(trecho_id)) %>%
  summarise(tempo_medio = mean(as.numeric(tempo_viagem)),
            dist = first(length)) %>%
  ungroup() %>%
  mutate(velocidade = dist/tempo_medio) %>%
  # filter(linha_sentido == "60-I") %>%
  # mutate(tempo_medio_quartil = list(tibble::enframe(quantile(tempo_medio, probs=c(0.25,0.5,0.75))))) %>%
  # unnest(tempo_medio_quartil) %>%
  # View()
  # st_buffer(0.0005) %>%
  mapview(zcol = "velocidade", lwd = 10)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#+ CORRIGIR O GTFS ---------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# Se houve uma pausa maior que 12 minutos entre uma parada e outra, eh pq houve lanche!
# Se houver troca de motorista, considera o do GTFS como o inicio novamente!

# deletar todas as viagens que terminem dps de 9h!

# OUTRO PONTO: se o veiculo estiver adiantado e tiver uma pausa em seguida, ele pode ficar mais tempo
# que o previsto pela pausa e voltar a rodar no horario programado

# Abrir arquivos
tempo_entre_paradas_total <- read_rds("../data/tempo_entre_paradas_fim_2019-08.rds")
  
tempo_entre_viagens_fim <- read_rds("../data/tempo_entre_viagens_fim_2019-08.rds")

# Abrir GTFS

stop_times <- read_rds("../data/stop_times_tratado/stop_times_tratado.rds")
  # .[linha %nin% expressos_cjao_vector]

trips <- read_delim("../data-raw/gtfs/2018/GTFS_fortaleza_20180907/trips.txt", delim = ",")

stop_times_linhas <- split(stop_times, by = "linha")

# stop_times_linhas_ok <- stop_times_linhas$`54`
# stop_times_linhas_ok <- stop_times_linhas$`333`
# stop_times_linhas_ok <- stop_times_linhas$`383`
# stop_times_linhas_ok <- stop_times_linhas$`360`
# stop_times_linhas_ok <- stop_times_linhas$`72`
# stop_times_linhas_ok <- stop_times_linhas$`51`
# stop_times_linhas_ok <- stop_times_linhas$`456`
# stop_times_linhas_ok <- stop_times_linhas$`648`
# stop_times_linhas_ok <- stop_times_linhas$`814`
# stop_times_linhas_ok <- stop_times_linhas$`712`
# stop_times_linhas_ok <- stop_times_linhas$`41`

# tempo_cenario <- "tempo_p50"
# tempo_cenario <- "tempo_p85"

corrigir_gtfs <- function(stop_times_linhas_ok, tempo_cenario = "tempo_p50") {
  
  # Pegar o inicio e o fim de cada viagem GTFS
  stop_times_linhas_ok[, viagem1 := rleid(viagem)]
  # stop_times_linhas_ok[, hora1 := zoo::na.approx(as.integer(hora), rule = 2), by = viagem1]
  stop_times_linhas_ok[, hora := as.ITime(hora)]
  stop_times_viagens <- stop_times_linhas_ok[, .(inicio = as.ITime(first(hora)), 
                                                 fim = as.ITime(last(hora)), 
                                                 linha_sentido = first(linha_sentido),
                                                 corrida = first(corrida), 
                                                 trip_id = first(trip_id)),
                                             keyby = .(linha, veiculo, viagem1)]
  
  # # Um pequeno interludio: qual o tempo quando o carro muda de motorista? (De B01 para B02)
  # stop_times_viagens %>%
  #   arrange(veiculo, inicio) %>%
  #   # group_by(veiculo, corrida) %>%
  #   # filter(row_number() == 1 | row_number() == n()) %>%
  #   # ungroup() %>%
  #   group_by(veiculo) %>%
  #   mutate(tempo = difftime(inicio,lag(fim), units = "mins")) %>%
  #   View()
  
  # Quais viagens comecaram ate 9h da manha? (considerar isso como hora pico)
  stop_times_viagens_pico <- stop_times_viagens[inicio < as.ITime("09:00:00")]
  
  # Extrair somente as viagens em pico das viagens totais
  stop_times_pico <- stop_times_linhas_ok[trip_id %in% stop_times_viagens_pico$trip_id]
  
  # Tambem separar as nao pico (usar mais na frente)
  # stop_times_npico <- fsetdiff(stop_times_linhas_ok, stop_times_pico)
  
  # Como tem muita hora NA (esta faltando no stop sequence), interpola-las
  stop_times_pico[, hora1 := zoo::na.approx(hora), by = viagem1]
  stop_times_pico[, hora1 := as.ITime(hora1)]
  
  # # Reorganizar
  stop_times_pico <- stop_times_pico %>%
    group_by(veiculo, viagem) %>%
    mutate(trip_start = first(hora1)) %>%
    ungroup() %>%
    arrange(veiculo, trip_start, hora1)
    
  setDT(stop_times_pico)
  
  # Refazer a viagem
  stop_times_pico[, viagem1 := rleid(viagem1)]
  # Estabelecer viagem propria do carro
  stop_times_pico[, viagem_carro := rleid(viagem1), by = veiculo]
  
  # Começar a inputar o tempo entre viagens do gtfs aqui:
  stop_times_pico[, stop_id_anterior := lag(stop_id), by = viagem1]
  stop_times_pico[, a := paste0(stop_id_anterior, "-", stop_id), by = viagem1]
  
  # Criar hora arredondada para poder bater com o arquivo de tempos de viagem
  stop_times_pico[, inicio_15 := as.ITime(round_date(as.POSIXct(hora1), "15 mins"))]
  stop_times_pico[, inicio_30 := as.ITime(round_date(as.POSIXct(hora1), "30 mins"))]
  
  tempo_entre_paradas_v1 <- tempo_entre_paradas_total[, .(inicio_15, a, get(tempo_cenario))]
  setnames(tempo_entre_paradas_v1, old = "V3", new = "tempo_mediana")
  
  
  # Trazer a mediana dos tempos DE viagem!
  stop_times_corrigido <- merge(stop_times_pico, tempo_entre_paradas_v1[, .(inicio_15, a, tempo_mediana)], 
                                by = c("a","inicio_15"),
                                all.x = TRUE, sort = FALSE)
  
  # Trazer a mediana dos tempos ENTRE viagens!
  stop_times_corrigido <- merge(stop_times_corrigido, 
                                tempo_entre_viagens_fim[, .(linha_sentido, inicio_30, tempo_entre_viagens_mediana)], 
                                by = c("inicio_30" ,"linha_sentido"),
                                all.x = TRUE, sort = FALSE)
  
  # Ajeitar ordem
  # stop_times_corrigido1 <- stop_times_corrigido[order(viagem1, stop_sequence)]
  
  # Calcular as novas hora de stop sequence
  stop_times_corrigido[, tempo_mediana := as.ITime(tempo_mediana)]
  # O primeiro horario considerado sera o primeiro do veiculo; a partir disso, somar os tempos acumulados
  # de acordo com o encontrado nos arquivos de GPS
  stop_times_corrigido[, hora_nova := ifelse(seq_len(.N) == 1,
                                             hora1,
                                             tempo_mediana 
  ), 
  by = veiculo]
  
  # Podem acontecer dois tipos de pausa:
  # Lanche: entre 30 e 59 minutos, sagrada;
  # Pausa maior: entre 1 e 2 horas, flexível
  stop_times_corrigido[, hora_lag := hora1 - lag(hora1),
                       by = "veiculo"]
  
  stop_times_corrigido[, pausa := ifelse(between(hora_lag, 1200, 5000) & viagem_carro != lag(viagem_carro),
                                         "sim", 
                                         "nao"),
                       by = veiculo]
  
  # Se for pausa, adicionar o valor da pausa. Senao, estabelecer o valor do tempo entre viagens
  # Se for a primeira parada de uma viagem que nao seja a seegunda e for pausa, adiciona o valor
  # da pausa
  # Se for a primeira parada de uma viagem que nao seja a segunda e nao fora pausa, adicionar o tempo
  # entre viagem daquele intervalo
  stop_times_corrigido[, hora_nova1 := if_else(viagem_carro > 1 & stop_sequence == 1 & pausa == "sim",
                                               as.integer(hora_lag),
                                               if_else(viagem_carro > 1 & stop_sequence == 1 & pausa == "nao",
                                                       as.integer(tempo_entre_viagens_mediana),
                                                       hora_nova)),
                       by = veiculo]
      
  # Se eu nao tiver uma estimativa do tempo de viagem pelo GPS naquele trecho (ou o tempo do GPS for exorbitante)
  # , adotar o tempo
  # previsto no gtfs mesmo!
  stop_times_corrigido[, hora_nova1 := ifelse(is.na(hora_nova1),
                                              as.integer(hora_lag),
                                              ifelse(hora_nova1 > 720 & seq_len(.N) != 1,
                                                     as.integer(hora_lag),
                                                     as.integer(hora_nova1))),
                       by = veiculo]
  
  # Fazer a soma acumulada da hora do comeco da primeira viagem e dos tempos entre viagens acumulados
  stop_times_corrigido[, hora_cum := cumsum(hora_nova1), by = veiculo]
  stop_times_corrigido[, hora_cum := as.ITime(hora_cum)]
  
  # Se for numa pausa, e o veiculo estiver adiantado em relacao ao agendado, o veiculo faz uma pausa
  # maior para se adequar ao horario (o contrario nao eh verdadeiro!)
  # Criar uma dummy para se o veiculo estiver adiantado ou nao
  stop_times_corrigido[, pontualidade := ifelse(difftime(hora_cum, hora1) < 0, 
                                             "adiantado",
                                             "atrasado"),
                       by = veiculo]
  
  # Se o veiculo estiver atrasado e estiver em uma pausa, considerar o valor agendado de viagem!
  stop_times_corrigido[, hora_nova2 := ifelse(pontualidade == "adiantado" & pausa == "sim",
                                              as.integer(difftime(as.ITime(hora1), lag(as.ITime(hora_cum)), units = "secs")),
                                              as.ITime(hora_nova1)),
                       by = veiculo]
  
  # # Se o veiculo fizer uma pausa maior que 1h, considerar que volta o valor agendado da viagem!
  # stop_times_corrigido[, hora_nova2 := ifelse(pontualidade == "adiantado" & pausa == "sim",
  #                                             as.integer(difftime(as.ITime(hora1), lag(as.ITime(hora_cum)), units = "secs")),
  #                                             as.ITime(hora_nova1)),
  #                      by = veiculo]
  
  # Atualizar o valor da hora acumulado
  stop_times_corrigido[, hora_cum2 := cumsum(hora_nova2), by = veiculo]
  # stop_times_corrigido[, hora_cum2 := as.ITime(hora_cum2)]
  
  # Selecionar colunas
  stop_times_corrigido_final <- 
    stop_times_corrigido[, .(trip_id, linha, linha_sentido, veiculo, viagem = viagem1, corrida, 
                             hora_programada = hora1, hora_real = hora_cum2,
                             stop_id, stop_sequence)]
    
  # # RECOMPOR O GTFS -----------------------------------------------------------------------------
  # # Para recompor, eh necessario somar a parte de pico a parte nao pico
  # 
  # stop_times_npico[, hora_real := NA_integer_]
  # stop_times_npico <-
  #   stop_times_npico[, .(trip_id, linha, linha_sentido, veiculo, viagem = viagem1, corrida,
  #                        hora_programada = hora1, hora_real,
  #                        stop_id, stop_sequence)]
  # 
  # end <- rbind(stop_times_npico, stop_times_corrigido_final)
  # 
  # end[, hora_real := as.ITime(hora_real)]
  # 
  # setorder(end, veiculo, viagem)
  # 
  # return(end)
  
  # Fazer correcao do trips.txt, com somente viagens no pico!
  trips_final <- trips %>%
    filter(trip_id %in% stop_times_viagens_pico$trip_id)

  final <- list(stop_times_corrigido_final, trips_final)
  # final <- list(end, trips)
  
}


# Aplicar funcao

corrigir_gtfs_safe <- possibly(corrigir_gtfs, otherwise = NA_real_)

stop_times_corrigido_p50 <- map(stop_times_linhas, corrigir_gtfs_safe, tempo_cenario = "tempo_p50")
stop_times_corrigido_p85 <- map(stop_times_linhas, corrigir_gtfs_safe, tempo_cenario = "tempo_p85")

# arredondar segundos

# tirar na's
vazios_50 <- which(map_lgl(stop_times_corrigido_p50, is.list))
vazios_85 <- which(map_lgl(stop_times_corrigido_p85, is.list))

stop_times_corrigido_50_ok <- stop_times_corrigido_p50[vazios_50]
stop_times_corrigido_85_ok <- stop_times_corrigido_p85[vazios_85]
# map(function(x) x[!is.na(x)])

# # Juntar tanto os stop_times como os trips
gtfs_corrigido_50 <- transpose(stop_times_corrigido_50_ok) %>%
  map(rbindlist)

write_rds(gtfs_corrigido_50, "../data/gtfs_temp_50.rds")

gtfs_corrigido_85 <- transpose(stop_times_corrigido_85_ok) %>%
  map(rbindlist)

write_rds(gtfs_corrigido_85, "../data/gtfs_temp_85.rds")


# abrir ---------------------------------------------------------------------------------------
gtfs_corrigido_50 <- read_rds("../data/gtfs_temp_50.rds")
gtfs_corrigido_85 <- read_rds("../data/gtfs_temp_85.rds")


# Salvar para o formato stop_times e trips ---------------------------------------------------------

# pegar stop_times e selecionar colunas
gtfs_corrigido_50_final <- gtfs_corrigido_50[[1]][, ':='(arrival_time = hora_real, 
                      departure_time = hora_real)] %>%
  .[, ':='(arrival_time = as.ITime(arrival_time), departure_time = as.ITime(departure_time))] %>%
  .[, .(trip_id, arrival_time, departure_time, stop_id, stop_sequence)]

gtfs_corrigido_85_final <- gtfs_corrigido_85[[1]][, ':='(arrival_time = hora_real, 
                      departure_time = hora_real)] %>%
  .[, ':='(arrival_time = as.ITime(arrival_time), departure_time = as.ITime(departure_time))] %>%
  .[, .(trip_id, arrival_time, departure_time, stop_id, stop_sequence)]


# salvar 50 -------------------------------------------------------
gtfs_corrigido_50_final %>%
  write_delim("../data/gtfs/gtfs_corrigido_50/stop_times.txt", delim = ",")

gtfs_corrigido_50[[2]] %>%
  .[, .(route_id, service_id, trip_id, shape_id)] %>%
  write_delim("../data/gtfs/gtfs_corrigido_50/trips.txt", delim = ",")

# Criar zip
# system("cd ../data/gtfs/gtfs_corrigido_50 && zip -r ../gtfs_corrigido_50.zip *")
shell("cd ../data/gtfs/gtfs_corrigido_50 && zip -r ../gtfs_corrigido_50.zip *")



# salvar 85 -------------------------------------------------------
gtfs_corrigido_85_final %>%
  write_delim("../data/gtfs/gtfs_corrigido_85/stop_times.txt", delim = ",")

gtfs_corrigido_85[[2]] %>%
  .[, .(route_id, service_id, trip_id, shape_id)] %>%
  write_delim("../data/gtfs/gtfs_corrigido_85/trips.txt", delim = ",")

# Criar zip
# system("cd ../data/gtfs/gtfs_corrigido_85 && zip -r ../gtfs_corrigido_85.zip *")
shell("cd ../data/gtfs/gtfs_corrigido_85 && zip -r ../gtfs_corrigido_85.zip *")




# Rodar feedvalidator
feed_call <- "feedvalidator.py -o ../data/gtfs/validator_gtfs_corrigido.html ../data/gtfs/gtfs_corrigido.zip"
system(feed_call)

#+ CHECAGEM DOS ERROS DO FEEDVALIDATOR ---------------------------------------------------------
gtfs_corrigido[[1]] %>%
  filter(trip_id == "U086-T02V02B01-I") %>% View()




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#+ VISUALIZACAO --------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("R/themes.R")

# O quanto a hora programada difere da hora real?
gtfs_corrigido_50[[1]] %>%
  mutate(hora_real = as.POSIXct(as.ITime(hora_real))) %>%
  mutate(hora_programada = as.POSIXct(hora_programada)) %>%
  ggplot(aes(x = hora_programada, y = hora_real))+
  geom_density_2d()+
  stat_density2d(aes(fill = stat(level)), geom = "polygon")+
  geom_abline(intercept = 0, slope = 1, color = "red")+
  scale_fill_viridis_c()+
  scale_x_datetime(limits = c(as.POSIXct("06:00:00", format = "%H:%M:%S"), NA))+
  scale_y_datetime(limits = c(as.POSIXct("06:00:00", format = "%H:%M:%S"), NA))+
  labs(x = "Hora programada", y = "Hora real")+
  theme_ipsum_rc()+
  theme(legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "mm"))

ggsave("figure/5-resultado_correcao_gtfs.png", dpi = 300, width = 16, height = 10, units = "cm")

gtfs_corrigido_50[[1]] %>%
  mutate(hora_real = as.POSIXct(as.ITime(hora_real))) %>%
  mutate(hora_programada = as.POSIXct(hora_programada)) %>%
  mutate(dif = hora_real - hora_programada) %>%
  ggplot()+
  geom_boxplot(aes(x = 1, y = dif), outlier.colour=rgb(.5,.5,.5, alpha=0.2))+
  theme_ipsum_rc()+
  labs(y = "Diferença (segundos) entre o horário real e programado")+
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "mm"))

ggsave("figure/5-resultado_correcao_gtfs_bp.png", dpi = 300, width = 16, height = 6, units = "cm")

summary(gtfs_corrigido[[1]]$dif)

  # Tem uns hora_real > "11:00:00" bem estranhos, checar!
gtfs_corrigido[[1]][hora_real > as.ITime("11:00:00")] %>% View()


# Visualizar o tempo de viagem real x tempo de viagem programado de TODOS os trechos
# Tirar NA's (TEM QUE CHECAR DEPOIS!)
stop_times_corrigido_ok <- stop_times_corrigido[!is.na(stop_times_corrigido)] %>%
  rbindlist()

# # Pegar tempos de viagem unicos
# stop_times_corrigido_ok %>%
#   distinct()

# Pegar uma linha
teste_45 <- stop_times_corrigido$`41`

# Pegar uma veiclo (T01)
teste_45 %>%
  filter(veiculo == 'T01') %>%
  # filter(grepl("^V01", viagem)) %>%
  gather(hora_tipo, hora, hora_programada:hora_real) %>%
  ggplot()+
  geom_path(aes(x = stop_sequence, y = as.POSIXct(hora), color = hora_tipo), size = 1) +
  # geom_point(aes(x = stop_sequence, y = as.POSIXct(hora)), size = 1)+
  facet_wrap(~viagem, scale = "free", ncol = 2)+
  # hrbrthemes::theme_ipsum_rc(base_size = 15, axis_title_size = 17)+
  hrbrthemes::theme_ipsum_rc()+
  theme(legend.position = "bottom",
        plot.margin=unit(c(0,0,0,0),"mm"))+
  labs(x = "Sequência de paradas",
       y = "Hora")
  
ggsave("figure/5-correcao_gtfs.png", dpi = 400, units = "cm", width = 16, height = 16)
  


# ui
ai <- read_rds("../data/stop_times_tratado/stop_times_tratado.rds")
ai <- fread("../data-raw/gtfs/2018/GTFS_fortaleza_20180907/stop_times.txt")
ai <- fread("../data-raw/gtfs/2018/GTFS_fortaleza_20180907/trips.txt")

ui <- fread("../data/gtfs/gtfs_corrigido/stop_times.txt")
ui <- fread("../data/gtfs/gtfs_corrigido/trips.txt")

ui %>% filter(grepl("^U657", trip_id)) %>% View()
ai %>% filter(grepl("U626", trip_id)) %>% View()


trips <- fread("../data//")

ui %>% filter(trip_id == "U657-T03V02B01-I")
