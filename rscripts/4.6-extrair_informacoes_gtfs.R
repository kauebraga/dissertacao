#+ setup, echo = TRUE, warning=FALSE, message=FALSE, error=FALSE ----------------------------------
source("R/setup.R")
library(tidytransit)
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE, eval = FALSE)


#+ TRATAR_STOP_TIMES ---------------------------------------------------------------------------
# Tratar arquivo stop times para se adequar a programacao de viagens

# Abrir GTFS stop.times
stop_times <- read_delim("../data-raw/gtfs/2018/GTFS_fortaleza_20180907/stop_times.txt", delim = ",")
stop_times <- setDT(stop_times)[, .(trip_id, arrival_time, stop_id, stop_sequence)]

# Abrir trips
trips <- read_delim("../data-raw/gtfs/2018/GTFS_fortaleza_20180907/trips.txt", delim = ",")
# Selecionar so os dias da semana e colunas necessarias
trips <- setDT(trips)[service_id == "U", .(trip_id, route_id, shape_id)]

# Juntar os dois
stop_times_v1 <- merge(stop_times, trips, by = "trip_id")

# Adicionar coluna referente ao carro
stop_times_v1[, veiculo := str_extract(trip_id, "T\\d{2}")]
stop_times_v1[, viagem := str_extract(trip_id, "V\\d{2}")]
stop_times_v1[, corrida := str_extract(trip_id, "B\\d{2}")]
# Adicionar coluna linha_sentido
stop_times_v1[, sentido := str_extract(trip_id, "[[:upper:]]$")]
stop_times_v1[, viagem := paste0(viagem, "-", sentido)]
stop_times_v1[, linha := as.numeric(route_id)]
stop_times_v1[, linha_sentido := paste0(linha, "-", sentido)]
# Selecionar colunas
stop_times_v2 <- stop_times_v1[, .(trip_id, linha, linha_sentido, veiculo, viagem, corrida, hora = arrival_time, stop_id, stop_sequence)]

write_rds(stop_times_v2, "../data/stop_times_tratado/stop_times_tratado.rds")


#+ Abrir GTFS

gtfs_for <- read_gtfs("../data-raw/gtfs/2018/GTFS_fortaleza_20180907.zip")


# Frequencia
oi <- get_route_frequency(gtfs_for, start_hour = 6, end_hour = 9)
oi[["."]][["routes_frequency"]] %>% View()
oi[["."]][["stops_frequency"]] %>% View()


# Abrir stoptimes
stop_times <- read_delim("../data-raw/gtfs/2018/GTFS_fortaleza_20181120/stop_times.txt", delim = ",")

# Qual a quantidade de partidas de cada linha?
partidas_linhas <-   stop_times %>%
  count(trip_id) %>%
  ungroup() %>%
  left_join(gtfs_for$trips %>% select(trip_id, route_id, shape_id), by = "trip_id") %>%
  # mutate(sentido = str_extract(trip_id, "[[:upper:]]$")) %>%
  # mutate(linha = as.numeric(route_id)) %>%
  # mutate(linha_sentido = paste0(linha, "-", sentido)) %>%
  count(shape_id, sort = TRUE)

# Comparar com GPS
gps <- read_rds("../data/tempo_entre_paradas/tempo_entre_paradas_2018-09-03_teste.rds")

# ui <- lapply(gps, function(x) x[!is.na(x)]) %>%
#   # juntar todas os carros! (also check https://www.brodrigues.co/blog/2017-03-24-lesser_known_purrr/)
#   map_depth(1, bind_rows)

# PEgar uma linha para testar
teste <- ui$`75` %>%
  count(linha_sentido, vehicleid, viagem) %>%
  ungroup() %>%
  count(linha_sentido, vehicleid) %>%
  group_by(linha_sentido) %>%
  summarise(viagens = sum(n))

teste_41

# Pegar a 41 no gtfs
viagens_gtfs <- stop_times %>%
  # mutate(departure_time = as.character(departure_time)) %>%
  group_by(trip_id) %>%
  summarise(n = n()) %>%
  # summarise(n = n(), inicio = first(departure_time)) %>%
  ungroup() %>%
  inner_join(gtfs_for$trips %>% filter(service_id == "U") %>% select(trip_id, route_id), by = "trip_id") %>%
  mutate(sentido = str_extract(trip_id, "[[:upper:]]$")) %>%
  mutate(linha = as.numeric(route_id)) %>%
  mutate(linha_sentido = paste0(linha, "-", sentido)) %>%
  # filter(linha_sentido == "45-I") %>%
  count(linha_sentido, sort = TRUE)
  # arrange(inicio) %>%
  # mutate(inicio = as.ITime(inicio)) %>%
  # mutate(headway = inicio - lag(inicio))

  # O que eh S041-T02V01B01-I?

stop_times %>%
  filter(trip_id %in% "S041-T02V01B01-I") %>%
  View()

stop_times %>% View("41")


#' Checar na base da bilhetagem o tanto de carro de cada linha (o real), e comparar com o obtido
#' pelo GPS

#+ comparar_carros_bilhetagem_e_gps --------------------------------

bi <- read_csv("../data/bilhetagem/2018/09/bilhetagem_2018-09-03.csv")

# Tirar corujoes e expressos
out <- c(expresos_cjao$linha, 61, 63, 65, 665, 619)

# Quantos carros rodaram de acordo com a bilhetagem?
bilhetagem_carros <- bi %>%
  count(linha, nome_linha, prefixo_carro) %>%
  count(linha, nome_linha) %>%
  mutate(linha = as.numeric(linha)) %>%
  filter(linha %nin% out)

# Quantas viagens foram realizadas de acordo com a bilhetagem
bilhetagem_viagens <- bi %>%
  arrange(linha, prefixo_carro, hora) %>%
  group_by(linha, prefixo_carro) %>%
  mutate(viagem = rleid(sentido_viagem)) %>%
  ungroup() %>%
  count(linha, sentido_viagem, prefixo_carro, viagem) %>%
  ungroup() %>%
  count(linha, sentido_viagem, prefixo_carro) %>%
  ungroup() %>%
  group_by(linha, sentido_viagem) %>%
  summarise(total = sum(n))

# Quantos carros foram coletados no gps?
abrir_gps <- function(gps_path) {
  
  gps <- read_rds(gps_path)
  # Tirar linhas que nao tem registro
  vazios <- map_lgl(gps, function (x) length(x) > 0)
  gps <- gps[vazios]
  ui <- map(gps, as.data.table)
  
}

gps <- abrir_gps("../data/tempo_entre_paradas/tempo_entre_paradas_2018-09-03_teste.rds")

gps_carros <- gps %>%
  rbindlist() %>%
  mutate(linha = str_extract(linha_sentido, "^\\d+")) %>%
  count(linha, vehicleid) %>%
  count(linha) %>%
  mutate(linha = as.numeric(linha))
  

# gps_carros <- gps %>%
#   map(length) %>%
#   tibble::enframe(name = "linha", value = "n") %>%
#   mutate(linha = as.numeric(linha)) %>%
#   mutate(n = as.numeric(n))

# Juntar os dois
comparacao_carros <- bilhetagem_carros %>%
  full_join(gps_carros, by = "linha", suffix = c(".bilhetagem", ".gps")) %>%
  mutate(dif = n.bilhetagem - n.gps)

ggplot()+
  geom_histogram(data = comparacao_carros, aes(dif))+
  theme_ipsum(grid = "Y")+
  labs(title = "Histograma da diferença entre a quantidade de veículos da frota real e da frota tratada pelo GPS",
       subtitle = "Dias úteis de Setembro/2019",
       y = "Diferença entre veículos",
       x = "Frequência")

# Numero de veiculos total que foram estimados no gps

sum(comparacao_carros$n.gps, na.rm = TRUE)  / sum(comparacao_carros$n.bilhetagem) 

#' Fontes principais de perda de dados de GPS:
#' 
#' - Nao havia a correspondencia do carro nas duas bases
#' - Nao havia informacoes da linha no arquivo GTFS
#' 
#' Ideia sobre o que fazer com os dados de GPS:
#' 
#' - Identificar quais veiculos da bilhetagem estao faltando no gps tratado;
#' - Ver quais as viagens esse veiculos realizam;
#' - Preencher a viagem desses veiculos com o tempo medio do gps;Altíssima filosofia de botequim. Um frequentador do Bar Madrid sugere que contratem alguém tocando castanholas para alegrar o ambiente. Meu amigo Felipinho, um dos donos, responde:
#' 
#' IDEIA DE COMO ATUALIZAR O ARQUIVO DE GTFS:
#' 
#' Para isso, é levantada a hipótese de o arquivo GTFS retrata fielmente a oferta do transporte público,
#' no sentido de que as informações de quantidades de viagens realizadas por dia são confiáveis. Para testar
#' isso, comparar com a quantidade de viagens obtida pela bilhetagem (ai tem o agravante da viagem na bilhetagem
#' ser registrada de modo manual) e também com a quantidade de viagens obtidas pelo gps (ai tem o agravante de algumas
#' linhas não terem todos os carros estimados).
#' 
#' Os passos sugeridos são:
#' 
#' - Identificar os veiculos no arquivo stop_times do GTFS (eh o T.. da coluna trip_id)
#' - Pegar somente as primeiras viagens que acontecem no pico manha (6h a 9h), e assumir o inicio daquelas viagens como verdadeiro
#' - Somar, entao o tempo médio estimado pelo GPS entre as paradas dessa linha até que se complete as viagens
#' 
#' Problemas desse método:
#' 
#' - Alguns carros fazem uma longa pausa entre algumas viagens (eh o B.. da coluna trip_id);
#' - Identificar quando acontecem essas pausas, e, se o tempo somado do tempo de viagem pelo GPS
#' até aquele momento for menor que o começo da primeira viagem após a pausa, assumir que esse começo
#' de acordo com o GTFS é o verdadeiro.
#' 
#' FAZER UMA SERIE TEMPORAL COMPARANDO O TEMPO DE VIAGEM DO GTFS E DO GPS ----------------------



#+ comparar_gtfs_com_gps -----------------------------------------------------------------------

# GTFS

stop_times <- read_delim("../data-raw/gtfs/2018/GTFS_fortaleza_20181120/stop_times.txt", delim = ",")
trips <- read_delim("../data-raw/gtfs/2018/GTFS_fortaleza_20181120/trips.txt", delim = ",") %>%
  filter(service_id == "U") %>% select(trip_id, route_id, shape_id)

viagens_gtfs <- copy(stop_times)
setDT(viagens_gtfs)[, departure_time := fastPOSIXct(paste0("2018-09-03 ", departure_time))]
viagens_gtfs <- viagens_gtfs[, .(inicio = first(departure_time), fim = last(departure_time)), by = trip_id]
viagens_gtfs[, tempo_viagem := difftime(fim, inicio, units = "mins")]
viagens_gtfs[, tempo_viagem := as.numeric(tempo_viagem)]
viagens_gtfs <- merge(viagens_gtfs, trips, all = FALSE)
viagens_gtfs_45 <- viagens_gtfs[shape_id %in% c("shape041-I", "shape041-V")]
viagens_gtfs_45[, veiculo := str_extract(trip_id, "T\\d{2}")]
# viagens_gtfs_45[, inicio_15_minutos := lubridate::hour(inicio)]
# viagens_gtfs_45 <- viagens_gtfs_45[order(inicio)]

# viagens_gtfs_45_agrupadas <- viagens_gtfs_45[, .(tempo_viagem_media = mean(tempo_viagem)), keyby = .(inicio_15_minutos)]
# vai <- viagens_gtfs_45_agrupadas[tempo_viagem_media > 0]

# GPS

gps <- read_rds("../data/tempo_entre_paradas/tempo_entre_paradas_2018-09-03_teste.rds")

# ui <- lapply(gps, function(x) x[!is.na(x)]) %>%
#   # juntar todas os carros! (also check https://www.brodrigues.co/blog/2017-03-24-lesser_known_purrr/)
#   map_depth(1, bind_rows)

# PEgar uma linha para testar
teste <- gps$`41` %>%
  filter(linha_sentido %in% c("41-I", "41-V")) %>%
  mutate(viagem = rleid(viagem)) %>%
  group_by(linha_sentido, vehicleid, viagem) %>%
  summarise(inicio = first(hora), fim = last(hora)) %>%
  mutate(tempo_viagem = difftime(fim, inicio, units = "mins")) %>%
  arrange(vehicleid, inicio)
  # mutate(inicio_15_minutos = lubridate::hour(inicio)) %>%
  # group_by(inicio_15_minutos) %>%
  # summarise(tempo_viagem_media = mean(tempo_viagem)) %>%
  # select(inicio_15_minutos, tempo_viagem_media)

# Juntar
comparacao_tempos <- vai %>%
  left_join(teste, by = "inicio_15_minutos", suffix =c(".gtfs", ".gps") ) %>%
  gather(key = "tipo", value = "tempo_de_viagem_medio", tempo_viagem_media.gtfs, tempo_viagem_media.gps)


ggplot()+
  geom_line(data = comparacao_tempos, aes(x = inicio_15_minutos, y = tempo_de_viagem_medio, colour = tipo) )
  # ylim(c(0, NA))
