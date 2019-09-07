source("R/setup.R")

# Abrir arquivo gps

gps <- read_rds("../data/gps_viagens/gps_viagens_2018-09-04.rds")

# ABRIR_GTFS -------------------------------------------------------------------------------

stop_times <- read_rds("../data/stop_times_tratado/stop_times_tratado.rds")

#' Calcular esses indicadores para hora pico manha (6h - 9h)

# SCHEDULE ADHERENCE --------------------------------------------------------------------------

# No caso, vai ser so pro fim e começo de viagem, porque nao ha disponivel tempo agendado entre paradas
# Identificar a relacao dos carros no gps e no gtfs

# Pegar linha 45 de teste
gps_45 <- gps$`72`
gps_45[, linha := str_extract(linha_sentido, "^\\d{1,}")]
gps_45[, sentido := str_extract(linha_sentido, "^[[:upper:]]{1}$")]

# Pegar o inicio e o fim de cada viagem
gps_45_viagens <- gps_45[, .(inicio = first(hora), fim = last(hora), linha_sentido = first(linha_sentido)), keyby = .(linha, vehicleid, viagem)]
# Pegar so hora pico (ultima viagem terminando 9h)
# gps_45_viagens <- gps_45_viagens[fim < as.POSIXct("2018-09-03 09:00:00")]
# Pegar a primeira viagem de cada veiculo
gps_45_viagens_primeira <- gps_45_viagens[, .(inicio = first(inicio), linha_sentido = first(linha_sentido), n = .N), keyby = .(linha, vehicleid)]
gps_45_viagens_primeira <- gps_45_viagens_primeira[order(inicio)]

stop_times_teste <- stop_times[linha == "72"]
stop_times_teste <- stop_times_teste[!is.na(hora)]
stop_times_teste <- stop_times_teste[, hora := fastPOSIXct(paste0("2018-09-03 ", hora))]
# Probleminha aqui! Checar veiculo T05
setorder(stop_times_teste, veiculo, hora, -stop_sequence)
stop_times_teste[, viagem1 := rleid(linha_sentido), by = veiculo]

# Pegar o inicio e fim da primeira viagem de cada carro do gtfs
stop_times_viagens <- stop_times_teste[, .(inicio = first(hora), fim = last(hora), linha_sentido = first(linha_sentido)), 
                                       keyby = .(linha, veiculo, viagem1)]
# stop_times_viagens <- stop_times_viagens[fim < as.POSIXct("2018-09-03 09:00:00")]
stop_times_viagens_primeira <- stop_times_viagens[, .(inicio = first(inicio), linha_sentido = first(linha_sentido), n = .N), 
                                                  keyby = .(linha, veiculo)]
stop_times_viagens_primeira <- stop_times_viagens_primeira[order(inicio)]


# Juntar os dois!!!! (https://stackoverflow.com/questions/21885290/data-table-roll-nearest-returns-multiple-results)
setkey(gps_45_viagens_primeira, linha_sentido, inicio)
setkey(stop_times_viagens_primeira, linha_sentido, inicio)

# Fazer juncao pelo mesmo sentido e pelo horario mais proximo
ok <- stop_times_viagens_primeira[gps_45_viagens_primeira, roll = "nearest"]

# Qual foi o horario mais proximo?
ok_v1 <- merge(ok, stop_times_viagens_primeira[, .(veiculo, inicio)], 
               by = "veiculo", 
               suffixes = c(".gps", ".gtfs"))

ok_v1 <- unique(ok_v1, by = "veiculo")




# Se tiver dois matchs, selecionar o de menor diferenca de tempo

#' Para escolher os horarios de pico, identificar o primeiro grande gap entre viagens!

# Juntar entao os horarios reais (gps) com os horarios programados (gtfs) <- pontualidade!

# Primeiro jogar a coluna de correspondencia pro gtfs!
correspondencia <- ok_v1[, .(veiculo, vehicleid)]


stop_times_viagens_novo <- merge(stop_times_viagens, correspondencia, 
                                 by = "veiculo",  
                                 all.x = TRUE)


pontualidade <- merge(gps_45_viagens, stop_times_viagens_novo[, .(vehicleid, veiculo, viagem = viagem1, inicio, fim)], 
                      all.x = TRUE, 
                      by = c("vehicleid", "viagem"),
                      suffixes = c(".gps", ".gtfs"))

# Pegar pico!
pontualidade_pico <- pontualidade[inicio.gps < as.POSIXct("2018-09-03 09:00:00")]
# Adicionar minutos atrasado no fim de cada viagem
pontualidade_pico[, diff_fim := difftime(fim.gps, fim.gtfs, units = "mins"), by = "vehicleid"]



# CONSISTENCIA DE HEADWAY ---------------------------------------------------------------------

# Qual o headway planejado? (a partir do inicio da viagem - primeira parada)
setorder(stop_times_viagens, linha_sentido, inicio)
stop_times_headway <- stop_times_viagens[, headway := inicio - lag(inicio), by = linha_sentido]

# Qual o headway real? (a partir do inicio da viagem - primeira parada)
setorder(gps_45_viagens, linha_sentido, inicio)
gps_45_headway <- gps_45_viagens[, headway := difftime(inicio, lag(inicio), units = "mins"), by = linha_sentido]

#' Antes de fazer o headway real da linha eh necessario ver se ficou algum carro de fora daquela linha,
#' fazendo isso atraves da comparacao com a base da bilhetagem... E ver se ficou algum de carro de fora
#' EM HORA PICO!
#' 
#' Se ficou, eh mto provavel que seja melhor descartar aquela linha, visto que o headway vai ficar maior
#' 
#' Manter somente as linhas que tiveram todos os carros estimados no gps no horário de pico
#' 
#' Outra questao importante: em relação a qual parada será calculado o headway? Seria interessante escolher
#' três paradas (início, parada intermediária, fim)?

# Quais linhas tiveram todos os carros estimados no GPS? Comparacao com a bilhetagem
# Para a bilhetagem
bi <- read_csv("../data/bilhetagem_integrado/2018/bilhetagemintegrado_2018-09-03.csv")

bi_hora_pico <- setDT(bi)[hora < as.POSIXct("2018-09-03 10:00:00")]
bi_hora_pico_linha <- bi_hora_pico[, .(n = .N), keyby = .(linha, vehicleid)]
bi_hora_pico_linha <- bi_hora_pico_linha[linha == "45"]
bi_hora_pico_linha <- bi_hora_pico_linha[, .(n = .N), keyby = .(linha)]

# Para o GPS
gps_45_pico <- gps_45_viagens[inicio < as.POSIXct("2018-09-03 10:00:00")]
gps_45_pico <- gps_45_pico[, (n = .N), keyby = .(linha, vehicleid)]
gps_45_pico <- gps_45_pico[, (n = .N), keyby = .(linha)]
