#' Função para interpolar pontos de GPS para pontos de parada
#' A função pede três argumentos: gps, paradas e linha
#' A linha representa o shape da linha naquele sentido
#' Os dados de GPS precisam ser divididos por sentido e por viagem

interpolar_gps_para_paradas_na_rede <- function(gps, paradas, linha) {
  
  # Ajeitar GPS
  gps_spatial <- gps %>% to_spatial() %>%  st_transform(31984) %>% mutate(id_gps_temp = 1:n())
  
  # Ajeitar linha
  linha <- linha %>% st_transform(31984)
  
  # Funcao para alinhar os pontos  com a linha
  source("R/snap_points_to_line.R")
  
  # Alinha os pontos de gps com a linha
  gps_alinhado <- snap_points_to_line(gps_spatial, linha)
  
  # Transformar o gps de pontos para trechos
  gps_trecho <- gps_alinhado %>%
    st_set_geometry(NULL) %>%
    mutate(id_gps_temp = 1:n()) %>%
    mutate(hora_fim = lead(hora),
           id_gps_temp_fim = lead(id_gps_temp)) %>%
    rename(id_gps_temp_inicio = id_gps,
           hora_inicio = hora) %>%
    slice(-n()) %>%
    mutate(id_trecho = 1:n()) %>%
    mutate(tempo = hora_fim - hora_inicio)
  
  # Adicionar buffer irrisorio aos pontos de GPS para facilitar a etapa seguinte de split
  gps_alinhado_buffer <- st_buffer(gps_alinhado, 2)
  
  # Dividir a linha por trechos a cada ponto de GPS
  linha_trecho <- lwgeom::st_split(linha, gps_alinhado_buffer) %>%
    # Extrair os trechos
    st_collection_extract("LINESTRING") %>%
    # Calcular o tamanho de cada trecho
    mutate(dist = st_length(.) %>% as.numeric()) %>%
    # Extrair os minimos fragmentos
    filter(dist > 2.1) %>%
    # Tirar os dois trechos grandes (primeiro e ultimo)
    slice(-1, -n()) %>%
    # Criar ID para o trecho
    mutate(id_trecho = 1:n()) %>%
    # Trazer de volta informacoes do gps daquele trecho
    left_join(gps_trecho, by = "id_trecho") %>%
    # Selecionar informacoes importantes
    select(id_trecho, hora_inicio, hora_fim, dist, tempo)
  

# PARADAS -------------------------------------------------------------------------------------

  paradas_spatial <- paradas %>% to_spatial() %>% st_transform(31984)
  
  # Alinhar as pradas gps com a linha
  paradas_alinhado <- snap_points_to_line(paradas_spatial, linha)
  
  paradas_alinhado_buffer <- st_buffer(paradas_alinhado, 3)
  
  # identificar de qual ponto de gps a parada esta mais proxima
  # como colocar em ordem do tip
  
  # dividir os trechos de gps pelas paradas
  
  # quebrar os trechos por paradas
  paradas_trecho <- lwgeom::st_split(linha_trecho, paradas_alinhado_buffer)
  
  # FINALLY!!!!!!!!!
  
  FIM_paradas <- st_collection_extract(paradas_trecho, "LINESTRING") %>%
    mutate(distnew = st_length(.) %>% as.numeric()) %>%
    # Extrair os minimos fragmentos
    filter(distnew > 2)
  
  # agora... agrupar por trecho: se a distancia da parada para a primeira parte do trecho (distnew) for menor que
  # a metade da distancia total do trecho (dist), entao a parada esta mais proxima do primeiro ponto
  # ou seja, somar a distancia (interpolar)... se for o contrario, esta mais proxima do ultimo ponto do trecho,
  # entao extrapolar 
  
  FIM_paradas_v1 <- FIM_paradas %>%
    group_by(id_trecho) %>%
    mutate(parada = ifelse(distnew < dist/2, "sim", "nao")) %>%
    ungroup() %>%
    mutate(tempo = as.numeric(tempo)) %>%
    mutate(velocidade = (dist/tempo)) %>%
    # Recalcular o tempo e as horas para os trechos em que ha paradas!
    group_by(id_trecho) %>%
    mutate(tempo = distnew/velocidade) %>%
    mutate(hora_fim = ifelse(row_number() == 1, hora_inicio + hms::hms(seconds = tempo), hora_fim)) %>%
    # Corrigir (nao sei oq houve..)
    mutate(hora_fim = as.POSIXct(hora_fim, origin = "1970-01-01")) %>%
    mutate(hora_inicio = ifelse(row_number() == 2, lag(hora_fim), hora_inicio)) %>%
    # De novo...
    mutate(hora_inicio = as.POSIXct(hora_inicio, origin = "1970-01-01")) %>%
    # Identificar trechos que tem parada
    add_count() %>%
    ungroup() %>%
    group_by(id_trecho) %>%
    mutate(id_trecho_new = ifelse(n == 2, paste0(as.character(id_trecho), ".", c(1, 2)), as.character(id_trecho))) %>%
    ungroup() %>%
    st_sf()
  
  # Agora, separar so os pontos que sao de paradas... colunas: stop_id, hora, lon, lat
  
  FIM_paradas_v2 <- FIM_paradas_v1 %>%
    st_cast("POINT") %>%
    group_by(id_trecho_new) %>%
    slice(1, n()) %>%
    arrange(id_trecho) %>%
    # Selecionar so as paradas e os GPS
    group_by(id_trecho_new) %>%
    slice(1) %>%
    ungroup() %>%
    add_count(id_trecho) %>%
    group_by(id_trecho) %>%
    mutate(tipo = ifelse(n == 2 & row_number() == 2, "parada", "gps")) %>%
    arrange(id_trecho) %>%
    # Tirar o horario inicial
    select(id_trecho, id_trecho_new, hora = hora_inicio, tipo, geometry) %>%
    # Trazer de volta o id do ponto gps (sempre lembrar que o ponto eh o primeiro do trecho...)
    left_join(gps_trecho %>% select(id_gps_temp_inicio, id_trecho), by = "id_trecho") %>%
    # Os pontos de parada nao apresentam id_gps, so..
    mutate(id_gps = ifelse(tipo == "parada", NA, id_gps_temp_inicio)) %>%
    st_sf()
  
  # agora.... a qual stop_id essa parada acima se refere?
  
  # vai ser na ignorancia mesmo...
  FIM_v3 <- FIM_paradas_v2 %>%
    st_join(paradas_alinhado_buffer)
  
}
