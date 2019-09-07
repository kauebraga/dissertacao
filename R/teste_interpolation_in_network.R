

# oooi ----------------------------------------------------------------------------------------

# GPS Points
gps_points <- structure(list(id_gps = c(4138176L, 4136334L, 4134534L, 4132685L, 
                                        4130891L, 4129035L, 4127232L, 4125387L, 4123620L, 4121861L, 4120114L, 
                                        4118381L, 4116721L, 3380373L, 3374532L, 3369036L, 3363258L, 3357540L, 
                                        3351543L, 3345549L, 3339777L, 3333210L, 3326793L, 3319251L, 3312822L, 
                                        3306501L), hora = structure(c(1535953786, 1535953816, 1535953846, 
                                                                      1535953876, 1535953906, 1535953936, 1535953966, 1535953996, 1535954026, 
                                                                      1535954056, 1535954086, 1535954116, 1535954146, 1535954176, 1535954206, 
                                                                      1535954236, 1535954266, 1535954296, 1535954326, 1535954356, 1535954386, 
                                                                      1535954416, 1535954446, 1535954476, 1535954506, 1535954536), class = c("POSIXct", 
                                                                                                                                             "POSIXt"), tzone = "UTC"), lon = c(-38.500763, -38.501413, -38.50252, 
                                                                                                                                                                                -38.503505, -38.504694, -38.505441, -38.506651, -38.507328, -38.507965, 
                                                                                                                                                                                -38.509063, -38.509735, -38.51022, -38.511546, -38.511778, -38.512788, 
                                                                                                                                                                                -38.513633, -38.514568, -38.51495, -38.515331, -38.515878, -38.516438, 
                                                                                                                                                                                -38.516628, -38.517129, -38.517651, -38.518056, -38.518358), 
                             lat = c(-3.80892, -3.807633, -3.805113, -3.802854, -3.800343, 
                                     -3.79881, -3.796178, -3.79474, -3.793426, -3.791048, -3.789561, 
                                     -3.78856, -3.78569, -3.785216, -3.783108, -3.781245, -3.778751, 
                                     -3.777118, -3.775673, -3.773774, -3.771845, -3.771159, -3.769336, 
                                     -3.767198, -3.765478, -3.764019)), row.names = c(NA, -26L
                                     ), class = "data.frame", .Names = c("id_gps", "hora", "lon", 
                                                                         "lat"))

gps_spatial <- gps_points %>% to_spatial() %>%  st_transform(31984) %>% mutate(id_gps_temp = 1:n())




# Stops
stops <- structure(list(stop_id = c(4873, 3215, 5083, 3346, 3363, 3362, 
                                    3542, 3543, 3540, 4629, 3528), lon = c(-38.516766, -38.515311, 
                                                                           -38.513903, -38.512154, -38.511001, -38.509844, -38.508943, -38.50816, 
                                                                           -38.507062, -38.505798, -38.504044), lat = c(-3.771828, -3.77695, 
                                                                                                                        -3.781432, -3.785157, -3.787631, -3.790069, -3.791997, -3.793663, 
                                                                                                                        -3.796027, -3.798711, -3.802504)), class = c("tbl_df", "tbl", 
                                                                                                                                                                     "data.frame"), row.names = c(NA, -11L), .Names = c("stop_id", 
                                                                                                                                                                                                                        "lon", "lat"))


# Linha
linha <- st_read("../data-raw/linhas_fortaleza_renan/SIT-FOR24052017.shp", crs = 4326) %>%
  select(ROUTE_NAME) %>%
  filter(ROUTE_NAME == "026 - (Volta)") %>%
  st_transform(31984)

# alinhar os pontos de gps com a linha
source("R/snap_points_to_line.R")

oiii_egua <- snap_points_to_line(gps_spatial, linha)

# oooi_fim <- st_nearest_points(gps_spatial, linha) %>%
#   st_cast("POINT")
# 
# # pegar so os pontos pares
# oooi_geometry <- oooi_fim[c(seq(2, length(oooi_fim), by = 2))]
# 
# oiii_egua <- gps_spatial %>%
#   st_set_geometry(oooi_geometry)

oiii_egua_trecho <- oiii_egua %>%
  st_set_geometry(NULL) %>%
  mutate(id_gps_temp = 1:n()) %>%
  mutate(hora_fim = lead(hora),
         id_gps_temp_fim = lead(id_gps_temp)) %>%
  rename(id_gps_temp_inicio = id_gps,
         hora_inicio = hora) %>%
  slice(-n()) %>%
  mutate(id_trecho = 1:n()) %>%
  mutate(tempo = hora_fim - hora_inicio)

# mapview(oiii_egua) + mapview(linha)

# # ou
# oiii_egua_v1 <- st_snap(oiii_egua, linha, tol=150)
# # ou
# oiii_egua_v1 <- st_set_precision(oiii_egua, 0.01)
# ou
oiii_egua_v1 <- st_buffer(oiii_egua, 1)

# mapview(linha)

# mapview(oiii_egua_v1) + mapview(linha)


st_intersects(oiii_egua_v1, linha)

# quebrar a linha por trechos entre pontos de gps
linha_trechos <- lwgeom::st_split(linha, oiii_egua_v1)

# FINALLY!!!!!!!!!

FIM <- st_collection_extract(linha_trechos, "LINESTRING") %>%
  mutate(dist = st_length(.) %>% as.numeric()) %>%
  # Extrair os minimos fragmentos
  filter(dist > 2) %>%
  # Tirar os dois trechos grandes (primeiro e ultimo)
  slice(-1, -n()) %>%
  mutate(id_trecho = 1:n())

# juntar com os trechos do gps

FIM_com_gps <- FIM %>%
  left_join(oiii_egua_trecho, by = "id_trecho") %>%
  select(id_trecho, hora_inicio, hora_fim, dist, tempo)


mapview(FIM) + 
  oiii_egua_v1 %>% mapview()

# transformar para pontos e extrair so o primeiro e o ultino
FIM_pontos <- FIM_com_gps %>%
  st_cast("POINT") %>%
  group_by(id_trecho) %>%
  slice(1, n())

mapview(FIM_pontos[1:3,])

# adicionar o id do gps


# e as paradas?

stops_spatial <- stops %>% to_spatial() %>% st_transform(31984)
# stops_spatial_teste <- stops %>% to_spatial() %>% st_transform(31984) %>% sfc_as_cols()
# gps_spatial_teste <- gps_spatial %>% sfc_as_cols()

# alinhar as pradas gps com a linha

paradas_egua <- snap_points_to_line(stops_spatial, linha)

# paradas_alinhadas <- st_nearest_points(stops_spatial, linha) %>%
#   st_cast("POINT")
# 
# mapview(paradas_alinhadas) + mapview(linha) + mapview(oiii_egua_v1) + mapview(gps_spatial)
# 
# # pegar so os pontos pares
# paradas_geometry <- paradas_alinhadas[c(seq(2, length(paradas_alinhadas), by = 2))]
# 
# paradas_egua <- stops_spatial %>%
#   st_set_geometry(paradas_geometry)

paradas_egua_buffer <- st_buffer(paradas_egua, 3)

# paradas_egua_vai <- paradas_egua %>% sfc_as_cols()

mapview(paradas_egua)

mapview(paradas_egua) + mapview(oiii_egua) 

# identificar de qual ponto de gps a parada esta mais proxima
# como colocar em ordem do tip

# dividir os trechos de gps pelas paradas

# quebrar os trechos por paradas
paradas_trechos <- lwgeom::st_split(FIM_com_gps, paradas_egua_buffer)

# FINALLY!!!!!!!!!

FIM_paradas <- st_collection_extract(paradas_trechos, "LINESTRING") %>%
  mutate(distnew = st_length(.) %>% as.numeric()) %>%
  # Extrair os minimos fragmentos
  filter(distnew > 2)

mapview(FIM_paradas) + mapview(parada_egua) + mapview(oiii_egua)

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
  left_join(oiii_egua_trecho %>% select(id_gps_temp_inicio, id_trecho), by = "id_trecho") %>%
  # Os pontos de parada nao apresentam id_gps, so..
  mutate(id_gps = ifelse(tipo == "parada", NA, id_gps_temp_inicio)) %>%
  st_sf()

mapview(FIM_paradas_v2, zcol = "tipo")

# agora.... a qual stop_id essa parada acima se refere?

# vai ser na ignorancia mesmo...

mapview(FIM_paradas_v2, zcol = "tipo") + mapview(paradas_egua_buffer)

FIM_v3 <- FIM_paradas_v2 %>%
  st_join(paradas_egua_buffer)

mapview(FIM_v3, zcol = "tipo")
