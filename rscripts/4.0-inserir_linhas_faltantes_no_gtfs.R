# abrir linhas do gabriel

linhas_faltantes <- map_dfr(c("../data/linhas_gabriel_ida.csv", "../data/linhas_gabriel_volta.csv"), read_delim, delim = ";")


# PASSOS PARA ADICIONAR UMA NOVA LINHA NO ARQUIVO DE GTFS -------------------------------------

# 1) Adicionar informacoes sobre a linha no arquivo routes.txt (route_id, .., route_short_name, route_long_name)
# 2) Adicionar o arquivo shapes.txt (quebrar a linha em diversos pontos, garantido que todas as paradas sejam pontos);

# Apos isso:
# - Adicionar as linhas faltante aos arquivos de linhas_2018 (rodar a funcao do arquivo analysis/salvar_linhas_2018.R)
# - Criar novamente o arquivo paradas_consolidadas

# 3) Adicionar informacoes ao arquivo trips.txt


# OUTRO METODO: USANDO DADOS DE 2015 ----------------------------------------------------------

# abrir trips, shapes, e stop_times de 2015

trips <- fread("../data-raw/gtfs/2015/trips.txt")
trips[, sentido := str_extract(trip_id, "[[:upper:]]$")]
trips[, linha := substr(trip_id, 2, 4)]
trips[, linha := as.numeric(linha)]
trips[, linha_sentido := paste0(linha, "-", sentido)]

shapes <- fread("../data-raw/gtfs/2015/shapes.txt") %>%
  mutate(linha = str_extract(shape_id, "\\d{3}")) %>%
  mutate(linha = as.integer(linha)) %>%
  mutate(sentido = str_sub(shape_id, -1, -1)) %>%
  mutate(linha_sentido = paste0(linha, "-", sentido))

stop_times <- fread("../data-raw/gtfs/2015/stop_times.txt")
stop_times[, sentido := str_extract(trip_id, "[[:upper:]]$")]
stop_times[, linha := substr(trip_id, 2, 4)]
stop_times[, linha := as.numeric(linha)]
stop_times[, linha_sentido := paste0(linha, "-", sentido)]

# linhas faltantes
linhas_faltante <- fread("../data/linhas_faltante.csv") %>%
  rename(linha_sentido = ".")

# Filtrar as linhas faltantes nos 3 arquivos!

trips_faltantes <- trips %>% filter(linha_sentido %in% linhas_faltante$linha_sentido)

shapes_faltantes <- shapes %>% filter(linha_sentido %in% linhas_faltante$linha_sentido)

stop_times_faltantes <- stop_times %>% filter(linha_sentido %in% linhas_faltante$linha_sentido)

# Checar se os stops continuam os mesmos

stops <- fread("../data-raw/gtfs/2018/GTFS_fortaleza_20180907/stops.txt")

setdiff(unique(stop_times_faltantes$stop_id), unique(stops$stop_id))

# testar 52
stop_times_faltantes %>%
  filter(linha_sentido == "52-V") %>%
  distinct(stop_id) %>%
  left_join(stops %>% select(stop_id, stop_lon, stop_lat)) %>%
  filter(!is.na(stop_lon)) %>%
  to_spatial(c("stop_lon", "stop_lat")) %>%
  mapview()

# testar 24
stop_times_faltantes %>%
  filter(linha_sentido == "24-I") %>%
  distinct(stop_id) %>%
  left_join(stops %>% select(stop_id, stop_lon, stop_lat)) %>%
  filter(!is.na(stop_lon)) %>%
  to_spatial(c("stop_lon", "stop_lat")) %>%
  mapview()

