# abrir paradas

stops <- read_delim("../data-raw/gtfs/2018/GTFS_fortaleza_20180907/stops.txt", delim=",") %>% 
  select(stop_id, stop_name, lon = stop_lon, lat = stop_lat)

stops_sf <- stops %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  %>%
  mutate(id_stop = 1:n())

# abrir stop times

stop_times <- read_delim("../data-raw/gtfs/2018/GTFS_fortaleza_20180907/stop_times.txt", delim = ",") %>%
  select(trip_id, stop_id, arrival_time, departure_time, stop_sequence)

# abrir trips

trips <- read_delim("../data-raw/gtfs/2018/GTFS_fortaleza_20180907/trips.txt", delim = ",") %>%
  select(trip_id, shape_id)

stops_linhas_vai <- stop_times %>%
  left_join(trips, by = "trip_id") %>%
  count(trip_id, shape_id) %>%
  group_by(shape_id) %>%
  slice(which.max(n))

stops_linhas <- stop_times %>%
  filter(trip_id %in% stops_linhas_vai$trip_id) %>%
  left_join(trips, by = "trip_id") %>%
  mutate(linhaa = as.integer(str_sub(shape_id, 6, 8))) %>%
  mutate(linha_sentido = paste(linhaa, stringr::str_sub(shape_id, 9, 19), sep = "")) %>%
  left_join(stops, by = "stop_id")  %>%
  select(stop_id, linha_sentido, stop_name, stop_sequence, lon, lat)

write_csv(stops_linhas, "../data/paradas_consolidadas/paradas_consolidadas_2018-09.csv")
