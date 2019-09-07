
st_layers("../data-raw/fortaleza_export.pbf")

for_osm <- st_read("../data-raw/fortaleza_export.pbf", layer = "multipolygons") %>%
  filter(grepl("terminal", name, ignore.case = TRUE)) %>%
  filter(grepl("integração papicu|messejana|lagoa|parangaba|siqueira|conjunto ceará|antônio bezerra", name, ignore.case = TRUE)) %>%
  filter(amenity == "bus_station") %>%
  mutate(terminal = c("cj_ceara", "lagoa", "parangaba", "messejana", "papicu", "siqueira", "ant_bezerra"))

for_osm %>% select(terminal) %>% write_rds("../data/terminais.rds")

mapview(for_osm)
