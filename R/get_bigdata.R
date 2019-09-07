
# base day is 2015-03-04

get_smartcard <- function(date = "2018-09-20", ...) {
  
  file <- paste0("../data/bilhetagem_integrado/2018/bilhetagemintegrado_", date, ".csv")
  
  smartcard <- readr::read_csv(file, ...)
  
  return(smartcard)
  
}



get_gps <- function(date = "2018-09-20", ...) {
  
  year <- stringr::str_extract(date, "\\d{4}")
  
  mes <- substr(date, 6, 7)
  
  file <- sprintf("../data/gps/%s/novo/%s/gps_%s.csv", year, mes, date)
  
  gps <- fread(file)
  
  gps[, id_gps := 1:nrow(gps)]
  gps[, vehicleid := as.numeric(vehicle_vehicleid)]
  gps[, hora := fasttime::fastPOSIXct(hora, tz="UTC")]
  gps <- gps[, .(id_gps, vehicleid, hora, lon = longitude, lat = latitude)]
  
  return(gps)
  
}
