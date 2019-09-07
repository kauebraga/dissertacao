Sys.setenv(TZ='UTC') 

options(scipen = 999)


library(zoo)
library(sp)
library(ggplot2)
library(dplyr)
library(sf)
library(fasttime)
# library(mapview)
#library(ggmap) #função geocode() pra extrair as coordenadas dos endereços
library(sf) #pra importar os dados espaciais e tal
library(data.table)
library(knitr)
library(readr)
library(tidyr)
library(hrbrthemes)
library(stringr)
# library(leaflet.minicharts)
library(purrr)
library(lubridate)
library(mapview)
library(RColorBrewer)
library(furrr)
#library(extrafont)
#extrafont::loadfonts(device="win")


#library(htmltools)
#library(htmlwidgets)

#library(tmap)



to_spatial <- function(df1, coordenada = c("lon", "lat")) {
  x <- st_as_sf(df1, coords = coordenada, crs = 4326)
}



# from https://github.com/r-spatial/sf/issues/231


sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  ui <- dplyr::bind_cols(x,ret)
  st_set_geometry(ui, NULL)
}

