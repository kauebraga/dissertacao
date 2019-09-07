library(sp)
library(maptools)
library(rgdal)

points_to_line_sf <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    
    sp_lines <- sp_lines %>%
      st_as_sf(crs = 4326) %>%
      st_set_crs(4326) %>%
      mutate(shape_id = unique(dat$shape_id))
    
    return(sp_lines)
  }
}

dat <- read.csv("../data-raw/gtfs/2018/GTFS_fortaleza_20180907/shapes.txt")

v_lines <- points_to_line_sf(data = dat, 
                             long = "shape_pt_lon", 
                             lat = "shape_pt_lat", 
                             id_field = "shape_id", 
                             sort_field = "shape_pt_sequence")


st_write(v_lines, "../data/linhas/2018/linhas_2018-09.shp")
