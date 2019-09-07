#' This functions snaps a set of points to a given linestring

snap_points_to_line <- function(points, line) {
  
  # alinhar as pradas gps com a linha
  points_align <- st_nearest_points(points, line) %>%
    st_cast("POINT")
  
  # pegar so os pontos pares
  points_new_geometry <- points_align[c(seq(2, length(points_align), by = 2))]
  
  points_align_end <- points %>%
    st_set_geometry(points_new_geometry)
  
}