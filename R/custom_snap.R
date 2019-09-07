custom_snap <- function(line, points, tolerance, crs = 29194) {
  
  points <- points %>% mutate(id = 1:n())
  # primeiro, tem que tirar o primeiro e ultimo ponto!
  points_extremo <- points %>% slice(1, n())
  points <- points %>% slice(-1, -n())
  
  points <- st_transform(points, crs)
  line <- st_transform(line, crs)
  # buffer the points by the tolerance
  points_buf <- st_buffer(points, 30)
  # intersect the line with the buffer
  line_intersect <- st_intersection(line, points_buf)
  # convert mutlinestrings (more than one road segment) into linestrings
  line_intersect <- do.call(rbind,lapply(1:nrow(line_intersect),function(x){st_cast(line_intersect[x,],"LINESTRING")}))
  
  # for each line intersection, calculate the nearest point on that line to our gps point
  nearest_pt <- do.call(rbind,lapply(points$id, 
                                     function(i){
                                       points[points$id==i,] %>%  st_nearest_points(line_intersect[line_intersect$id==i,]) %>% st_sf %>%
                                         st_cast('POINT') %>% mutate(id = i)
                                     }
  )
  )
  
  nearest_pt<- nearest_pt[seq(2, nrow(nearest_pt), by = 2),] %>%
    mutate(option = 1:nrow(.))
  
  # find an unambiguous reference point with only one snap option
  unambiguous_pt <- nearest_pt %>%
    group_by(id) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    filter(count == 1) %>%
    slice(1)
  
  # calculate network distance along our line to each snapped point
  dists <- rgeos::gProject(as(line,'Spatial'), as(nearest_pt,'Spatial'))
  # join back to nearest points data
  dists <- nearest_pt %>% cbind(dists)
  
  # we want to recursively do the following:
  # 1. calculate the network distance from our unambiguous reference point to the next id point in the data
  # 2. keep the snapped point for that id that was closest *along the network*  to the previous id
  # 3. set the newly snapped point as our reference point
  # 4. repeat
  
  # get distances from our reference point to the next point id
  for(i in unambiguous_pt$id:(max(dists$id)-1)){
    next_dist <- which.min(abs(dists[dists$id== i +1,]$dists - dists[dists$id== unambiguous_pt$id,]$dists ))
    next_option <- dists[dists$id== i +1,][next_dist,]$option
    nearest_pt <- nearest_pt %>% filter(id != i+1 | option == next_option)
    unambiguous_pt <- nearest_pt %>% filter(id ==i+1 & option == next_option)
    dists <- nearest_pt %>% cbind(dists = rgeos::gProject(as(line,'Spatial'), as(nearest_pt,'Spatial')))
  }
  
  # and in the reverse direction
  for(i in unambiguous_pt$id:(min(dists$id)+1)){
    next_dist <- which.min(abs(dists[dists$id== i -1,]$dists - dists[dists$id== unambiguous_pt$id,]$dists ))
    next_option <- dists[dists$id== i -1,][next_dist,]$option
    nearest_pt <- nearest_pt %>% filter(id != i-1 | option == next_option)
    unambiguous_pt <- nearest_pt %>% filter(id ==i-1 & option == next_option)
    dists <- nearest_pt %>% cbind(dists = rgeos::gProject(as(line,'Spatial'), as(nearest_pt,'Spatial')))
  }
  
  # transform back into lat/lng
  snapped_points <- nearest_pt %>%
    st_transform(4326) %>%
    cbind(st_coordinates(.)) %>%
    st_set_geometry(NULL) %>%
    select(-option) %>%
    # juntar os dois primeiros pontos de volta
    rbind(points_extremo %>% select(id, X = lon, Y = lat) %>% st_set_geometry(NULL)) %>%
    arrange(id)
  
  
  return(snapped_points)
}