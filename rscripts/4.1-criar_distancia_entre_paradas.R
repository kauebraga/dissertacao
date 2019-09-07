source("R/setup.R")


# Linhas do renan
expresos_cjao <- st_read("../data-raw/linhas_fortaleza_renan/SIT-FOR24052017.shp", crs = 4326,
                         options = "ENCODING=WINDOWS-1252") %>%
  mutate(linha = as.character(CDIGO)) %>%
  mutate(linha = as.numeric(linha)) %>%
  filter(TIPO_LINHA == "CJI" | grepl("express(a|o)", NOME, ignore.case = TRUE))


# ABRIR SHAPES ------------------------------------------------------------

shapes <- read_delim("../data-raw/gtfs/2018/GTFS_fortaleza_20180907/shapes.txt", delim = ',') %>%
  mutate(linha = str_extract(shape_id, "\\d{3}")) %>% 
  mutate(linha = as.integer(linha)) %>%
  mutate(sentido = str_sub(shape_id, -1, -1)) %>%
  mutate(linha_sentido = paste0(linha, "-", sentido)) %>%
  select(linha, linha_sentido, shape_pt_sequence, lon = shape_pt_lon, lat = shape_pt_lat) %>%
  # Tirar essa linha 4 que eu nao sei muito o que significa
  filter(linha_sentido %nin% c("4-I", "4-V")) %>%
  # Tirar expressos e corujoes
  filter(linha %nin% c(61, 63, 65, 665, 619, expresos_cjao$linha)) %>%
  select(-linha) %>%
  arrange(linha_sentido)

# Isolar linhas dos shapes para comparacao
linhas_shapes <- unique(shapes$linha_sentido)



# ABRIR LINHAS_PARADAS ----------------------------------------------------


stops_linhas <- read_csv("../data/paradas_consolidadas/paradas_consolidadas_2018-09.csv") %>%
  # Tirar essa linha 4 que eu nao sei muito o que significa
  filter(linha_sentido %nin% c("4-I", "4-V")) %>%
  mutate(linha = str_extract(linha_sentido, "^\\d+")) %>%
  # Tirar expressos e corujoes
  filter(linha %nin% c(61, 63, 65, 665, 619, expresos_cjao$linha)) %>%
  # Filtrar somente as linhas que tem nos shapes
  filter(linha_sentido %in% linhas_shapes) %>%
  select(-linha) %>%
  arrange(linha_sentido)

# Agora, filtra somente as linhas dos stops nos shapes
linhas_stops <- unique(stops_linhas$linha_sentido)

shapes <- shapes %>% filter(linha_sentido %in% linhas_stops)


# CONFERIR SE AS LINHAS DAS DUAS TABELAS BATEM ----------------------------
a <- unique(shapes$linha_sentido)
b <- unique(stops_linhas$linha_sentido)

identical(a, b)

which(a != b)

# CRIAR LISTAS ------------------------------------------------------------

stops_linhas_list <- split(setDT(stops_linhas), by = "linha_sentido")
shapes_list <- split(setDT(shapes), by = "linha_sentido")



shapes_df <- shapes_list$`41-V`
stops_linhas_df <- stops_linhas_list$`41-V`


# CRIAR FUNCAO ------------------------------------------------------------

# Em qual ponto do arquivo shapes encontra-se a parada daquela linha e sentido?
shapes_stops_dists <- function(shapes_df, stops_linhas_df) {
  
  # Se a linha tiver so duas paradas, considerar aquelas duas parada
  
  if (nrow(stops_linhas_df) == 2) {
    
    shapes_df_v1 <- stops_linhas_df %>% mutate(dist = 0) %>% select(-stop_name)
    
    
  } else {
    
    
    # Separar a primeira e ultima parada
    stops_linhas_ok <- stops_linhas_df %>% 
      # slice(-1, -n()) %>%
      slice(-1) %>%
      mutate(stop_sequence_id = 1:n())
    
    # stops_linhas_ultimas <- slice(stops_linhas_df, 1, n()) %>% mutate(dist = 0) %>% select(-stop_name)
    stops_linhas_ultimas <- slice(stops_linhas_df, 1) %>% mutate(dist = 0) %>% select(-stop_name)
    
    # ou..
    # shapes_sp <- shapes_df %>% to_spatial() %>% st_transform(29194) %>%
    #   as_Spatial()
    # 
    # stops_linhas_sp <- stops_linhas_ok %>% to_spatial() %>% st_transform(29194) %>% as_Spatial()
    # 
    # # Aplicar geosphere
    # dist <- rgeos::gProject(linha_sp, gps_viagem_sp)
    
    #
    
    uui <- RANN::nn2(select(stops_linhas_ok, lon, lat), select(shapes_df, lon,lat), 1)
    
    # pegar o shape_pt_sequence que refere a qual parada
    vamos <- shapes_df %>% 
      mutate(stop_sequence_id = uui$nn.idx, dist = uui$nn.dists*111320) %>%
      left_join(stops_linhas_ok %>% select(stop_sequence, stop_sequence_id, stop_id)) %>%
      group_by(stop_sequence) %>%
      slice(which.min(dist)) %>%
      ungroup() %>%
      select(shape_pt_sequence, stop_id, stop_sequence)
    
    get.dist <- function(lon, lat) geosphere::distHaversine(tail(cbind(lon,lat),-1),head(cbind(lon,lat),-1))
    
    # jogar no arquivo de shapes
    shapes_df_v1 <- shapes_df %>%
      left_join(vamos, by = "shape_pt_sequence") %>%
      mutate(stop_sequence = as.numeric(stop_sequence)) %>%
      mutate(dist = c(0,cumsum(get.dist(lon,lat)))) %>%
      filter(!is.na(stop_sequence)) %>%
      select(-shape_pt_sequence) %>%
      # Juntar as ultimas
      rbind(stops_linhas_ultimas) %>%
      arrange(stop_sequence)
    
  }
  
}


# APLICAR FUNCAO ----------------------------------------------------------
shapes_stops_dists_safe <- possibly(shapes_stops_dists, otherwise = NA_real_)

trechos <- map2(shapes_list, stops_linhas_list, shapes_stops_dists_safe)

# SALVAR ------------------------------------------------------------------

trechos %>%
  rbindlist(use.names = TRUE) %>%
  write_rds("../data/distancia_entre_paradas_consolidadas_2019-09.rds")
  