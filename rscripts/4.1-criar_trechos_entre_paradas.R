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



# shapes_df <- shapes_list$`31-I`
# stops_linhas_df <- stops_linhas_list$`31-I`


# CRIAR FUNCAO ------------------------------------------------------------

# Em qual ponto do arquivo shapes encontra-se a parada daquela linha e sentido?
shapes_stops_dists <- function(shapes_df, stops_linhas_df) {
  
  stops_linhas_df <- stops_linhas_df %>% mutate(stop_sequence_id = 1:n())
  
  uui <- RANN::nn2(select(stops_linhas_df, lon, lat), select(shapes_df, lon,lat), 1)
  
  # pegar o shape_pt_sequence que refere a qual parada
  vamos <- shapes_df %>% 
    mutate(stop_sequence_id = uui$nn.idx, dist = uui$nn.dists*111320) %>%
    left_join(stops_linhas_df %>% select(stop_sequence, stop_sequence_id)) %>%
    group_by(stop_sequence) %>%
    slice(which.min(dist)) %>%
    ungroup() %>%
    select(shape_pt_sequence, stop_sequence)
  
  # jogar no arquivo de shapes
  shapes_df_v1 <- shapes_df %>%
    left_join(vamos, by = "shape_pt_sequence") %>%
    mutate(stop_sequence = as.numeric(stop_sequence)) %>%
    fill(stop_sequence)
  # mutate(stop_sequence = ifelse(is.na(stop_sequence), 0, stop_sequence)) %>%
  # mutate(trecho = rep(1:length(rle(stop_sequence)$lengths), times = rle(stop_sequence)$lengths))
  
  # criar data.frame so com as paradas
  shape_df_colar <- shapes_df_v1 %>%
    group_by(linha_sentido, stop_sequence) %>%
    slice(1) %>%
    ungroup() %>%
    slice(-1) %>%
    mutate(stop_sequence = stop_sequence - 1)
  
  # Colar
  shapes_df_v2 <- shapes_df_v1 %>%
    rbind(shape_df_colar) %>%
    arrange(shape_pt_sequence, stop_sequence) %>%
    to_spatial() %>%
    group_by(linha_sentido, stop_sequence) %>%
    summarise(n = n(), do_union=FALSE) %>%
    st_cast("LINESTRING") %>%
    ungroup() %>%
    mutate(stop_sequence_inicio = stop_sequence, stop_sequence_fim = stop_sequence + 1) %>%
    mutate(length = st_length(.)) %>%
    rename(trecho = stop_sequence) %>%
    left_join(select(stops_linhas_df, stop_id, stop_sequence), by = c("stop_sequence_inicio" = "stop_sequence")) %>%
    left_join(select(stops_linhas_df, stop_id, stop_sequence), by = c("stop_sequence_fim" = "stop_sequence")) %>%
    rename(stop_id_inicio = stop_id.x, stop_id_fim = stop_id.y) %>%
    select(-n) %>%
    # Tirar a ultima coluna
    slice(-n())
  
  
  
  # # Função para fazer o cast
  # points_to_line <- function(linestrings) {
  #   
  #   n <- nrow(linestrings) - 1
  #   
  #   vai_ne <- function(x) {
  #     pair <- st_union(linestrings[x,], linestrings[x + 1,])
  #     line <- st_cast(pair, "LINESTRING")
  #     return(line)
  #     
  #   }
  #   
  #   ui <- map_dfr(1:n, vai_ne)
  #   
  # }
  # 
  # 
  # egua <- points_to_line(shapes_df_v2)
  
}


# APLICAR FUNCAO ----------------------------------------------------------

trechos <- map2(shapes_list, stops_linhas_list, shapes_stops_dists)


# TESTES ------------------------------------------------------------------

mapview(trechos$`26-I`) + mapview(stops_linhas_list$`26-I` %>% to_spatial())
  

# SALVAR ------------------------------------------------------------------

write_rds(trechos, "../data/trechos_entre_paradas/trechos_entre_paradas_2019-09.rds")
  