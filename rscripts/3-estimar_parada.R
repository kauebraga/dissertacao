# # Selecionar as bilhetagem e os GPS do mes de setembro
# bi <- "../data/bilhetagem_integrado/2018/bilhetagemintegrado_2018-09-20.csv"
# stop_linhas <-  "../data/paradas_consolidadas/paradas_consolidadas_2018-09.csv"

# Definir funcao
bilhetagem_parada <- function(bi, stop_linhas = "../data/paradas_consolidadas/paradas_consolidadas_2018-09.csv") {
  
  # abrir bilhetagem
  
  terminais <- tibble::tribble(
    ~linha,               ~lon, ~lat,
    1, -38.584392, -3.737581,
    3, -38.563376, -3.776291,
    5, -38.569898, -3.771568,
    6, -38.502051, -3.831126,
    7, -38.607575, -3.772973,
    8, -38.485016, -3.738307,
    10, -38.586828, -3.789931
  )
  
  bilhetagem_total <- fread(bi) %>%
    mutate(hora = fasttime::fastPOSIXct(hora, tz = "UTC")) %>%
    filter(!is.na(lon)) %>%
    arrange(id, hora) %>%
    mutate(linha_sentido = paste(linha, stringr::str_sub(sentido_viagem, 1, 1), sep = "-"))
  
  bilhetagem_terminais <- bilhetagem_total %>%
    filter(linha %in% terminais$linha) %>%
    mutate(stop_id = NA, stop_sequence = 1) %>%
    select(id, hora, linha, prefixo_carro, tipo_cartao, sentido_viagem, integracao, dia, stop_id, stop_sequence, lon, lat)
  
  
  bilhetagem <- bilhetagem_total %>% filter(linha %nin% terminais$linha)
  
  # extrair dia
  
  dia <- unique(bilhetagem$dia)[1]
  
  # abrir bilhetagem paradas
  
  stops_linhas <- read_csv(stop_linhas)
  
  # vai
  
  # garantir que todas as linhas da bilhetagem tenham correspondente nas linhas do gtfs e vice-versa
  linhas.gtfs <- unique(stops_linhas$linha_sentido)
  linhas.bilhetagem <- unique(bilhetagem$linha_sentido)
  
  # Quais linhas estao na bilhetagem mas nao estao no GTFS?
  linhas_fora <- dplyr::setdiff(linhas.bilhetagem, linhas.gtfs) %>% sort()
  
  bilhetagem.correto <- bilhetagem %>%
    filter(linha_sentido %in% linhas.gtfs)
  
  
  
  stops_bilhetagem <- function(linha_escolhida, bilhetagem_vai, stops_linhas_escolhido) {
    data.linha <- linha_escolhida
    data.bilhetagem <- bilhetagem_vai
    data.stops_linhas <- stops_linhas_escolhido
    
    bilhetagem.1 <- data.bilhetagem %>% filter(linha_sentido == data.linha)
    
    paradas <- filter(data.stops_linhas, linha_sentido == data.linha) 
    
    paradas$id_arrocha <- 1:nrow(paradas)
    
    closest <- RANN::nn2(select(paradas, lon, lat), select(bilhetagem.1, lon, lat), 1)
    
    arrocha <- closest$nn.idx %>% as.vector() #isola os indexs
    
    bilhetagem.2 <- bilhetagem.1 %>% 
      mutate(id_arrocha = arrocha) %>% #junta a minha bilhetagem
      left_join(paradas, by = c("id_arrocha"), suffix = c(".bilhetagem", ".parada"))
    #left_join(select(stops, id_parada, id_stop)) %>%
    #mutate(intervalo = floor_date(hora, "15 min"))
    
    return(bilhetagem.2)
    
    
  }
  
  linhaaa <- unique(bilhetagem.correto$linha_sentido)
  
  bi_parada_final <- linhaaa %>% 
    map_dfr(stops_bilhetagem, bilhetagem.correto, stops_linhas) %>%
    arrange(id, hora) %>%
    select(id, hora, linha, prefixo_carro, tipo_cartao, sentido_viagem, integracao, dia, stop_id, stop_sequence, lon = lon.parada, lat = lat.parada) %>%
    rbind(bilhetagem_terminais)
  
  # Lista com o output final mais as linhas q nao tiveram correspondentes
  
  oi <- list(bilhetagem_paradas = bi_parada_final, linhas_fora = linhas_fora)
  write_rds(assign(dia,oi), sprintf("../data/bilhetagem_com_paradas/rds/bilhetagem_com_paradas_%s.rds", dia))
}

# Aplicar

# Aplicar a funcao
dir_bilhetagem <- dir("../data/bilhetagem_integrado/2018", full.names = TRUE, pattern = "*.csv")

walk(dir_bilhetagem, bilhetagem_parada)