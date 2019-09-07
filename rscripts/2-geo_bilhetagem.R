
Sys.setenv(TZ='UTC') 

# funcao para integracao

integrar_dados <- function(data1, data2) {
  
  Sys.setenv(TZ='UTC') 
  # localização dos terminais
  
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
  # abrir dados
  
  `%nin%` = Negate(`%in%`)
  
  bilhetagem_all <- fread(data1)
  bilhetagem_all[, hora := fasttime::fastPOSIXct(hora, tz="UTC")]
  bilhetagem_all[, prefixo_carro := as.numeric(prefixo_carro)]
  
  bilhetagem_terminais <- bilhetagem_all %>%
    filter(linha %in% terminais$linha) %>%
    left_join(terminais) %>%
    mutate(vehicleid = NA) %>%
    select(id, hora, linha, prefixo_carro, vehicleid, sentido_viagem, tipo_cartao, integracao, dia, momento, lon, lat)
  
  bilhetagem <- bilhetagem_all %>%
    filter(linha %nin% terminais$linha)
  
  # filter(tipo_cartao != "Inteira")
  # bilhetagem <- read_csv("data/bilhetagem/2017/bilhetagem_2017-11-03.csv")
  
  gps <- data.table::fread(data2)
  gps[, id_gps := 1:nrow(gps)]
  gps[, vehicleid := as.numeric(vehicle_vehicleid)]
  gps[, hora := fasttime::fastPOSIXct(hora, tz="UTC")]
  gps <- gps[, .(id_gps, vehicleid, hora, lon = longitude, lat = latitude)]
  
  # # para 2015
  # dicionario2 <- read.csv("data/dicionario_veiculos.csv", sep = ";", header=TRUE)
  # para 2018
  dicionario2 <- read_delim("../data/dicionario/2018/veiculos.csv", delim = ";") %>%
    select(vehicleid = id_veiculo, prefixo_carro = cod_veiculo) %>%
    mutate(vehicleid = as.numeric(vehicleid)) %>%
    mutate(prefixo_carro = as.numeric(prefixo_carro)) %>%
    filter(!is.na(prefixo_carro))
  
  #isso vai criar coluna vehicleid na bilhetagem
  bilhetagem_v1 <- bilhetagem %>% 
    # mutate(prefixo_carro = as.character(prefixo_carro)) %>%
    left_join(dicionario2)
  
  
  #extrair o dia correto  
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  dia_correto <- Mode(bilhetagem$dia)
  
  #criar data.table    
  setDT(bilhetagem_v1)
  setDT(gps)
  gps1 <- gps[,.(id_gps,vehicleid, hora)]
  
  setkey(bilhetagem_v1, vehicleid, hora)
  setkey(gps1, vehicleid, hora)
  
  
  
  #funcao para buscar o horario da bilhetagem mais proximo ao horario no gps
  aiai <- gps1[bilhetagem_v1, roll="nearest"] %>% 
    arrange(vehicleid) %>%
    # criar coluna com lat e lon na bilhetagem e extrair somente aqueles que nao foram NA's
    left_join(select(gps, id_gps, lon, lat), by = "id_gps") %>%
    select(id, hora, linha, prefixo_carro, vehicleid, sentido_viagem, tipo_cartao, integracao, dia, momento, lon, lat) %>%
    rbind(bilhetagem_terminais)
  
  
  # aiai_fim %>%
  #   filter(is.na(id_gps)) %>%
  #   filter(linha == 28) %>%
  #   # count(linha, nome_linha, sort = T) %>%
  #   View()
  # 
  # aiai_ok <- aiai_fim %>%
  #   # left_join(terminais, by = "linha") %>%
  #   # filter(!is.na(lon)) %>%
  #   filter(!is.na(id_gps)) %>%
  #   arrange(vehicleid)
  
  fwrite(aiai, paste0("../data/bilhetagem_integrado/2018/bilhetagemintegrado_", dia_correto, ".csv"))
  
}

# Aplicar

dir_gps <- dir("../data/gps/2018/novo/09", full.names = TRUE, pattern = "*.csv")
dir_bilhetagem <- dir("../data/bilhetagem/2018/09", full.names = TRUE, pattern = "*.csv")

walk2(dir_bilhetagem[1:10], dir_gps[1:10], integrar_dados)
walk2(dir_bilhetagem[11:20], dir_gps[11:20], integrar_dados)
walk2(dir_bilhetagem[21:30], dir_gps[21:30], integrar_dados)

# para agosto

dir_gps <- dir("../data/gps/2018/novo/08", full.names = TRUE, pattern = "*.csv")
dir_bilhetagem <- dir("../data/bilhetagem/2018/08", full.names = TRUE, pattern = "*.csv")

invisible(parallel::mcmapply(integrar_dados, dir_bilhetagem[1:10], dir_gps[1:10],
                             mc.cores = 4))

invisible(parallel::mcmapply(integrar_dados, dir_bilhetagem[11:20], dir_gps[11:20],
                             mc.cores = 4))

invisible(parallel::mcmapply(integrar_dados, dir_bilhetagem[21:31], dir_gps[21:31],
                             mc.cores = 4))
