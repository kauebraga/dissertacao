source("R/setup.R")
# source("R/get_bigdata.R")

# ideia: so fazer essa analise de gps para as 50 linhas mais carregadas... mas como justificar isso?
# linhas menos carregadas não passam por grandes corredores
# linhas menos carregadas 

# linhas_100 <- get_smartcard() %>%
#   count(linha, sort = T) %>%
#   slice(1:100) %>%
#   identity()
# 
# # quantas pessoas essas linhas carregam?
# 
# get_smartcard() %>%
#   filter(linha %in% linhas_100$linha) %>%
#   nrow()

bilhetagem <- "../data/bilhetagem_integrado/2018/bilhetagemintegrado_2018-09-03.csv"
gps <- "../data/gps/2018/novo/09/gps_2018-09-03.csv"

integrar_gps <- function(bilhetagem, gps) {
  
  # ABRIR DADOS -------------------------------------------------------------
  
  # Bilhetagem
  bilhetagem <- read_csv(bilhetagem) %>%
    # filter(linha %in% linhas_50$linha) %>%
    identity()
  
  # # Quais sao as linhas mais carregadas?
  # linhas_50 <- bilhetagem %>%
  #   count(linha, sort = TRUE) %>%
  #   slice(1:50)
  # 
  # sum(linhas_50$n)/nrow(bilhetagem)
  
  # GPS
  gps <- fread(gps)
  gps[, id_gps := 1:nrow(gps)]
  gps[, vehicleid := as.numeric(vehicle_vehicleid)]
  gps[, hora := fasttime::fastPOSIXct(hora, tz="UTC")]
  gps <- gps[, .(id_gps, vehicleid, hora, lon = longitude, lat = latitude)]
  
  
  # # Ou quais as linhas com pelo menos 1000 validacoes diarias?
  # bilhetagem %>%
  #   count(linha, sort = TRUE) %>%
  #   filter(n > 5000) %>%
  #   View()
  
  # Linhas do renan
  expresos_cjao <- st_read("../data-raw/linhas_fortaleza_renan/SIT-FOR24052017.shp", crs = 4326,
                           options = "ENCODING=WINDOWS-1252") %>%
    mutate(linha = as.character(CDIGO)) %>%
    mutate(linha = as.numeric(linha)) %>%
    filter(TIPO_LINHA == "CJI" | grepl("express(a|o)", NOME, ignore.case = TRUE))
  
  # Tirar expressos e corujoes da base da bilhetagem
  bilhetagem <- bilhetagem %>%
    filter(linha %nin% c(61, 63, 65, 665, 619, expresos_cjao$linha))
  
  # # testes ------------------------------------------------------------------
  # # sempre fazer esses teste para saber se o tempo esta correto
  # hist(bilhetagem$hora, breaks = "hours")
  # hist(gps$hora, breaks = "hours")
  # 
  # mean(gps$odometro)
  # mean(gps$que)
  # mean(gps$vehicleid)
  # 
  # max(gps$vehicleid)
  
  # filtro temporal -----------------------------------------
  
  # determinar o começo e o final de cada viagem (em cada linha)
  
  # bilhetagem_v1 <- bilhetagem %>%
  #   arrange(vehicleid, hora) %>%
  #   group_by(vehicleid) %>%
  #   # resolver aqui
  #   mutate(pegada = rep(1:length(rle(linha)$lengths), times = rle(linha)$lengths)) %>%
  #   ungroup() %>%
  #   group_by(vehicleid, linha, pegada) %>%
  #   summarise(start_trip = min(hora), end_trip = max(hora)) %>%
  #   arrange(vehicleid, pegada) %>%
  #   mutate(start_trip = start_trip - hms::hms(0, 10, 0),
  #          end_trip = end_trip + hms::hms(0, 10, 0)) %>%
  #   identity()
  
  setDT(bilhetagem)
  bilhetagem_v1 <- bilhetagem[order(vehicleid, hora)]
  # bilhetagem_v1[, pegada := rep(1:length(rle(linha)$lengths), times = rle(linha)$lengths), by = vehicleid]
  bilhetagem_v1[, pegada := rleid(linha), by = vehicleid]
  bilhetagem_v1 <- bilhetagem_v1[, .(start_trip = min(hora), end_trip = max(hora)), 
                                 keyby = .(vehicleid, linha, pegada)]
  bilhetagem_v1 <- bilhetagem_v1[order(vehicleid, pegada)]
  bilhetagem_v1[,  ':='(start_trip = start_trip - hms::hms(0, 15, 0), 
                        end_trip = end_trip + hms::hms(0, 15, 0))]
  
  # agora, juntar o gps com a bilhetagem (usar join do data.table)
  # setDT(gps)
  gps_v2 <- gps %>%
    left_join(bilhetagem_v1, by = "vehicleid") %>%
    filter(data.table::between(hora, start_trip, end_trip))
  
  # gps_v2 <- gps[bilhetagem_v1, on = "vehicleid", allow.cartesian=TRUE]
  # gps_v2 <- gps_v2[between(hora, start_trip, end_trip)]
  
  
  
  
  # filtro espacial ---------------------------------------------------------
  
  # abrir
  linhas <- st_read("../data/linhas/2018/linhas_2018-09.shp", crs = 4326) %>%
    # transformar para utm
    st_transform(31984) %>%
    # criar buffer de 400 metros
    st_buffer(300) %>%
    st_transform(4326) %>%
    # extrair somente a linha (sem o sentido) %>%
    mutate(linha_v1 = str_extract(shape_id, "\\d{3}")) %>% 
    mutate(linha_v1 = as.integer(linha_v1)) %>%
    count(linha_v1) %>%
    select(linha = linha_v1, -n) %>%
    # Tirar a linha 767, que nao tem nenhuma viagem
    filter(linha != 767)
  
  # Quais linhas comecam ou terminam em terminais?
  linhas_terminal <- read_csv("../data/paradas_consolidadas/paradas_consolidadas_2018-09.csv") %>%
    group_by(linha_sentido) %>%
    slice(1, n()) %>%
    filter(grepl("terminal", stop_name, ignore.case = TRUE)) %>%
    separate(linha_sentido, c("linha", "sentido")) %>%
    mutate(terminal = case_when(
      grepl("ANTONIO BEZERRA", stop_name, fixed = TRUE) ~ "ant_bezerra",
      grepl("SIQUEIRA", stop_name, fixed = TRUE) ~ "siqueira",
      grepl("PAPICU", stop_name) ~ "papicu",
      grepl("PRAÇA TERMINAL CONJUNTO CEARÁ, SN", stop_name) ~ "cj_ceara",
      grepl("MESSEJANA", stop_name) ~ "messejana",
      grepl("LAGOA", stop_name) ~ "lagoa",
      grepl("PARANGABA", stop_name) ~ "parangaba"
    )) %>%
    distinct(linha, terminal)
  
  # Abrir shape dos terminais e fazer integracao com linhas do terminal
  terminais <- read_rds("../data/terminais.rds") %>%
    left_join(linhas_terminal, by = "terminal") %>%
    select(-terminal)
  
  # Identificar linhas que nao tenham o temrinal como fim ou comeco
  linhas_nterminal <- read_csv("../data/paradas_consolidadas/paradas_consolidadas_2018-09.csv") %>%
    group_by(linha_sentido) %>%
    slice(1, n()) %>%
    filter(!grepl("terminal", stop_name, ignore.case = TRUE)) %>%
    select(linha_sentido, stop_sequence, lon, lat) %>%
    to_spatial() %>%
    st_transform(31984) %>%
    # Criar buffer em relacao a ultima parada
    st_buffer(70) %>%
    st_transform(4326) %>%
    # extrair somente a linha (sem o sentido) %>%
    mutate(linha = str_extract(linha_sentido, "^\\d+")) %>%
    ungroup() %>%
    # mutate(linha_v1 = as.integer(linha_v1)) %>%
    count(linha) %>% 
    select(-n)
  
  # Compor linhas: tirar a parte do terminal, se passar por terminal, e nao terminal
  linhas_finais <- rbind(terminais, linhas_nterminal) %>% mutate(linha = as.numeric(linha))
  
  linhas_list <- split(linhas, linhas$linha)
  linhas_com_finais_list <- split(linhas_finais, linhas_finais$linha)
  
  # # teste
  # linhas1 <- linhas_list$`26`
  # linhas_com_finais1 <- linhas_com_finais_list$`26`
  
  # Funcao para extrair os finais de cada linha
  extrair_finais_linhas <- function(linhas1, linhas_com_finais1) {
    
    # Juntar os inicios e fim
    linhas_com_finais1 <- linhas_com_finais1 %>% mutate(id = 1) %>% count(linha, id) %>% select(-n, -id)
    # Primeiro, tirar so a parte que nao esta no final
    vai1 <- st_difference(linhas1, linhas_com_finais1) %>% select(-linha.1) %>% mutate(tipo = "em_linha")
    linhas_com_finais <- linhas_com_finais1 %>%  mutate(tipo = "inicio_ou_fim")
    # Agora, juntar isso com os finais
    fim <- rbind(vai1, linhas_com_finais)
  }
  
  linhas_fim <- map2(linhas_list, linhas_com_finais_list, extrair_finais_linhas) %>% 
    bind_rows() %>% 
    as_tibble() %>% 
    st_sf(crs = 4326)
  
  # # Fazer a juncao das linhas com os terminais
  # linhas_fim <- linhas %>%
  #   mutate(terminal = "nao") %>%
  #   rbind(terminais)
  
  
  # # teste
  mapview(filter(linhas_fim, linha == "71")) + mapview(shapes_desagregados %>% filter(linha_sentido == "71-I")
                                                       %>% to_spatial())
  
  # gps_v3 <- gps_v2 %>%
  #   st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # quais linhas foram registradas na base do gps
  gps_linhas <- unique(gps_v2$linha)
  
  # quais linhas foram registradas na base do gtfs
  gtfs_linhas <- unique(linhas_fim$linha)
  
  # garantir que todas as linhas do gps tenha correspondente na base gtfs
  linhas_v1 <- linhas_fim %>%
    filter(linha %in% gps_linhas)
  
  # aqui
  setDT(gps_v2)
  gps_v3 <- gps_v2[linha %in% unique(linhas_v1$linha)]
  
  
  # criar uma lista com um data.frame para cada linha
  gps_list <- gps_v3[order(linha)]
  gps_list <- split(gps_list, by = "linha")
  
  setDT(linhas_v1)
  gtfs_list <- linhas_v1[, linha := as.numeric(linha)]
  gtfs_list <- linhas_v1[order(linha)]
  gtfs_list <- split(gtfs_list, by = "linha")
  
  
  # realizar operacao verificando quais pontos 
  # future::plan("multiprocess")
  # ooohh <- furrr::future_map2_dfr(gps_list, gtfs_list, st_join)
  gps_list <- map(gps_list, st_as_sf, coords = c("lon", "lat"), crs = 4326) 
  gtfs_list <- map(gtfs_list, st_sf, crs = 4326) 
  ooohh <- map2_dfr(gps_list, gtfs_list, st_join)
  
  # salvar
  dia <- unique(bilhetagem$dia)[1]
  out <- sprintf("../data/gps_linhas/gps_linhas_%s.rds", dia)
  
  source("R/sfc_as_cols.R")
  ooohh %>% as_tibble() %>%
    st_sf(crs = 4326) %>%
    sfc_as_cols() %>%
    write_rds(out)
}


# APLICAR -----------------------------------------------------------------

source("R/sfc_as_cols.R")
dir_bi <- dir("../data/bilhetagem_integrado/2018", full.names = TRUE, 
              pattern = "bilhetagemintegrado_2018-09")
dir_gps <- dir("../data/gps/2018/novo/09", full.names = TRUE, pattern = "*.csv")

# walk2(dir_bi[1:5], dir_gps[1:5], integrar_gps)
.rs.restartR()
# walk2(dir_bi[6:10], dir_gps[6:10], integrar_gps)
# walk2(dir_bi[11:15], dir_gps[11:15], integrar_gps)
# walk2(dir_bi[16:25], dir_gps[16:25], integrar_gps)
# walk2(dir_bi[26:30], dir_gps[26:30], integr ar_gps)

# para agosto
dir_bi <- dir("../data/bilhetagem_integrado/2018", full.names = TRUE, pattern = "*.csv")
dir_gps <- dir("../data/gps/2018/novo/08", full.names = TRUE, pattern = "*.csv")

invisible(parallel::mcmapply(integrar_gps, dir_bi[1:4], dir_gps[1:4],
                             mc.cores = 1))
  
invisible(parallel::mcmapply(integrar_gps, dir_bi[5:8], dir_gps[5:8],
                             mc.cores = 1))
