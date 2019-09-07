ligar_servidor <- function(cidade, porta = 8080) {
  
  if (Sys.info()[1] == "Linux") {
    
    command <- sprintf("java -Xmx4G -jar ../otp/programs/otp.jar --router %s --graphs ../otp/graphs --server --port %s --securePort 8802", cidade, porta)
    
    system(command, intern = FALSE, wait = FALSE)
    otp_for <- otp_connect(router = cidade, port = porta)
    
  } else {
    
    otp_setup(otp = "../otp/programs/otp.jar", dir = "../otp", router = cidade, port = porta)
    otp_for <- otp_connect(router = cidade)
    
  }
}

municipio <- "for_corrigido"
selecionar = "8880104f47fffff"

matriz_acessibilidade <- function(municipio, selecionar = NULL) {
  
  # ABRIR ARQUIVOS ----------------------------------------------------------
  points <- fread("../otp/points/points_for_padrao_08.csv")
  
  # GERAR TABELA DE MATRIZ --------------------------------------------------
  
  for_od <- points %>%
    expand(id_hex, id_hex) %>%
    left_join(points) %>%
    left_join(points, by = c("id_hex1" = "id_hex"), suffix = c(".origem", ".destino")) %>%
    rename(origem = id_hex, destino = id_hex1)
  
  if (is_null(selecionar)) {
    
    for_od <- for_od
  } else if (is_character(selecionar)) {
    
    for_od <- filter(for_od, origem == selecionar)
  }
  
  
  # GERAR LISTA COM COORDENADAS ---------------------------------------------
  
  
  origem <- map2(for_od$Y.origem, for_od$X.origem, c)
  destino <- map2(for_od$Y.destino, for_od$X.destino, c)
  
  names(origem) <- 1:length(origem)
  names(destino) <- 1:length(destino)
  
  
  url <- paste0("http://localhost:8080/otp/routers/", municipio, "/plan")
  
  request_url <- function(origem, destino, vai) {
    
    # TRATAR AS COORDENADAS ---------------------------------------------------
    
    fromPlace <- paste0(origem, collapse = ",")
    toPlace <- paste0(destino, collapse = ",")
    
    
    # MAKE REQUEST ------------------------------------------------------------
    
    req <- httr::GET(
      vai,
      query = list(
        fromPlace = fromPlace,
        toPlace = toPlace,
        mode = "TRANSIT,WALK",
        date = "09-10-2018",
        time = "07:30am",
        maxWalkDistance = "800",
        # walkReluctance = "2",
        # clampInitialWait = 0,
        # arriveBy = "FALSE",
        # transferPenalty = "0",
        # minTransferTime = "0",
        numItineraries = "1"
      )
    )
    
    text <- httr::content(req, as = "text", encoding = "UTF-8")
    
    x <- jsonlite::fromJSON(text)
    
  }
  
  # plan(multiprocess)
  # finni <- future_map2(origem, destino, request_url, vai = url, .progress = TRUE)
  finni <- parallel::mcmapply(request_url, origem, destino, MoreArgs = list(vai = url), 
                              SIMPLIFY = FALSE,
                              mc.cores = 2)
  
  names(finni) <- paste(for_od$origem, for_od$destino, sep = "-")
  
  
  # FUNCAO PARA ACESSAR CONTEUDO DA CONSULTA --------------------------------
  
  acessar_consulta <- function(list.consulta) {
    
    if (is.data.frame(list.consulta[["plan"]][["itineraries"]])) {
      
      # df <- list.consulta[["plan"]][["itineraries"]] %>%
      #   as.data.frame() %>%
      #   select(duration, walkTime, transitTime, waitingTime, transfers) %>%
      #   mutate(option = 1:n())
      
      df <- setDT(list.consulta[["plan"]][["itineraries"]][["legs"]][[1]])
      # df <- df[, .(duration, walkTime, walkDistance, transitTime, waitingTime, transfers)]
      
      # df <- setDT(list.consulta[["plan"]][["itineraries"]])
      # df <- df[, .(duration, walkTime, walkDistance, transitTime, waitingTime, transfers)]
      
    } else {
      
      df <- data.table(duration = 0, walkTime = 0, walkDistance = 0, transitTime = 0, waitingTime = 0, transfers = 0)
      
    }
    
    
  }
  
  fin_v1 <- map(finni, acessar_consulta) %>%
    rbindlist(idcol="origem_destino") %>%
    mutate_at(c("duration", "walkTime", "transitTime", "waitingTime"), ~ round(./60, digits = 1)) %>%
    separate(origem_destino, c("id_origem", "id_destino"), sep = "-", remove = FALSE)
  
  
}

library(opentripplanner)
# Ligar otp
ligar_servidor("for_corrigido")

# Aplicar funcao
tictoc::tic()
teste <- matriz_acessibilidade("for_corrigido", selecionar = 10)
tictoc::toc()


# ---------------------------------------------------------------------------------------------

fin_v1 %>%
  filter(origem_destino == "8880104f47fffff-8880104557fffff")

teste_real %>%
  filter(par == "8880104f47fffff-8880104557fffff")

# comparar outputs

comparar <- fin_v1 %>%
  mutate(duration = duration - waitingTime) %>%
  # select(origem_destino, duration) %>%
  left_join(teste_real, by = c("origem_destino" = "par")) %>%
  mutate(travel_time = travel_time / 60)



# USANDO O PACOTE OTP -------------------------------------------------------------------------

library(opentripplanner)

otp_stop()

ligar_servidor("forcorrigidocm")
ligar_servidor("forpadrao")
vai <- otp_connect(router = "forcorrigidocm")
vai1 <- otp_connect(router = "forpadrao")

points <- fread("../otp/points/points_for_padrao_08.csv")
# points <- fread("../otp/points/points_for_09.csv")

# sample <- points %>% slice(1:50)

points_origem <- points %>%
  expand(id_hex, id_hex) %>%
  left_join(points) %>%
  left_join(points, by = c("id_hex1" = "id_hex"), suffix = c(".origem", ".destino")) %>%
  rename(origem = id_hex, destino = id_hex1)
  # filter(origem == "8980104e203ffff")

points_origem1 <-   as.matrix(points_origem[, 3:4])

points_destino <- points %>%
  expand(id_hex, id_hex) %>%
  left_join(points) %>%
  left_join(points, by = c("id_hex1" = "id_hex"), suffix = c(".origem", ".destino")) %>%
  rename(origem = id_hex, destino = id_hex1)
  # filter(origem == "8980104e203ffff")

points_destino1 <-   as.matrix(points_destino[, 5:6])


tictoc::tic()
ooi <- otp_plan(otpcon = vai,
                fromPlace = points_origem1,
                toPlace = points_destino1,
                fromID = points_origem$origem,
                toID = points_origem$destino,
                date_time = as.POSIXct("2018-09-10 07:00:00"),
                mode = c("WALK", "TRANSIT"),
                numItineraries = 1,
                get_geometry = FALSE,
                ncores = 4)
tictoc::toc()
        