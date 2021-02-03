gps_com_linhas <- "../data/gps_linhas/gps_linhas_2018-09-04.rds"

gps_para_paradas <- function(gps_com_linhas) {
  
  gps <- read_rds(gps_com_linhas)
  setDT(gps)
  gps <- na.omit(gps, cols = "linha.y")
  # Excluir algumas linhas corujoes que persistiram ao filtro passado
  # gps <- gps[linha.y %nin% c(61, 63, 65, 665, 619)]
  # gps <- gps[]
  
  paradas <- read_csv("../data/paradas_consolidadas/paradas_consolidadas_2018-09.csv")
  
  linhas_total <- gps %>% drop_na(linha.y) %>% filter(linha.y != 4) %>% distinct(linha.y) %>% .$linha
  
  por_linha <- function(linhas) {
    
    # # delete
    # linhas <- 71
    # linhas <- 88
    # linhas <- 76
    # linhas <- 86
    # linhas <- 60
    # linhas <- 41
    # linhas <- 215
    # linhas <- 26
    # linhas <- 75
    # linhas <- 45
    # linhas <- 320
    # linhas <- 11
    # linhas <- 325
    # linhas <- 330
    
    carros_vai <- gps[linha.y %in% linhas]
    carros_vai_1 <- unique(carros_vai$vehicleid)
    
    paradas_linha <- paradas %>%
      separate(linha_sentido, c("linha_iso", "sentido")) %>%
      filter(linha_iso %in% linhas) %>%
      group_by(sentido) %>%
      # Criar indices (1 eh ida, 2 eh volta)
      mutate(a = group_indices()) %>%
      ungroup() %>%
      # Filtrar somente a ida
      filter(a == 1) %>%
      # Ajeitar o nome dos terminais
      mutate(stop_name = janitor::make_clean_names(stop_name)) %>%
      # Colocar no formato comum
      mutate(stop_name = case_when(stop_name == "praca_terminal_antonio_bezerra_sn" ~ "ant_bezerra",
                                   stop_name == "praca_terminal_messejana_sn" ~ "messejana",
                                   stop_name == "praca_terminal_parangaba_sn" ~ "parangaba",
                                   stop_name == "praca_terminal_lagoa_sn" ~ "lagoa",
                                   stop_name == "praca_terminal_siqueira_sn" ~ "siqueira",
                                   stop_name == "praca_terminal_papicu_sn" ~ "papicu",
                                   stop_name == "praca_terminal_conjunto_ceara_sn" ~ "cj_ceara")) %>%
      # Criar id para o stop_sequence (as vezes pode estar fora de ordem..)
      mutate(stop_sequence_id = 1:n()) %>%
      select(stop_id, stop_name, stop_sequence, stop_sequence_id, lon, lat)
    
    parada_inicial <- 1
    parada_final <- last(paradas_linha, stop_sequence) %>% .[,"stop_sequence"] %>% as.numeric()
    parada_inicio_ou_fim <- c(parada_final, parada_inicial)
    parada_inicio_ou_fim_lag <- c(parada_final, parada_final -1, parada_inicial, parada_inicial + 1)
    
    # # deletar
    # para linha 71
    # carros <- 32101
    # para linha 88
    # carros <- 33095
    # Para linha 76
    # carros <- 32593
    # para linha 86
    # carros <- 32932
    # para linha 41
    # carros <- 32962
    # carros <- 32328
    # carros <- 32365
    # para linha 60
    # carros <- 32627
    # # para linha 26
    # carros <- 34090
    # carros <- 34092
    # # para linha 75
    # carros <- 33316
    # carros <- 36579
    # carros <- 33045 # problematico!
    # para a linha 45
    # carros <- 32257
    # carros <- 34094
    # carros <- 32253
    # para a linha 215
    # carros <- 32063
    # para a linha 320
    # carros <- 32697
    # para a linha 11
    # carros <- 33742
    # para a linha 325
    # carros <- 33663
    # para a linha 330
    # carros <- 32821
    
    por_carro <- function(carros) {
      
      # Extrair o veiculo desejado
      gps_carro <- carros_vai[vehicleid %in% carros]
      gps_carro <- gps_carro[order(hora)]
      # gps_carro <- gps_carro %>% arrange(hora, id_gps, terminal == "nao") %>% 
      # distinct(id_gps, .keep_all = TRUE)
      gps_carro <- gps_carro %>% mutate(id_gps_carro = 1:n())
      # gps_carro$dist <- c(1000, geosphere::distCosine(
      #   head(gps_carro[, c("lon", "lat")],-1), tail(gps_carro[, c("lon", "lat")],-1) ) )
      # gps_carro <- gps_carro %>%
      #   mutate(tempo = hora - lag(hora, default = 10000)) %>%
      #   # Criar grupos de tempos (se o intervalo entre tempo for menor que 80s, eh em servico)
      #   mutate(em_linha = ifelse(tempo < 200, 0, 1)) %>%
      #   # Criar grupos toda vez que o tempo for mais que 200
      #   mutate(idx = cumsum((em_linha == 1L)))
      
      # TENTATIVA FRUSTRADA DE INTERPOLACAO INTERPOLACAO ANTES:
      
      # seqlast <- function (from, to, by) 
      # {
      #   vec <- do.call(what = seq, args = list(from, to, by))
      #   if ( tail(vec, 1) != to ) {
      #     return(c(vec, to))
      #   } else {
      #     return(vec)
      #   }
      # }
      #   
      # interpolate_by_group <- function(df) {
      #   
      #   # Criar pontos a cada 10 segundos
      #   full.time   <- with(df,seqlast(df$hora[1],tail(df$hora,1),by=10))
      #   
      #   df_viagens <- distinct(df, hora, .keep_all = TRUE)
      #   
      #   library(zoo)
      #   df.zoo <- zoo(df_viagens[,c("lon", "lat")], df_viagens$hora)        # convert to zoo object
      #   result <- na.approx(df.zoo,xout=full.time)  # interpolate; result is also a zoo object
      #   # Transformar para df
      #   zoo.to.data.frame <- function(x, index.name="hora") {
      #     stopifnot(is.zoo(x))
      #     xn <- if(is.null(dim(x))) deparse(substitute(x)) else colnames(x)
      #     setNames(data.frame(index(x), x, row.names=NULL), c(index.name,xn))
      #   }
      #   
      #   # gps_carro_new <- zoo.to.data.frame(result) %>% as_tibble() %>%
      #   #   left_join(gps_viagens %>% select(-lon, -lat), by = "hora") %>%
      #   #   fill(linha_sentido, vehicleid, viagem)
      #   
      #   # Pegar as paradas daquele sentido
      #   gps_carro_new <- zoo.to.data.frame(result)
      #   gps_carro_new <- merge(setDT(gps_carro_new), 
      # setDT(df_viagens)[, !c("lon", "lat")], by = "hora", all.x = TRUE)
      #   gps_carro_new <- fill(gps_carro_new, linha.x, vehicleid)
      # }
      # 
      # gps_carro <- gps_carro %>%
      #   split(.$idx) %>%
      #   map_dfr(interpolate_by_group)
      
      # Para cada ponto de GPS, qual a parada mais proxima?
      opa <- RANN::nn2(select(paradas_linha, lon, lat), select(gps_carro, lon, lat), 1)
      
      setDT(gps_carro)
      vamos <- gps_carro[, ':='(stop_sequence_id = opa$nn.idx, dist = opa$nn.dists*111320)]
      # Trazer o stop_id, lon e lat de cada parada
      vamos <- merge(vamos, paradas_linha, by = "stop_sequence_id", suffixes = c(".gps", ".parada"))
      # Tirar o stop_sequence_id
      vamos <- vamos[, !c("stop_sequence_id")]
      # vamos[, id_gps_temp := 1:nrow(vamos)]
      vamos <- vamos[order(hora)]
      
      # PERGUNTA - por que eu nao fiz ao contrario? pegar o ponto de gps mais proximo de cada parada...
      # RESPOSTA - porque eu preciso do tempo em cada parada para todas as viagens dos carros.. se eu
      # fizesse ao contrario, eu teria o ponto de gps mais proxima de cada parada de TODAS as viagens
      # que foram realizadas.. e eu nao quero isso taokey.
      
      quantiles_zero_out <- function(vector, ...) {
        
        vector <- vector[!vector %in% 0]
        
        fim <- quantile(vector, ...)
        
      }
      
      vamos_v1 <- vamos %>%
        mutate(stop_sequence = as.numeric(stop_sequence)) %>%
        # Se for um stop_sequence do tipo (3, 4, 3), deletar  o 4!
        mutate(stop_sequence = ifelse(stop_sequence > lag(stop_sequence) & stop_sequence > lead(stop_sequence),
                                      NA, 
                                      stop_sequence)) %>%
        # E uma situacao 
        filter(!is.na(stop_sequence))
      
      
      # Se a linha tiver somente um registro em fim de viagem (por falta de resolucao espacial
      # do GPS), duplicar esse registro, ou seja: o momento de fim sera igual ao momento de 
      # inicio da viagem
      vamos_v2 <- vamos_v1 %>%
        # Identificar casos em que so tem um registro no fim da viagem
        mutate(um_registro = ifelse(tipo == "inicio_ou_fim" & lag(tipo) == "em_linha" & lead(tipo) == "em_linha", 
                                    1, 
                                    NA)) %>%
        # Filtrar essas observacoes
        filter(um_registro == 1) %>%
        select(-um_registro) %>%
        # Juntar com o df original
        rbind(vamos_v1) %>%
        # Ordenar por hora
        arrange(hora) %>%
        # A viagem acaba quando o veiculo tem o seu primeiro registro no terminal e termina quando
        # tem seu ultimo registro no terminal
        mutate(bloco_inicio_ou_fim = rleid(tipo)) %>%
        group_by(bloco_inicio_ou_fim) %>%
        mutate(inicio_ou_fim = ifelse(tipo != "em_linha" & row_number() == 1,
                                      "fim",
                                      ifelse(tipo != "em_linha" & row_number() == n(),
                                             "inicio", NA))) %>%
        ungroup() %>%
        # Identificar os pontos de gps de inicio ou fim
        mutate(stop_sequence_num = stop_sequence) %>%
        mutate(stop_sequence = ifelse(inicio_ou_fim %in% c("inicio", "fim"), 
                                      paste0(stop_sequence_num, "-", inicio_ou_fim), 
                                      stop_sequence_num)) %>%
        # Deletar os demais pontos de inicio ou fim
        filter(stop_sequence %nin% parada_inicio_ou_fim) %>%
        # Me arriscando
        # Criar um bloco de pontos na linha, dps de toda a cabuetagem
        mutate(em_linha = ifelse(grepl("^\\d{,2}$", stop_sequence), "viagem", "final")) %>%
        mutate(bloco_em_linha = rleid(em_linha)) %>%
        group_by(bloco_em_linha) %>%
        mutate(dif = c(0, diff(stop_sequence_num))) %>%
        add_count() %>%
        mutate(que = quantiles_zero_out(dif, 0.85)) %>%
        ungroup()
      
      
      # Eu preciso fazer isso para identificar o sentido da viagem!
      vamos_v3 <- vamos_v2 %>%
        # Criar bloco de paradas: cada stop_sequence unico tera um identificador unico
        mutate(bloco_paradas = rleid(stop_sequence)) %>%
        # Agrupar pelos blocos e escolher o ponto de gps que esta a menor distancia daquela parada
        group_by(bloco_paradas) %>%
        slice(which.min(dist)) %>%
        ungroup() %>%
        select(id_gps, id_gps_carro, vehicleid, hora, linha = linha.x, stop_id, 
               stop_sequence, dist, lon.gps, lat.gps, 
               lon.parada, lat.parada, que) %>%
        filter(!(grepl("^\\d+$", stop_sequence) & grepl("^\\d+-[[:lower:]]+$", lag(stop_sequence)) & 
                   grepl("^\\d+-[[:lower:]]+$", lead(stop_sequence)))) %>%
        # Definir o sentido
        # mutate(sentido = ifelse(stop_sequence <= lead(stop_sequence) & lead(stop_sequence) <= lead(stop_sequence, 2),
        #                         "ida",
        #                         ifelse(stop_sequence >= lead(stop_sequence) & lead(stop_sequence) >= lead(stop_sequence, 2), 
        #                                "volta", 
        #                                NA))) %>%
        # Definir sentido: se o "que" (que eh 85 percentil das diferencas entre paradas) for maior que
        # zero, assume-se que a localizacao das paradas esta subindo, entao o sentido da viagem eh "ida"
        # Se for o contrario, assume-se que a viagem eh de volta
        mutate(sentido = ifelse(que > 0, "ida",
                                ifelse(que < 0, "volta", NA))) %>%
        # mutate(sentido = ifelse(stop_sequence == paste0(c(parada_inicial,parada_inicial+1), "-inicio"), "ida",
        #                         ifelse(stop_sequence == paste0(parada_inicial, "-fim"), "volta",
        #                                ifelse(stop_sequence == paste0(parada_final, "-inicio"), "volta",
        #                                       ifelse(stop_sequence == paste0(parada_final, "-fim"), "ida",
        #                                              sentido))))) %>%
        # Corrigir a informacao de sentido nos pontos de inicio e fim de linha: se o ponto for de parada 
        # inicial (ou parada de incial lag) e estiver comecando uma viagem, sera de "ida". E por ai vai...
        mutate(sentido = ifelse(grepl(sprintf("^(%i|%i)-inicio", parada_inicial, parada_inicial+1), stop_sequence), "ida",
                                ifelse(grepl(sprintf("^(%i|%i)-fim", parada_inicial, parada_inicial+1), stop_sequence), "volta",
                                       ifelse(grepl(sprintf("^(%i|%i)-inicio", parada_final, parada_final-1), stop_sequence), "volta",
                                              ifelse(grepl(sprintf("^(%i|%i)-fim", parada_final, parada_final-1), stop_sequence), "ida",
                                                     sentido))))) %>%
        # mutate(sentido = ifelse(grepl("^\\d{1}-inicio" ,stop_sequence), "ida",
        #                         ifelse(grepl("^\\d{1}-fim", stop_sequence), "volta",
        #                                ifelse(grepl("^\\d{2}-inicio", stop_sequence), "volta",
        #                                       ifelse(grepl("^\\d{2}-fim", stop_sequence), "ida",
        #                                              sentido))))) %>%
        # Preencher a informacao de sentido
        fill(sentido) %>%
        # Adicionar id da viagem
        mutate(viagem = rleid(sentido))
      
      # Consertar as inconsistencias e refazer o bloco
      vamos_v4 <- vamos_v3 %>%
        mutate(stop_sequence = as.character(stop_sequence)) %>%
        # Deletar viagens que tenham poucos registros (provavelmente erroneas!)
        group_by(viagem) %>%
        mutate(registros = n()) %>%
        # O registro so vai ser valiado se tiver 75% das paradas e se o primeiro e o ultimo forem do final da linha!
        filter(registros > 0.3 * parada_final & grepl("^\\d{,2}-[[:lower:]]+$", last(stop_sequence)) & 
                 grepl("^\\d{,2}-[[:lower:]]+$", first(stop_sequence))) %>%
        ungroup() %>%
        # Refazer as viagens
        mutate(viagem = rleid(viagem))
      
      
      # ATENCAO: ------------------------------------------------------------------------------------
      # SE QUISER QUE O OUTPUT SEJA O DADO DE GPS COMPLETO COM AS INFORMACOES DE SENTIDO E VIAGEM,
      # DESCOMENTAR O CODIGO ABAIXO. ESSE DADO EH MAIS ADEQUADA PRA FAZER A INTERPOLACAO.
      # OBRIGADO
      
      # Quando comecam e terminam as viagens?
      pontos_de_incio_e_fim <- vamos_v4 %>%
        # Separar somente a primeira e ultima viagem
        group_by(viagem) %>%
        slice(1, n()) %>%
        ungroup() %>%
        # Selecionar colunas
        select(id_gps_carro, stop_sequence, sentido, viagem) %>%
        # Reformular
        group_by(sentido, viagem) %>%
        mutate(stop_sequence = c("inicio", "fim")) %>%
        ungroup()
      
      pontos_de_incio_e_fim_v1 <- pontos_de_incio_e_fim %>%
        spread(stop_sequence, id_gps_carro) %>%
        arrange(viagem)
      
      # Agora, voltar pro meu gps de carros..
      gps_com_sentido_e_viagem <- gps_carro %>%
        left_join(pontos_de_incio_e_fim, by = "id_gps_carro")
      
      # Selecionar so as observacoes que estao entre o inicio e o fim determinado la em cima
      gps_final <- setDT(gps_com_sentido_e_viagem)[id_gps_carro %inrange% 
                                                     pontos_de_incio_e_fim_v1[, c("inicio", "fim")]] %>%
        fill(sentido, viagem) %>%
        mutate(linha_sentido = paste0(linha.x, "-", substr(toupper(sentido), 1, 1))) %>%
        select(id_gps, linha_sentido, vehicleid, hora, viagem, stop_sequence, lon, lat)
      
    }
    
    safe_por_carro <- possibly(por_carro, otherwise = NA_real_)
    tudo_carros <- map(carros_vai_1, safe_por_carro)
    names(tudo_carros) <- carros_vai_1
    return(tudo_carros)
    
  }
  
  # future::plan(multiprocess)
  # tudo_linhas <- furrr::future_map(linhas_total, por_linha)
  tudo_linhas <- map(linhas_total, por_linha)
  names(tudo_linhas) <- linhas_total
  
  # # tirar NAs (from https://stackoverflow.com/questions/25777104/remove-na-from-list-of-lists/25777254)
  # ui <- lapply(tudo_linhas, function(x) x[!is.na(x)]) %>%
  #   # juntar todas os carros! (also check https://www.brodrigues.co/blog/2017-03-24-lesser_known_purrr/)
  #   map_depth(1, bind_rows)
  
  # Salvar em disco
  data_out <- str_extract(gps_com_linhas, "\\d{4}-\\d{2}-\\d{2}")
  path_out <- sprintf("../data/gps_com_sentido/gps_com_sentido_%s.rds", data_out)
  write_rds(tudo_linhas, path_out)
  
}


# APLICAR FUNCAO (PARA UM DIA, POR ENQUANTO) ------------------------------
source("R/setup.R")

files <- dir("../data/gps_linhas", full.names = TRUE, pattern = "gps_linhas_2018-09")

tictoc::tic()
# invisible(parallel::mclapply(files[1:4], gps_para_paradas, mc.cores = 4))
tictoc::toc()
# invisible(parallel::mclapply(files[5:8], gps_para_paradas, mc.cores = 4))
# invisible(parallel::mclapply(files[9:12], gps_para_paradas, mc.cores = 4))
# invisible(parallel::mclapply(files[13:16], gps_para_paradas, mc.cores = 4))
# invisible(parallel::mclapply(files[17:20], gps_para_paradas, mc.cores = 4))
# invisible(parallel::mclapply(files[21:24], gps_para_paradas, mc.cores = 4))
# invisible(parallel::mclapply(files[25:28], gps_para_paradas, mc.cores = 4))
# invisible(parallel::mclapply(files[29:30], gps_para_paradas, mc.cores = 4))
.rs.restartR()
