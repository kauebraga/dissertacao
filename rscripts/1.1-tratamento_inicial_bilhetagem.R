library(readr)
library(dplyr)
library(purrr)

# - -----------------------------------------------------------------------
# BILHETAGEM
# - -----------------------------------------------------------------------

Sys.setenv(TZ='UTC') 

output_column.POSIXct <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%OS", tz = "UTC")
}

# funcao para extrair o valor mais utilizado (moda) --------------------------------------------------

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

trataresalvar_bilhetagem <- function(df) {
  # abrir arquivo e drop a ultima coluna
  data <- readr::read_delim(df, delim = ";", col_names = F) %>%
    select(id = X1, linha = X2, nome_linha = X3, prefixo_carro = X4, hora = X5, tipo_cartao = X6, nome_cartao = X7,
           sentido_viagem = X8, integracao = X9) %>%
    mutate(hora = as.POSIXct(hora, format="%d/%m/%Y %H:%M:%S", tz = "UTC"), #transformar hora para format POSIXct
           dia = lubridate::date(hora), #isola o dia
           momento = strftime(hora, format="%H:%M:%S", tz = "UTC"),
           nome_cartao = iconv(nome_cartao, "UTF-8", "WINDOWS-1252")) %>%
    mutate(tipo_cartao = if_else(nome_cartao %in% c("04-VALE TRANSPORTE", "12-VALE-TRANPORTE AVULSO", 
                                                    "08-VT IDENTIFICAÇÃO"), "Vale Transporte",
                                 if_else(nome_cartao %in% c("02-ESTUDANTE ETUFOR", "11-ESTUDANTE COM DEBITO"), 
                                         "Estudante",
                                         ifelse(nome_cartao %in% c("GRATUIDADE IDENTIFIC. ETUFOR", "03-GRATUIDADE IDOSO",
                                                                   "GRAT. DEFICIENTE - C/ACOMP", "GRAT. DEFICIENTE - S/ACOMP"),
                                                "Gratuidade", nome_cartao))))
  
  
  
  # extrair somente o dia certo para salvar o arquivo com o dia correto
  dia_correto <- Mode(as.character(data$dia))
  
  fwrite(data, paste0("../data/bilhetagem/2018/08/bilhetagem_", dia_correto, ".csv"))
  
}


# aplicar -----------------------------------------------------------------


df <- "../data-raw/bilhetagem/2018/11/Viagenssigom20181119.csv"

bi_201811 <- dir("../data-raw/bilhetagem/2018/11/", full.names = TRUE)
bi_201810 <- dir("../data-raw/bilhetagem/2018/10/", full.names = TRUE)
bi_201809 <- dir("../data-raw/bilhetagem/2018/09/", full.names = TRUE)
bi_201808 <- dir("../data-raw/bilhetagem/2018/08/", full.names = TRUE)

walk(bi_201811, trataresalvar_bilhetagem)

# para agosto
invisible(parallel::mclapply(bi_201808, trataresalvar_bilhetagem, 
                             mc.cores = 4))
  