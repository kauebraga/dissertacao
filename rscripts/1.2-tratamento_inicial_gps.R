library(data.table)
library(dplyr)
library(stringr)
library(readr)
library(fasttime)
library(lubridate)

options(scipen = 999)



# arq <- dir("data-raw/gps/2018", full.names = T, pattern = "*.csv")
# 
# 
# sep_gps <- function(file) {
#   
#   data <- fread(file, select = c(2, 3, 4, 9)) %>%
#     mutate(vai = gsub("(\\d{4})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})", "\\1-\\2-\\3 \\4:\\5:\\6", metrictimestamp, perl = TRUE)) %>%
#     mutate(vai = fastPOSIXct(vai, tz = "UTC"))
#   
#   # mes <- substr(file, 24, 25)
#   # 
#   # data %>%
#   #   select(hora = vai, lon = longitude, lat = latitude, vehicleid = vehicle_vehicleid) %>%
#   #   write_csv(paste0("E:/kaue/projetos/dissertacao/data-raw/gps/2018/tratado/gps_2018", "-", mes, ".csv"))
#   
# }
# 
# 
# 
# purrr::walk(arq[11:12], sep_gps)
# 
# 
# sep_gps("../data-raw/gps/2018/")
# 
# 
# # PARA ALGUNS MESES:
# 
# data <- fread("../data-raw/gps/2018/Paint082018.csv")
# 
# 
# data1 <- data %>%
#   # mutate(as.character(metrictimestamp)) %>%
#   filter(metrictimestamp > 20180000000000)
#   # filter(startsWith("201808", metrictimestamp))
# 
# # AGOSTO DE 2018 PARECE OK!
# 
# # Para julho de 2018
# 
# data <- fread("../data-raw/gps/2018/Paint072018.csv")
# 
# data1 <- data %>%
#   # mutate(as.character(metrictimestamp)) %>%
#   filter(metrictimestamp < 20180700000000)
# 
# data1 <- data[1:100, ]
# data2 <- data1[, hora := as.numeric(metrictimestamp)]
# data2 <- data2[, hora1 := as.character(metrictimestamp)]
# data2 <- data1[, hora2 := ifelse(metrictimestamp < 20180700000000, as.POSIXct(hora, origin = "2000-01-01"),
#                                 as.POSIXct(hora1, format = "%Y%m%d%H%M%S"))]
# 
# 
# data_fim <- data %>%
#   slice(1:100)
#   # mutate(hora1 = ifelse(metrictimestamp < 20180700000000, as.POSIXct(as.numeric(metrictimestamp), origin = "2000-01-01"), 
#   #                       as.POSIXct(as.character(metrictimestamp), format = "%Y%m%d%H%M%S")))
#   # # mutate(vai = fastPOSIXct(vai, tz = "UTC"))
#   # # filter(!(grepl("20180801\\d{6}", metrictimestamp))) %>%
#   # # mutate(metrictimestamp = as.numeric(metrictimestamp)) %>%
#   # # mutate(ai = as.POSIXct(metrictimestamp, origin = "2000-01-01"))
#   # select(hora, lon = longitude, lat = latitude, vehicleid = vehicle_vehicleid)
#   
# data_tipo1 <- data_fim %>%
#   mutate(id = 1:n()) %>%
#   filter(metrictimestamp > 20180700000000) %>%
#   mutate(hora = as.POSIXct(as.character(metrictimestamp), format = "%Y%m%d%H%M%S", tz = "UTC"))
# 
# data_tipo2 <- data_fim %>%
#   mutate(id = 1:n()) %>%
#   filter(metrictimestamp <= 20180700000000) %>%
#   mutate(hora = as.POSIXct(as.numeric(metrictimestamp), origin = "2000-01-01", tz = "UTC")) %>%
#   # mutate(hora = hora + hms::hms(3, 0, 0)) %>%
#   identity()
# 
# 
# oooi <- rbind(data_tipo1, data_tipo2) %>%
#   arrange(id)
# 
# # em data.tablês
# 
# data_tipo1 <- data[, id := 1:nrow(data)]
# data_tipo1 <- data_tipo1[metrictimestamp > 20180000000000]
# # esse
# data_tipo1 <- data_tipo1 %>% mutate(hora = as.POSIXct(as.character(metrictimestamp), format = "%Y%m%d%H%M%S", tz = "UTC"))
# # ou esse (NAO FUNCIONA!)
# setDT(data_tipo1)
# data_tipo1 <- data_tipo1[, hora := as.POSIXct(as.character(metrictimestamp), format = "%Y%m%d%H%M%S", tz = "UTC")]
# 
# 
# 
# 
# data_tipo2 <- data[, id := 1:nrow(data)]
# data_tipo2 <- data_tipo2[metrictimestamp <= 20180000000000]
# data_tipo2 <- data_tipo2[, hora := as.POSIXct(as.numeric(metrictimestamp), origin = "2000-01-01", tz = "UTC")]
# 
# oooi <- rbindlist(list(data_tipo1, data_tipo2))
# oooi <- oooi[order(id)]
# 
# 
# 
# oooi %>%
#   select(hora, lon = longitude, lat = latitude, vehicleid = vehicle_vehicleid) %>%
#   fwrite("gps_2018-07.csv")



# FUNCAO !!!!!!!!! --------------------------------------------------------

# essa funcao oferece como output o arquivo de gps do mês com a coluna "hora" já tratada, pronta para ser dividia por dia através dos comandos no linux

pre_tratar_gps <- function(gps, tz = "UTC") {
  
  data <- fread(gps, select = c(2, 3, 4, 9))
  data <- data[, id := 1:nrow(data)]
  
  data_tipo1 <- data[metrictimestamp > 20180000000000]
  data_tipo2 <- data[metrictimestamp <= 20180000000000]
  
  if (nrow(data_tipo2) > 0) {
    
    data_tipo1 <- data_tipo1 %>% mutate(hora = as.POSIXct(as.character(metrictimestamp), format = "%Y%m%d%H%M%S", tz = tz))
    data_tipo2 <- data_tipo2 %>% mutate(hora = as.POSIXct(as.numeric(metrictimestamp), origin = "2000-01-01", tz = tz))
    
    oooi <- rbindlist(list(data_tipo1, data_tipo2))
    oooi <- oooi[order(id)]
    
  } else {
    
    data_tipo1 <- data_tipo1 %>% mutate(hora = as.POSIXct(as.character(metrictimestamp), format = "%Y%m%d%H%M%S", tz = tz))
    oooi <- data_tipo1
    
  }
  
  oooi <- oooi %>%
    mutate(hora = with_tz(hora, "America/Fortaleza")) %>%
    mutate(hora = force_tz(hora, "UTC"))
  
  setDT(oooi)
  
  oooi <- oooi[, dia := as.IDate(hora)]
  
  oooi[, fwrite(.SD, paste0("../data-raw/gps/2018/gps_", dia,".csv")), 
      by = dia]
  
}

Sys.setenv(TZ='GMT')

pre_tratar_gps("../data-raw/gps/2018/Paint082018.csv")
beepr::beep()
# head(opa)
# fwrite(opa, "../data-raw/gps/2018/tratado_novo/gps_2018-10.csv")


# TESTE -------------------------------------------------------------------

setDT(opa)
opa <- opa[, dia := as.IDate(hora)]

opa[, fwrite(.SD, paste0("gps_", dia,".csv")), 
          by = dia]


# TESTE FEATHER! ----------------------------------------------------------

# install.packages("feather")
library(feather)

opa[, write_feather(.SD, paste0("../data-raw/gps/2018/gps_", dia,".feather")), by = "dia"]

opa1 <- read_feather("../data-raw/gps/2018/gps_2018-10-01.feather")

str(opa1)


# ABRIR ARQUIVOS PARA CHECAGEM --------------------------------------------

opa <- fread("../data-raw/gps/2018/tratado_novo/10/gps_2018-10-01.csv")

head(vai)

# OOKKKKKKKKKKKK!!!

# VERIFICAR SE A HORA ESTÁ OK!!!!!!!! -------------------------------------



summary(opa$hora)

opa1 <- opa %>%
  mutate(dia = day(hora)) %>%
  mutate(hora1 = hora - hms("03:00:00")) %>%
  filter(dia == 6)

library(ggplot2)

ggplot(opa1)+
  geom_histogram(aes(hora1))

# abrir outro mes

vai <- fread("../data-raw/gps/2018/tratado_novo/gps_2018-10.csv") %>%
  mutate(hora = fastPOSIXct(hora))

summary(vai$hora)
  
va1 <- vai %>%
  mutate(dia = day(hora)) %>%
  # mutate(hora1 = hora - hms("03:00:00")) %>%
  filter(dia == 8)

ggplot(va1)+
  geom_histogram(aes(hora))





# abrir outro mes

vai <- fread("../data-raw/gps/2018/tratado_novo/gps_2018-11.csv")

vai1 <- vai %>%
  mutate(hora1 = fastPOSIXct(hora, tz = "America/Fortaleza")) %>%
  # mutate(hora1 = hora1 - hms("03:00:00")) %>%
  identity()

head(vai1, 100)

summary(vai1$hora1)

vai2 <- vai1 %>%
  mutate(dia = day(hora1)) %>%
  filter(dia == 8)

ggplot(vai2)+
  geom_histogram(aes(hora1))+
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour")
