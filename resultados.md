---
title: "Consolidação dos dados"
output:
  html_document:
    fig_caption: yes
    keep_md: yes
    number_sections: yes
    theme: journal
    toc: yes
    toc_float:
      collapsed: no
      smooth_scroll: no
editor_options:
  chunk_output_type: console
---


```r
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE, error=FALSE, eval = FALSE)

library(sp)
library(ggplot2)
library(dplyr)
library(sf)
# library(mapview)
#library(ggmap) #função geocode() pra extrair as coordenadas dos endereços
library(sf) #pra importar os dados espaciais e tal
library(data.table)
library(knitr)
library(readr)
library(tidyr)
library(hrbrthemes)
library(leaflet)
library(stringr)
library(leaflet.minicharts)
library(purrr)
library(lubridate)
library(mapview)
library(RColorBrewer)
#library(extrafont)
#extrafont::loadfonts(device="win")

options(scipen=10000)


#library(htmltools)
#library(htmlwidgets)

#library(tmap)
```

Esse documento tem o principal objetivo de aplicar a metodologia estabelecida no trabalho. A metodologia pode ser encontrada no trabalho (https://drive.google.com/file/d/0B_DtPz7vN9LqbGZsWmlPSDYtV24xTEJJUVdKWEVTVDNmMVFZ/view?usp=sharing) nos capítulos de Consolidação dos Dados e de Construção do Sistema de Indicadores do SIT-FOR (capítulos 3 e 4, respectivamente). O documento, que aplica a metodologia dos dois capítulos, apresenta os mesmos tópicos encontrados na metodologia, com o código utilizado para realização de cada etapa. 

# Consolidação dos dados

## Tratamento inicial

Para a base da bilhetagem:


```r
#OBSERVACAO IMPORTANTE: se for salvar os arquivos de bilhetagem, utilizar tz= ""
#Esse formato com tz="UTC" so foi utilizado para salvar os arquivos de GPS e garantir que o 
#horario saisse correto

output_column.POSIXct <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%OS", tz = "")
}

# funcao para extrair o valor mais utilizado (moda) --------------------------------------------------

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# tratamento bilhetagem -----------------------------------------------------------------------------------


trataresalvar_bilhetagem <- function(data) {
  # abrir arquivo e drop a ultima coluna
  data <- readr::read_delim(data, delim = ";", col_names = F) %>%
    select(id = X1, linha = X2, nome_linha = X3, prefixo_carro = X4, hora = X5, tipo_cartao = X6, nome_cartao = X7,
           sentido_viagem = X8, integracao = X9) %>%
    mutate(hora = as.POSIXct(hora, format="%d/%m/%Y %H:%M:%S"), #transformar hora para format POSIXct
           dia = lubridate::date(hora), #isola o dia
           momento = strftime(hora, format="%H:%M:%S"),
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
  
  write_csv(data, paste0("data/bilhetagem/2017/bilhetagem_", dia_correto, ".csv"))

}
```

Para os dados de GPS:


```r
# comando unix

# cut -f14,16,17,30,40,42,43 m2m_frota_empresa_db.metric_201701.csv | awk '{ split($3, f, " "); print >  ("gps_" f[1] ".csv")}'
```

Para os dados GTFS:


```r
# Passo 2

# abrir paradas

stops <- read_delim("data/gtfs_2015/stops.txt", delim=",") %>% 
  select(stop_id, stop_name, lon = stop_lon, lat = stop_lat)

stops_sf <- stops %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  %>%
  mutate(id_stop = 1:n())

# abrir stop times

stop_times <- read_delim("data/gtfs_2015/stop_times.txt", delim = ",") %>%
  select(trip_id, stop_id, arrival_time, departure_time, stop_sequence)

# abrir trips

trips <- read_delim("data/gtfs_2015/trips.txt", delim = ",") %>%
  select(trip_id, shape_id)

stops_linhas_vai <- stop_times %>%
  left_join(trips, by = "trip_id") %>%
  count(trip_id, shape_id) %>%
  group_by(shape_id) %>%
  slice(which.max(n))

stops_linhas <- stop_times %>%
  filter(trip_id %in% stops_linhas_vai$trip_id) %>%
  left_join(trips, by = "trip_id") %>%
  mutate(linhaa = as.integer(str_sub(shape_id, 6, 8))) %>%
  mutate(linha_sentido = paste(linhaa, stringr::str_sub(shape_id, 9, 19), sep = "")) %>%
  left_join(stops, by = "stop_id")  %>%
  select(stop_id, linha_sentido, stop_name, stop_sequence, lon, lat)

write_csv(stops_linhas, "gtfs_2015/paradas_consolidadas.csv")
```

Para o endereço dos usuários (still a looooooot of work to do...):


```r
ID = read_delim("data-raw/Linkage Table - BU ID and Card ID_V1.csv", col_names = T, delim=";")

ID_v2 <- ID %>% 
  select(id = CODIGO_USUARIO, endereco = ENDERECO, bairro = BAIRRO) %>%
  mutate(endereco = iconv(endereco, to='WINDOWS-1252')) %>%
  mutate(endereco = tolower(endereco)) %>% #tudo para minusculo
  mutate(primeira_palavra = str_extract(endereco, '\\w*')) %>%
  filter(primeira_palavra != "")

# quais as primeiras palavras mais usadas?

ID_v2 %>%
  count(primeira_palavra) %>%
  arrange(desc(n)) %>%
  View()

primeiras_palavras = c("rua", "r", "r.", "ru",
                       "av", "avenida", "av.",
                       "travessa", "tr", "tv",
                       "Vila", "vl", 
                       "al", "alameda")

ID_v3 <-  ID_v2 %>%
  filter(primeira_palavra %in% primeiras_palavras) %>%
  # corrigir todas as primeiras palavras
  mutate(endereco = sub("r[[:blank:]]", "rua ", endereco)) %>%
  mutate(endereco = sub("r.[[:blank:]]", "rua ", endereco)) %>%
  mutate(endereco = sub("ru[[:blank:]]", "rua ", endereco)) %>%
  mutate(endereco = sub("av[[:blank:]]", "avenida ", endereco)) %>%
  mutate(endereco = sub("av.[[:blank:]]", "avenida ", endereco)) %>%
  mutate(endereco = sub("tr[[:blank:]]", "travessa ", endereco)) %>%
  mutate(endereco = sub("tv[[:blank:]]", "travessa ", endereco)) %>%
  mutate(endereco = sub("vl[[:blank:]]", "vila ", endereco)) %>%
  mutate(endereco = sub("al[[:blank:]]", "alameda ", endereco)) %>%
  # tirar espaços em branco
  mutate(endereco = trimws(endereco, which = "both")) %>%
  # trocar pontuacoes por espaço
  mutate(endereco = gsub("[[:punct:]]", " ", endereco)) %>%
  # manter somente o logradouro
  mutate(endereco_v1 = gsub("^.*? ", "", endereco))

# deletar aqueles que comecam com numeros e so uma letra
ID_v4 = ID_v3 %>% 
  filter(grepl("^\\D{1} ", endereco_v1)) 
  filter(grepl("^\\d+", endereco_v1))
```

## Georreferenciamento da base da bilhetagem


```r
# funcao para integracao

integrar_dados <- function(data1, data2) {
        
    # abrir dados
        
      bilhetagem <- read_csv(data1)
      # bilhetagem <- read_csv("data/bilhetagem/2017/bilhetagem_2017-11-03.csv")
      
      gps <- data.table::fread(data2, header = F) %>%
        setNames(c("lat", "lon", "hora", "odometro", "que", "ui", "vehicleid")) %>%
        mutate(id_gps = 1:n(),
               vehicleid = as.factor(vehicleid)) %>%
        mutate(hora = fasttime::fastPOSIXct(hora, tz="UTC"), #transforma hora
                dia = lubridate::date(hora), #isola o dia
                momento = strftime(hora, format="%H:%M:%S", tz="UTC"))
      
      # gps <- data.table::fread("data/gps/2017/gps_2017-11-03.csv", header = F) %>%
      #   setNames(c("lat", "lon", "hora", "odometro", "que", "ui", "vehicleid")) %>%
      #   mutate(id_gps = 1:n(),
      #          vehicleid = as.factor(vehicleid)) %>%
      #   mutate(hora = fasttime::fastPOSIXct(hora, tz="UTC"), #transforma hora
      #           dia = lubridate::date(hora), #isola o dia
      #           momento = strftime(hora, format="%H:%M:%S", tz="UTC"))
      # - dicionario
      
      dicionario2 <- read.csv("data/dicionario_veiculos.csv", sep = ";", header=TRUE)
      
      # dicionario2 <- dicionario2 %>%
      #   select(vehicleid = id, prefixo_carro = carro) %>%
      #   arrange(prefixo_carro)
      
      dicionario2 <- dicionario2 %>%
        select(vehicleid, prefixo_carro = numbus)
      
      dicionario2$vehicleid <- as.factor(dicionario2$vehicleid)
      
      #isso vai criar coluna vehicleid na bilhetagem
      bilhetagem <- bilhetagem %>% 
        left_join(dicionario2) %>% 
        mutate(vehicleid = as.factor(vehicleid))
      
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
      
      
      #extrair o dia correto  
        dia_correto <- Mode(bilhetagem$dia)
      
      #criar data.table    
        setDT(bilhetagem)
        setDT(gps)
        gps1 <- gps[,.(id_gps,vehicleid, hora)]
      
        setkey(bilhetagem, vehicleid, hora)
        setkey(gps1, vehicleid, hora)
        
        
      
        #funcao para buscar o horario da bilhetagem mais proximo ao horario no gps
        aiai <- gps1[bilhetagem, roll="nearest"] %>% 
          arrange(vehicleid)
      
        # criar coluna com lat e lon na bilhetagem e extrair somente aqueles que nao foram NA's
        aiai_ok <- aiai %>% 
          left_join(select(gps, id_gps, lat, lon), by = "id_gps") %>%
          # left_join(terminais, by = "linha") %>%
          # filter(!is.na(lon)) %>%
          filter(!is.na(id_gps)) %>%
          arrange(vehicleid)
        
        write_csv(aiai_ok, paste0("data/bilhetagem_integrado/2017/bilhetagemintegrado_", dia_correto, ".csv"))

  }


dir_gps <- dir("data/gps/2017", full.names = TRUE, pattern = "*.csv")
dir_bilhetagem <- dir("data/bilhetagem/2017", full.names = TRUE, pattern = "*.csv")



purrr::walk2(dir_bilhetagem[31], dir_gps[50], integrar_dados)

# O QUE FALTA FALZER?
# Falta pegar a localizacao de cada terminal e liga-lo ao numero da linha
```


```r
resultados_geo <- function(bi, bi_integrada) {
  
  bilhetagem <- read_csv(bi)
  bilhetagem_integrada <- read_csv(bi_integrada)
  
  bi_rows <- nrow(bilhetagem)
  bi_integrada_rows <- nrow(bilhetagem_integrada)
  
  vai <- data.frame(bi_vai = bi_rows, bi_integrada_vai = bi_integrada_rows)
}

dir_bi <- dir("data/bilhetagem/2015-03", full.names = T)[c(2,3,4,5,8,9,10,11,12)]
dir_bi_integrado <- dir("data/bilhetagem_integrado", pattern = "*.csv", full.names = T)[c(1:9)]

urra <- map2_dfr(dir_bi, dir_bi_integrado, resultados_geo)

urra %>%
  mutate(dia = c(2, 3, 4, 5, 9, 10, 11, 12, 13),
         dif = bi_vai - bi_integrada_vai) %>%
  gather(key = tipo, value = n, bi_integrada_vai, dif) %>%
  ggplot()+
  geom_col(aes(x = dia, y=n, fill = tipo), position = "stack")+
  geom_text(aes(dia, n, label = n), position = position_stack(vjust = 0.2, reverse = F))+
  scale_x_continuous(breaks = c(2, 3, 4, 5, 9, 10, 11, 12, 12, 13))+
  scale_fill_brewer(palette = "Set2")+
  coord_flip()+
  theme_ipsum_rc(grid = "X")+
  theme(legend.position = "bottom")


read_csv(dir_bi[2]) %>%
  filter(linha %in% c(1, 3, 5, 6, 7, 8, 9, 10)) %>%
  # filter(grepl("terminal | term.", nome_linha, ignore.case = T)) %>%
  count(linha, nome_linha) %>%
  View()
```


## Georreferenciamento dos endereços




## Estimação da parada de embarque dos usuários


```r
# abrir bilhetagem

bilhetagem <- read_csv("data/bilhetagem_integrado/bilhetagemintegrado_2015-03-04.csv") %>%
  arrange(id, hora) %>%
  mutate(linha_sentido = paste(linha, stringr::str_sub(sentido_viagem, 1, 1), sep = "-"))

# abrir bilhetagem paradas

stops_linhas <- read_csv("gtfs_2015/paradas_consolidadas.csv")

# vai

# garantir que todas as linhas da bilhetagem tenham correspondente nas linhas do gtfs e vice-versa
linhas.gtfs <- unique(stops_linhas$linha_sentido)

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

uri <- linhaaa %>% 
  map_dfr(stops_bilhetagem, bilhetagem.correto, stops_linhas) %>%
  arrange(id, hora)

uri %>%
  select(id, hora, linha, nome_linha, prefixo_carro, tipo_cartao, nome_cartao, sentido_viagem, integracao, stop_id, stop_sequence, lon = lon.parada, lat = lat.parada) %>%
  write_csv("bilhetagem_com_paradas_2015-03.csv")
```

## Resumo da consolidação

# Tratamento de dados de GPS

# Método ODX

## Escolha do dia base


```r
bilhetagem_semana <- read_csv("data/bilhetagem_paradas_novo/bilhetagem_com_paradas_2015-03.csv") %>%
  mutate(dia = day(hora))

bilhetagem <- bilhetagem_semana %>%
  mutate(dia = day(hora)) %>%
  filter(dia == "4")

bilhetagem_v1 <- bilhetagem %>%
  filter(id != 0) %>%
  mutate(linha_sentido = paste(linha, substr(sentido_viagem, 1, 1), sep = "-"))
```

Como determinado na metodologia, o dia base escolhido é o dia com a maior quantidade de usuários realizando viagens. Acredita-se que essa escolha é importante para identificar um dia de maior carregamento do sistema. A FIGURA X mostra a distribuição da quantidade usuários viajando por dia dentro do período de análise. É possível identificar o DIA TAL como o dia com maior quantidade de usuários viajando, sendo consequentemente o dia com maior quantidade de viagens. Além disso, esse dia apresenta a maior porcentagem de usuários frequentes do sistema.


```r
# funcao para calcular viagens por id

viagens_por_id <- function(bi) {
  vai <- bi %>%
    group_by(dia, id) %>%
    summarise(n = n())
}

# quais viajantes são frequentes? realiza viagem em pelo menos 4 dias dos 9 dias:

viajantes_frequentes <- bilhetagem_semana %>%
  filter(id != 0) %>%
  count(id, dia) %>%
  count(id) %>%
  filter(nn >= 4)

# qual a porcentagem desses usuários frequentes que viaja em cada dia?

que <- bilhetagem_semana %>%
  filter(id != 0) %>%
  distinct(dia, id) %>%
  left_join(viajantes_frequentes) %>%
  mutate(frequente = ifelse(is.na(nn), "nao", "sim")) %>%
  count(dia, frequente)
  # distinct(dia, id, .keep_all = T)

que %>%
  group_by(dia) %>%
  mutate(soma = sum(n)) %>%
  ungroup() %>%
  mutate(perc = n / soma) %>%
  ggplot() + 
  geom_col(aes(x = dia, y = perc, position = "stack", fill = frequente))
  

viagens_por_dia <- bilhetagem_semana %>%
  filter(id != 0) %>%
  count(dia, id) %>%
  group_by(dia) %>%
  summarise(viajantes = n(), total = sum(n))
  

ora <- bilhetagem_semana %>%
  split(.$dia) %>%
  map_dfr(viagens_por_id)

'%nin%' <- Negate('%in%')
options(scipen=999)


ora_v1 <- ora %>%
  filter(dia %nin% c(6, 14)) %>%
  arrange(id, dia) %>%
  group_by(id) %>%
  summarise(dias_viajados = n(),
            media_viagens = mean(n), 
            desvio_viagens = sd(n))
```

## Completando o padrão de viagens

Como definido na metodologia, é feita uma tentativa de completar as viagens dos usuários que só tiveram registrada uma etapa do deslocamento, sendo feita uma consulta ao histórico. A FIGURA X mostra o resultado da aplicação da metodologia. No total, TANTAS viagens foram completadas e TANTAS viagens foram descartadas da base por não terem sido completadas.

## Estimação de parada e momento de embarque

A Figura X resume o resultado da aplicação do método de estimação de embarque. No total, TANTAS viagens do dia base tiveram seu embarque modificado a partir da consulta ao histórico, e TANTAS mantiveram a mesma parada.

## Estimação da parada e momento de desembarque

A aplicação do __Trip Chaining Method__ com uma distância limite de caminhada de 15 minutos resultou nos valores mostrados na FIGURA TAL.

## Inferência de integração

## Resumo da aplicação do Método ODX

A FIGURA X resume as etapas de método e a quantidade dados estimados em cada uma delas.

# Cálculo de indicadores

