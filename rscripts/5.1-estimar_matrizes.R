source("R/criar_script_python_parallel_multiple.R")
source("R/setup.R")
library(opentripplanner)


# CONSTRUIR O GRAPH ---------------------------------------------------------------------------

construir_graph <- function(cidade) {
  
  # Os arquivos de gtfs e .obj devem estar na pasta "cidade"
  
   opentripplanner::otp_build_graph(otp = "../otp/programs/otp.jar", dir = "../otp", router = cidade,
                                    memory = 6) 
  
}

# Aplicar
construir_graph("forpadrao")
construir_graph("forcorrigidocm")
construir_graph("forcorrigidoce")

# cidade <- "forcorrigidocm"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCAO PARA APLICAR O OTP !!!!!!!!!!!!!
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

aplicar_otp <- function(cidade) {
  
  py_nome <- dir("../otp/py", pattern = sprintf("otp_%s", cidade))[1] 
  
  comando <- sprintf("cd ../otp && java -jar programs/jython.jar -Dpython.path=programs/otp.jar py/%s", py_nome)
  
  shell(comando)
  
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCAO PARA COLAR OS ARQUIVOS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# cidade <- "forcorrigidocm"

colar_arquivos_otp <- function(cidade) {
  
  # colar os arquivos
  
  # pegar os arquivos
  files <- dir("../data/output_ttmatrix", 
               pattern = sprintf("^ttmatrix_%s_pt", cidade),
               full.names = TRUE)
  
  # extrair os horarios
  horarios <- str_extract(files, "\\d{1,2}-\\d{1,2}") %>% unique()
  
  # funcao para abrir e juntar os arquivos de cada horario
  
  # horarios1 <- horarios[1]
  
  abrir_e_juntar <- function(horarios1) {
    
    files_ok <- dir("../data/output_ttmatrix", 
                    pattern = sprintf("^ttmatrix_%s_pt_%s", cidade, horarios1),
                    full.names = TRUE)
    
    # abrir, juntar e salvar arquivos
    path_out <- sprintf("../data/ttmatrix_fim/ttmatrix_%s_%s.csv", cidade, horarios1)
    
    furrr::future_map(files_ok, fread) %>%
      rbindlist() %>%
      fwrite(path_out)
    
    # # remove files?
    # walk(files_ok, file.remove)
  }
  
  # aplicar funcao
  plan(multiprocess)
  invisible(furrr::future_map(horarios, abrir_e_juntar))
  
  
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# APLICACAO DA FUNCAO PARA ESTIMACAO DAS MATRIZES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ESTIMAR MATRIZ PARA GTFS PADRAO -------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Escolher dia
dia <- "2018-09-10"

# Criar arquivo em python em paralelo, entre 7 e 9 da manha, e para todos os modos
criar_script_python_paral_modes("forpadrao",
                                modo = "tp",
                                data = dia,
                                from = 6, 
                                until = 8, 
                                every = 15)
      
# aplicar otp para todos os modos
aplicar_otp("for_padrao", data = dia, res = "09")


    

# ESTIMAR MATRIZ PARA GTFS CORRIGIDO 50 ------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Escolher dia
dia <- "2018-09-10"

# Criar arquivo em python em paralelo, entre 7 e 9 da manha, e para todos os modos
criar_script_python_paral_modes("forcorrigidocm", 
                                data = dia, 
                                modo = "tp", 
                                from = 7, 
                                until = 8, 
                                every = 15)

# aplicar otp para todos os modos
aplicar_otp("forcorrigidocm")


# ESTIMAR MATRIZ PARA GTFS CORRIGIDO 85 ------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Escolher dia
dia <- "2018-09-10"

# Criar arquivo em python em paralelo, entre 7 e 9 da manha, e para todos os modos
criar_script_python_paral_modes("forcorrigidoce", 
                                data = dia, 
                                modo = "tp", 
                                from = 6, 
                                until = 8, 
                                every = 15)

# aplicar otp para todos os modos
aplicar_otp("forcorrigidoce")


# APLICA FUNCOES PARA COLAR AQUIVOS ------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# para gtfs padrao
colar_arquivos_otp("forpadrao")

# para gtfs corrigido p50
colar_arquivos_otp("forcorrigidocm")

# para gtfs padrao
colar_arquivos_otp("forcorrigidoce")





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# COMPARAR OS TEMPOS DE VIAGEM ESTIMADOS ------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

teste_corrigido1 <- fread("../data/ttmatrix/ttmatrix_for_corrigido_pt_08_7-0.csv") %>%
  mutate(par = paste0(origin, "-", destination)) %>%
  select(par, travel_time)

teste_corrigido2 <- fread("../data/ttmatrix/ttmatrix_for_corrigido_pt_08_7-15.csv") %>%
  mutate(par = paste0(origin, "-", destination)) %>%
  select(par, travel_time)

teste_corrigido3 <- fread("../data/ttmatrix/ttmatrix_for_corrigido_pt_08_7-30.csv") %>%
  mutate(par = paste0(origin, "-", destination)) %>%
  select(par, travel_time)

teste_corrigido4 <- fread("../data/ttmatrix/ttmatrix_for_corrigido_pt_08_7-45.csv") %>%
  mutate(par = paste0(origin, "-", destination)) %>%
  select(par, travel_time)

# Comparar o corrigo para tempos de viagem diferente
problematico <- "8880104f05fffff-8880104d49fffff"	
problematico <- "8880104f01fffff-8880104d49fffff"	
problematico <- "8880104f0dfffff-8880104e09fffff"	
problematico <- "8880104c39fffff-88801041b1fffff"	

teste_corrigido1 %>% filter(par == problematico)
teste_corrigido2 %>% filter(par == problematico)
teste_corrigido3 %>% filter(par == problematico)
teste_corrigido4 %>% filter(par == problematico)

teste_prog1 <- fread("../data/ttmatrix/ttmatrix_for_padrao_pt_08_7-0.csv") %>%
  mutate(par = paste0(origin, "-", destination)) %>%
  select(par, travel_time)

teste_prog2 <- fread("../data/ttmatrix/ttmatrix_for_padrao_pt_08_7-15.csv") %>%
  mutate(par = paste0(origin, "-", destination)) %>%
  select(par, travel_time)

teste_prog3 <- fread("../data/ttmatrix/ttmatrix_for_padrao_pt_08_7-30.csv") %>%
  mutate(par = paste0(origin, "-", destination)) %>%
  select(par, travel_time)

teste_prog4 <- fread("../data/ttmatrix/ttmatrix_for_padrao_pt_08_7-45.csv") %>%
  mutate(par = paste0(origin, "-", destination)) %>%
  select(par, travel_time)


teste_prog1 %>% filter(par == problematico)
teste_prog2 %>% filter(par == problematico)
teste_prog3 %>% filter(par == problematico)
teste_prog4 %>% filter(par == problematico)

join <- full_join(teste_prog1, teste_corrigido1, by = "par", suffix = c(".prog", ".real")) %>%
  mutate(dif = travel_time.real - travel_time.prog)

boxplot(join$dif)
summary(join$dif)
    
join %>%
  ggplot()+
  # ggbeeswarm::geom_beeswarm(aes(x = 1, y = dif))
  geom_boxplot(aes(x = 1, y = dif), outlier.colour=rgb(.5,.5,.5, alpha=0.2))+
  theme_ipsum_rc()

# Quais sao as maiores diferenças?
join %>%
  separate(par, into = c("origem", "destino"), remove = FALSE) %>%
  arrange(desc(abs(dif))) %>%
  slice(1:1000) %>%
  add_count(origem) %>%
  View()

# Quais as diferencas maiores que 15 minutos?

join %>%
  separate(par, into = c("origem", "destino")) %>%
  filter(abs(dif) > 900) %>%
  add_count(origem) %>%
  View()

join %>%
  separate(par, into = c("origem", "destino")) %>%
  filter(origem == "8880104f05fffff") %>%
  View()

# Pontos mais problematicos


# Trazer os pontos
points <- fread("../otp/points/points_for_corrigido_08.csv") %>%
  to_spatial(c("X", "Y"))
    
points %>% filter(id_hex == "8880104f47fffff") %>%
  mapview()
    
points %>% filter(id_hex == "8880104cd5fffff") %>%
  mapview()
