
# SCRIPT PARA TRATAMENTO DE DADOS DO USO DO SOLO ----------------------------------------------




# DADOS DE SAUDE ------------------------------------------------------------------------------

# abrir estabelecimentos ativos
cnes_ativos <- fread("../data-raw/saude/cnes_ativonone.csv.csv")

# abrir todos
cnes <- fread("../data-raw/saude/cnesnone.csv.csv")

cnes_ativos %>% count(ds_tipo_unidade, sort = T) %>% View()

# Fazer a juncao das bases
cnes_fim <- cnes %>%
  left_join(cnes_ativos %>% select(co_cnes, ds_tipo_unidade)) %>%
  # Selecionar so as unidades de interesse
  filter(ds_tipo_unidade %in% c("CENTRO DE SAUDE/UNIDADE BASICA",
                                "UNIDADE DE APOIO DIAGNOSE E TERAPIA (SADT ISOLADO)",
                                "POSTO DE SAUDE",
                                "POLICLINICA",
                                "HOSPITAL GERAL",
                                "PRONTO ATENDIMENTO",
                                "PRONTO SOCORRO GERAL",
                                "PRONTO SOCORRO ESPECIALIZADO",
                                "CENTRO DE ATENCAO HEMOTERAPIA E OU HEMATOLOGICA",
                                "LABORATORIO DE SAUDE PUBLICA",
                                "UNIDADE MISTA",
                                "CENTRO DE APOIO A SAUDE DA FAMILIA",
                                "UNIDADE DE VIGILANCIA EM SAUDE",
                                "CENTRAL DE GESTAO EM SAUDE"))

# salvar
write_rds(cnes_fim, "../data/saude/saude_ativo_2015.rds")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RAIS ----------------------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Abrir
rais <- fread("../data-raw/rais/rais_2017_raw.csv")

# Filtrar Fortaleza
rais <- rais[cidade == "FORTALEZA"]

# Limpeza de estabelecimentos com total de empregos outliers _ do codigo do Bruno ------------------

# Extrair o cnae do setor
rais <- setDT(rais)[, cnae.setor := substr(clas_cnae10, 1, 2)]

# Extrair os setores desejados 
rais.problema <- setDT(rais)[cnae.setor %in% c("40","41","60","62","74","90")]

# Extrair somente os que tem vinculos ativos
# hist(rais.problema$qt_vinc_ativos)
# summary(rais.problema$qt_vinc_ativos)
rais.problema <- rais.problema[qt_vinc_ativos > 0]
# dessas, umas 7 mil n??o tinham v??nculos ativos

# o valor no percentil 90
quanti<-function(x){quantile(x,probs = 0.9)}
# quantidade acima do percentil 90
quant<-function(x){sum(x>=quantile(x,probs = 0.9))}
# interquantile range s?? dos acima do percentil 90
IQa<-function(x){IQR(x[x>=quantile(x,probs = 0.90)])}
IQb<-function(x){3}
#O valor do percentil 90 somado a 3 vezes o valor do interquantile range
IQe<-function(x){quantile(x,probs = 0.90)+IQR(x[x>=quantile(x,probs = 0.90)])*3}
#quantidade de casos acima desse threshold
IQf<-function(x){sum(x>quantile(x,probs = 0.90)+IQR(x[x>=quantile(x,probs = 0.90)])*3)}

iqa<-aggregate(rais.problema$qt_vinc_ativos, by=list(rais.problema$cnae.setor), IQa)
q<-aggregate(rais.problema$qt_vinc_ativos, by=list(rais.problema$cnae.setor), quanti)
qq<-aggregate(rais.problema$qt_vinc_ativos, by=list(rais.problema$cnae.setor), quant)
iqb<-aggregate(rais.problema$qt_vinc_ativos, by=list(rais.problema$cnae.setor), IQb)
iqe<-aggregate(rais.problema$qt_vinc_ativos, by=list(rais.problema$cnae.setor), IQe)
iqf<-aggregate(rais.problema$qt_vinc_ativos, by=list(rais.problema$cnae.setor), IQf)

# vai agregar essas informa????es na base


# rais.problema<-data.table(rais.problema)
rais.problema[,p90:=quantile(qt_vinc_ativos,0.90),by=cnae.setor]
geral<-cbind.data.frame(q,qq[,2],iqa[,2],iqb[,2],iqe[,2],iqf[,2])
names(geral)<-c("cnae.setor","quantil","freq","desviointerq","fator","corte","outliers")

rais.problema2 <- merge(rais.problema, setDT(geral), 
                        all.x = TRUE)

rais.problema2$diff<-rep(0,nrow(rais.problema2))

rais.problema2$diff[rais.problema2$qt_vinc_ativos>=rais.problema2$corte] <- 
  rais.problema2$qt_vinc_ativos[rais.problema2$qt_vinc_ativos>=rais.problema2$corte] - rais.problema2$corte[rais.problema2$qt_vinc_ativos>=rais.problema2$corte]

dif<-aggregate(rais.problema2$diff, by=list(rais.problema2$cnae.setor), sum)

geral2<-cbind.data.frame(q,qq[,2],iqa[,2],iqb[,2],iqe[,2],iqf[,2],dif[,2])

names(geral2)<-c("cnae.setor","q","freq","desviointerq","fator","corte","outlier","perda")

# criando nova vari??vel com valores de outliers corrigidos
rais$qt_vinc_ativos2<-rais$qt_vinc_ativos

#zerando empregos de administra????o p??blica
rais$qt_vinc_ativos2<-ifelse(rais$cnae.setor=="75",0,rais$qt_vinc_ativos2)

#tabela com valor de corte por setor
geral3<-geral2[,c(1,6)]

#colocando esse valor de corte na base
rais <- merge(rais, geral3, 
              by="cnae.setor",
              all.x = TRUE)

#substituindo valores maiores que o corte pelo valor de corte
rais$qt_vinc_ativos2<-ifelse(rais$cnae.setor %in% c("40","41","60","62","74","90") & rais$qt_vinc_ativos2>rais$corte,rais$corte,rais$qt_vinc_ativos2)

# corrigir coordenadas
rais[, ':='(lon = str_replace(lon, ",", "."),
            lat = str_replace(lat, ",", "."))]

rais[, ':='(lon = as.numeric(lon),
            lat = as.numeric(lat))]

rais_fim <- rais[qt_vinc_ativos2 > 0]

# Salvar
write_rds(rais_fim, "../data/rais/rais_2017_corrigido.rds")
