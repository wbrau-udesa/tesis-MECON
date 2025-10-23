# Directorio
setwd('G:/Mi unidad/TESIS ECON/R')

# Librerías
library(tidyverse)
library(readxl)
library(fastDummies)
library(homals)
library(aspect)

# Cargo datos
eph <- readRDS('eph_complete.rds')
load('pond_identificadores.RData')
variables_usadas <- read_excel("G:/Mi unidad/TESIS ECON/Sobre la EPH/variables usadas.xlsx", 
                               col_types = c("skip", "text", "text", 
                                             "text", "text", "text", "skip","skip"))
table(variables_usadas$Tipo, variables_usadas$Bienestar)

# Encadenadas ---------------

ncol(eph)
eph = eph
Nombre_NA = names(select(eph,contains('NA_')))
colnames(eph)[!colnames(eph) %in% c(variables_usadas$Nombre, Nombre_NA)] # pond, identificadores, y las que empiezan con NA

# Tipos de variable
unique(variables_usadas$Tipo)
num = variables_usadas$Nombre[variables_usadas$Tipo == "num"]
ord = variables_usadas$Nombre[variables_usadas$Tipo == "ordinal"]
categ = c(variables_usadas$Nombre[variables_usadas$Tipo == "categ"], Nombre_NA)

# Eliminar la indicadora de -999 (pues una categoria por raiz o hija ya fue eliminada)
nums <- unlist(lapply(eph, is.numeric))  
tienen9 = names(eph[,nums])[apply(eph[,nums], 2, FUN= function(x) mean(x < -100) > 0)]
for (var in tienen9){
  print(variables_usadas$Tipo[variables_usadas$Nombre==var])
  print(unique(eph[var]))
}
eph[,nums][eph[,nums] < -900] = 0
rm(nums, tienen9, var)



# DUMMIES ---------------
eph_dummies = dummy_cols(eph, 
                         select_columns = c(categ, ord),
                         remove_first_dummy = TRUE)

# base para PCA sin identificadores ni ponderadores
eph_dummies = eph_dummies[!colnames(eph_dummies) %in% c(identificadores,pond, 'IMPUTA')]
saveRDS(eph_dummies, file = 'eph_dummies.rds')

# ASPECT ---------------
# base para PCA sin identificadores ni ponderadores
eph_scaled = eph[!colnames(eph) %in% c(identificadores,pond, 'IMPUTA')]
eph_scaled = eph_scaled[variables_usadas$Nombre]
unique(variables_usadas$Tipo)
variables_usadas$Tipo[variables_usadas$Tipo=="categ"] = "nominal"
variables_usadas$Tipo[variables_usadas$Tipo=="num"] = "numerical"
unique(variables_usadas$Tipo)

# categóricas a factores
str(eph_scaled[categ])
for (var in categ){
  eph_scaled[,var] = factor(eph_scaled[,var])
}
rm(var)
str(eph_scaled[categ])

# ordinales a enteros
str(eph_scaled[c(ord)])
for (var in ord){
  eph_scaled[,var] = as.integer(eph_scaled[,var])
}
rm(var)
str(eph_scaled[ord])

# aspect
scaled_categord = corAspect(eph_scaled[c(categ, ord)], 
                          aspect='aspectEigen', 
                          level = c( rep('nominal',length(categ)),
                                     rep('ordinal',length(ord))),
                          itmax=100)

# vemos cómo codificó las categorías
scaled_categord$scoremat
scaled_categord$catscores[['ESTADO']] #nominales como estado, en desorden
scaled_categord$catscores[['materiales']] # ordinales: mantiene el orden pero cambia las distancias
saveRDS(scaled_categord$catscores, file='catscores.rds')

# eph_scaled
eph_scaled = cbind(scaled_categord$scoremat,eph_scaled[num])
summary(eph_scaled)
saveRDS(eph_scaled, file='eph_scaled.rds')
