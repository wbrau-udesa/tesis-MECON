# Directorio
setwd('G:/Mi unidad/TESIS ECON/R')

# Librerías
library(tidyverse)
library(readxl)

# Cargo datos
load('eph_ord.RData')
load('pond_identificadores.RData')
variables_usadas <- read_excel("G:/Mi unidad/TESIS ECON/Sobre la EPH/variables usadas.xlsx", 
                               col_types = c("skip", "text", "text", 
                                             "text", "text", "text", "skip"))


# eph_complete -------------

# Nos quedamos sólo con las observaciones con datos completos para TODAS las variables
nrow(eph[complete.cases(eph),]) / nrow(eph)
eph_complete = eph[complete.cases(eph),]

# Volvemos a eliminar variables sin variabilidad ahora que sacamos los NA
sin_variab = colnames(eph_complete)[apply(eph_complete, 2, FUN=function(x) length(unique(x))<2)]
eph_complete = eph_complete[!colnames(eph_complete) %in% sin_variab]
sin_variab
rm(sin_variab)

# guardamos
saveRDS(eph_complete, file = 'eph_complete.rds')

# eph_NA----------------
# 0 a la variable y dummy indicando NA

fliar = c('DECIFR','RDECIFR','GDECIFR','ADECIFR','DECCFR','RDECCFR','GDECCFR','ADECCFR','ITF','IPCF')
eph_NA = eph
eph_NA$NA_fliar = 0
eph_NA$NA_fliar[is.na(eph_NA$ITF)] = 1
mean(eph_NA$NA_fliar)
eph_NA[fliar][is.na(eph_NA[fliar])] = 0
rm(fliar)

total_ind = c('DECINDR','RDECINDR','GDECINDR','ADECINDR')
eph_NA$NA_total_ind = 0
eph_NA$NA_total_ind[is.na(eph_NA$DECINDR)] = 1
mean(eph_NA$NA_total_ind)
eph_NA[total_ind][is.na(eph_NA[total_ind])] = 0
rm(total_ind)

princip_ind = c('P21','DECOCUR','RDECOCUR','GDECOCUR','ADECOCUR')
eph_NA$NA_princip_ind = 0
eph_NA$NA_princip_ind[is.na(eph_NA$P21)] = 1
mean(eph_NA$NA_princip_ind)
eph_NA[princip_ind][is.na(eph_NA[princip_ind])] = 0
rm(princip_ind)

# qué otras variables tienen muchos NAs
options(scipen=999)
sort(apply(eph_NA,2,FUN = function(x) mean(is.na(x))*100), decreasing = T)[1:10]

eph_NA$NA_PP08D = 0
eph_NA$NA_PP08D[is.na(eph_NA$PP08D)] = 1
eph_NA$PP08D[is.na(eph_NA$PP08D)] = 0

eph_NA$NA_PP07D = 0
eph_NA$NA_PP07D[is.na(eph_NA$PP07D)] = 1
eph_NA$PP07D[is.na(eph_NA$PP07D)] = 0

eph_NA$NA_jefe_CALIFICACION_ord = 0
eph_NA$NA_jefe_CALIFICACION_ord[is.na(eph_NA$jefe_CALIFICACION_ord)] = 1
eph_NA$jefe_CALIFICACION_ord[is.na(eph_NA$jefe_CALIFICACION_ord)] = 0

eph_NA$NA_TOT_P12 = 0
eph_NA$NA_TOT_P12[is.na(eph_NA$TOT_P12)] = 1
eph_NA$TOT_P12[is.na(eph_NA$TOT_P12)] = 0

eph_NA$NA_V2_M = 0
eph_NA$NA_V2_M[is.na(eph_NA$V2_M)] = 1
eph_NA$V2_M[is.na(eph_NA$V2_M)] = 0

nrow(eph_NA[complete.cases(eph_NA),]) / nrow(eph_NA) # ahora podemos conservar el 83% de las observaciones
eph_NA = eph_NA[complete.cases(eph_NA),]


# Volvemos a eliminar variables sin variabilidad ahora que sacamos los NA
sin_variab = colnames(eph_NA)[apply(eph_NA, 2, FUN=function(x) length(unique(x))<2)]
sin_variab
eph_NA = eph_NA[!colnames(eph_NA) %in% sin_variab]
rm(sin_variab)

# guardamos
saveRDS(eph_NA, file = 'eph_NA.rds')





