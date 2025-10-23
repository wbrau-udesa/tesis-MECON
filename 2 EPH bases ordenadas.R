setwd('G:/Mi unidad/TESIS ECON/R')

library(tidyverse)
library(readxl)

load('eph_limpia.RData')
load('pond_identificadores.RData')


# eph EDO, SOSA & SVARC---------------
## ---- Income, property and wealth ----
# Propietario de la vivienda
unique(eph$II7)
table(eph$II1, eph$II7) 
eph$II7_propietario = 0
eph$II7_propietario[eph$II7 %in% c(1,2)] = 1
unique(eph$II7_propietario)


# Para subsidios invierto el orden de la dummy: 1 es que no neceesita subsidios o ayuda social
eph$V5 = eph$V5 * (-1) + 1
### Para comprar en cuotas invierto el orden de la dummy: 1 si no 
#eph$V16 = eph$V16*(-1) + 1
#NO! Que V16 MAS POSITIVA SEA COMPRAR MAS EN CUOTAS

## ---- Empleo y educación del cabeza de familia ----
jefes = eph %>%
  group_by(CODUSU_NRO_HOGAR)%>%
  summarise(jefes = sum(CH03 ==1))
min(jefes$jefes)
max(jefes$jefes) # todos los hogares tienen a lo sumo un jefe
sum(jefes$jefes==0) # hay 9 hogares sin jefe de hogar
rm(jefes)


unique(eph$CH09)
unique(eph$NIVEL_ED)
unique(eph$CALIFICACION)
# Transformo calificación en una variable ordenada
table(eph$ESTADO,eph$CALIFICACION)
# 0 si no corresponde (Inactivo, o desocupado sin empleo anterior)
# 0 si es no calificado
# 1 operativo
# 2 técnico
# 3 profesional
eph$CALIFICACION_ord = 0
eph$CALIFICACION_ord[eph$CALIFICACION=='Operativos'] = 1
eph$CALIFICACION_ord[eph$CALIFICACION=='Técnicos'] = 2
eph$CALIFICACION_ord[eph$CALIFICACION == 'Profesionales'] = 3
eph$CALIFICACION_ord[is.na(eph$CALIFICACION)] = NA
eph = eph%>%select(!CALIFICACION)

jefes = eph %>%
  filter(CH03 == 1) %>%
  select(CODUSU_NRO_HOGAR, CH09, NIVEL_ED, ESTADO, CALIFICACION_ord) %>%
  summarise(CODUSU_NRO_HOGAR = CODUSU_NRO_HOGAR,
            jefe_ocupado = ifelse(ESTADO == 1, 1, 0),
            jefe_CALIFICACION_ord = CALIFICACION_ord,
            jefe_CH09 = CH09,
            jefe_NIVEL_ED = NIVEL_ED)
eph = merge(eph,
      jefes, by = 'CODUSU_NRO_HOGAR', all.x=T)
rm(jefes)

## ---- Dwelling characteristics ----
# a partir del índice de privación material de los hogares y otros del mismo doc

# Materiales (piso y techo) 

unique(eph$IV3)
eph$piso = NA
eph$piso[eph$IV3 == 3] = 0 # insuf
eph$piso[eph$IV3 == 2] = 1 # parcialm suf
eph$piso[eph$IV3 == 1] = 2 # suf
eph$piso[eph$IV3 == 4] = 2
unique(eph$piso)

unique(eph$IV4) # no hay 7
unique(eph$IV5) # no hay 7
eph$techo = NA
eph$techo[eph$IV4 %in% c(1,2,3)  & eph$IV5 == 1] = 2
eph$techo[eph$IV4 %in% c(4,5)  & eph$IV5 == 1] = 2
eph$techo[is.na(eph$IV4)  & eph$IV5 == 1] = 2
eph$techo[eph$IV4 %in% c(1,2,3)  & eph$IV5 == 0] = 1
eph$techo[is.na(eph$IV4)  & eph$IV5 == 0] = 1
eph$techo[eph$IV4 %in% c(4,5)  & eph$IV5 == 0] = 0
eph$techo[eph$IV4 %in% c(6,7)] = 0
unique(eph$techo)

eph$materiales = NA
eph$materiales[eph$piso == 2 & eph$techo == 2] = 2
eph$materiales[eph$piso == 1 ] = 1 ; eph$materiales[eph$techo == 1 ] = 1
eph$materiales[eph$piso == 0 ] = 0 ; eph$materiales[eph$techo == 0 ] = 0
unique(eph$materiales)

eph= eph %>% select(!c(IV3, piso, IV4, IV5, techo))

# Acceso al agua
unique(eph$IV6); unique(eph$IV7) # no hay 3 de ninguna de las categorías
eph$agua = NA
eph$agua[eph$IV6 %in% c(2, 3)] = 0
eph$agua[eph$IV6 == 1 & eph$IV7 %in% c(3, 4)] = 1
eph$agua[eph$IV6 == 1 & eph$IV7 == 2] = 2
eph$agua[eph$IV6 == 1 & eph$IV7 == 1] = 3
unique(eph$agua)
eph = eph %>% select(!c(IV6, IV7))

# Sewage
unique(eph$IV8); unique(eph$IV9); unique(eph$IV10); unique(eph$IV11)
sum(eph$IV10==2)
eph$saneamiento = 1 # con baño dentro de la vivienda con arrastre de agua a red publica a camara 
eph$saneamiento[eph$IV8== 0] = 0
eph$saneamiento[eph$IV8== 1 & eph$IV9 >1] = 0
eph$saneamiento[eph$IV8== 1 & eph$IV11 >2] = 0
eph$saneamiento[eph$IV8== 1 & eph$IV10 > 2] = 0
unique(eph$saneamiento)
sum(eph$saneamiento==0)
eph = eph %>% select(!c(IV8, IV9, IV10, IV11))

# Location
unique(eph$IV12_1); unique(eph$IV12_2); unique(eph$IV12_3) 
eph$IV12 = eph$IV12_1 + eph$IV12_2 + eph$IV12_3
unique(eph$IV12)
# lo ordeno de menor a mayor bienestar
eph$IV12 = eph$IV12 * (-1) 
eph = eph %>% select(!c(IV12_1, IV12_2, IV12_3))

## ---- Domestic employee ----
eph$W_domestico = eph$W_12 + eph$W_22
unique(eph$W_domestico)
eph = eph %>% select(!c(W_12, W_22))

## Base de Edo et al. (2020), sólo 19 variables y a nivel de hogar ----
eph_edo = eph%>%
  filter(CH03 == 1) %>% # son todas variables nivel de hogar, me quedo con los jefes
  select(CODUSU_NRO_HOGAR, 
         CH09, NIVEL_ED, ESTADO, CALIFICACION_ord,
         V8, V8_M, V9, V9_M, V10, V10_M,
         II7_propietario, V5, V16,
         materiales, agua, saneamiento, IV12,
         W_domestico)

length(unique(eph_edo$CODUSU_NRO_HOGAR)) / nrow(eph_edo)

nrow(eph_edo[complete.cases(eph_edo),]) / nrow(eph_edo) #muy pocos missings, así que los borramos
eph_edo = eph_edo[complete.cases(eph_edo),]
save(eph_edo, file = 'eph_edo.Rdata')

# eph_ordinal ----------

# Vivienda
names(eph)
unique(eph$IV1)
eph$IV1_ord = 0
eph$IV1_ord[eph$IV1 == 1] = 1
eph$IV1_ord[eph$IV1 == 2] = 1

# Hacinamiento
unique(eph$II1)
unique(eph$IX_TOT)
eph$hacinamiento = eph$II1 / eph$IX_TOT


# Hacienamiento vivienda
eph$CODUSU = substr(eph$CODUSU_NRO_HOGAR,1,nchar(eph$CODUSU_NRO_HOGAR[1])-2)

hacinamiento_v = eph %>% 
  group_by(CODUSU) %>%
  summarise(hacinamiento_v = IV2 / sum(unique(IX_TOT)))
eph = merge(eph, hacinamiento_v, by='CODUSU', all.x=T)
sum(eph$hacinamiento != eph$hacinamiento_v, na.rm=T)
rm(hacinamiento_v)

# Usos: cuartos extra ademas de para dormir y trabajar
unique(eph$II2)
unique(eph$II3_1)
eph = eph %>% select(!II3)

eph$usos = eph$II1 / (eph$II2 + eph$II3_1) 
min(eph$usos,na.rm=T)
max(eph$usos,na.rm=T)

# cocina_lavadero_garage
unique(eph$II4_1)
unique(eph$II4_2)
unique(eph$II4_3)
eph$II4_1[eph$II4_1==-9999] = 0
eph$II4_2[eph$II4_2==-9999] = 0
eph$II4_3[eph$II4_3==-9999] = 0
eph$II4= eph$II4_1 + eph$II4_2 + eph$II4_3

# usos_II4
unique(eph$II5)
unique(eph$II6)
unique(eph$II5_1)
unique(eph$II6_1)


eph$usos_II4 =  eph$II4 - (eph$II5_1 + eph$II6_1) 
max(eph$usos_II4,na.rm=T)
min(eph$usos_II4,na.rm=T)

eph = eph %>% select(!c(II1, II2, II3_1,II4_1, II4_2, II4_3,
                        II5, II6, II5_1, II6_1,
                        IV2))


# tenencia
eph$II7[eph$II7<0] = 0


# combustible
eph$II8_ord = NA
unique(eph$II8)
eph$II8_ord[eph$II8 == -9999] = 0
eph$II8_ord[eph$II8 == 3] = 1
eph$II8_ord[eph$II8 == 2] = 2
eph$II8_ord[eph$II8 %in% c(1,4)] = 3
eph = eph %>% select(!c(II8))


# baño
unique(eph$II9)
eph$II9_ord = NA
eph$II9[eph$II9==-9999] = 5
eph$II9_ord = eph$II9 * (-1) + 5
unique(eph$II9_ord)
eph = eph %>% select(!'II9')

# V1 + V2 o V22
unique(eph$V2)
unique(eph$V21)
unique(eph$V22)
eph$V21[eph$V22 == 1] = 1
eph$V2 = eph$V2 + eph$V21
eph = eph %>% select(!c(V21, V22))
max(eph$V2, na.rm=T)

# V3 o V4
sum(eph$V3==1 & eph$V4==1, na.rm=T)
sum(eph$V3==0 & eph$V4==1, na.rm=T)
sum(eph$V3==1 & eph$V4==0, na.rm=T)
eph$V3V4 = eph$V3 + eph$V4
eph = eph %>% select(!c(V3, V4))


# ayuda
unique(eph$V6)
unique(eph$V7)
eph$ayuda = NA
eph$ayuda[eph$V6 == 1] = 0
eph$ayuda[eph$V7 == 1] = 0
eph$ayuda[eph$V6 == 0] = 1
eph$ayuda[eph$V7 == 0] = 1
eph = eph %>% select(!c(V6, V7))

# Sólo nos quedamos con algunas M
eph = eph %>% select(!c(V11, V12, V18))


eph$V17_ord = NA
eph$V17_ord = eph$V17 * (-1)
eph = eph%>%select(!'V17')

eph$V19_ord = NA
unique(eph$V19_A)
unique(eph$V19_B)
eph$V19_ord[eph$V19_A == 0 & eph$V19_B == 0] = 1
eph$V19_ord[eph$V19_A == 1] = 0
eph$V19_ord[eph$V19_B == 1] = 0
unique(eph$V19_ord)
eph = eph %>% select(!c(V19_A, V19_B))

eph$V3V4_M = eph$V3_M + eph$V4_M
eph = eph %>% select(!c(V3_M, V4_M))


# tareas del hogar
colnames(select(eph, contains('w')))

eph$W4 = eph$W4 / eph$IX_TOT
eph$W5 = eph$W5 / eph$IX_TOT

eph$W_externo = 0
eph$W_externo[eph$W_13==1 | eph$W_23==1] = 1 
eph$W_externo[is.na(eph$W_13)| is.na(eph$W_23)] = NA 
eph = eph %>% select(!c(W_13, W_23))


# Características miembros
eph[eph$COMPONENTE == 51, ]
eph[eph$COMPONENTE == 71, ]


# salud
unique(eph$CH08.1)
unique(eph$CH08.2)
unique(eph$CH08.3)
unique(eph$CH08.4)

eph$CH08 = eph$CH08.1 + eph$CH08.2 + eph$CH08.3
max(eph$CH08, na.rm=T)
eph$CH08[eph$CH08 %in% c(1,2,3)] = 1
mean(eph$CH08+eph$CH08.4, na.rm=T)
eph = eph %>% select(!c(CH08.1, CH08.2, CH08.3, CH08.4))

unique(eph$CH15_COD) # demasiados códigos, vuela :D
eph = eph %>% select(!c(CH15_COD))
unique(eph$CH16_COD) # demasiados códigos, vuela :D
eph = eph %>% select(!c(CH16_COD))

# educac
sum(eph$CH06<10) # no hay menores a 10 años, consideramos que todos deben saber leer y escribir
sum(eph$CH06 < 17)

unique(eph$CH10)
eph$CH10_ord = 0
eph$CH10_ord[eph$CH10==1 & eph$CH06<17] = 1
eph$CH10_ord[eph$CH10 %in% c(1,2) & eph$CH06>=17] = 1
eph$CH10_ord[is.na(eph$CH10)] = NA
eph = eph %>% select(!'CH10')

unique(eph$CH11)
eph$CH11[eph$CH11==-9999] = 0

sort(unique(eph$CH06))

eph = eph%>%select(!c(CH12, CH13, CH14))



# categoría
eph$CAT = 0
unique(eph$CAT_OCUP)
unique(eph$CAT_INAC)
eph$CAT[eph$CAT_OCUP %in% c(-9999,-99999)] = 0
eph$CAT[eph$CAT_INAC %in% c(-9999,-99999)] = 0
eph$CAT[eph$CAT_INAC == 1] = 1
eph$CAT[eph$CAT_INAC == 2] = 2
eph$CAT[eph$CAT_INAC == 3] = 3
eph$CAT[eph$CAT_INAC == 4] = 4
eph$CAT[eph$CAT_INAC == 6] = 5
eph$CAT[eph$CAT_INAC == 7] = 6
eph$CAT[eph$CAT_OCUP == 1] = 7
eph$CAT[eph$CAT_OCUP == 2] = 8
eph$CAT[eph$CAT_OCUP == 3] = 9
eph$CAT[eph$CAT_OCUP == 4] = 10
eph$CAT[is.na(eph$CAT_OCUP)] = NA
eph$CAT[is.na(eph$CAT_INAC)] = NA
unique(eph$CAT)
sum(is.na(eph$CAT))
eph = eph %>% select(!c(CAT_OCUP, CAT_INAC))

# la línea entre el activo y el inactivo: cómo busca o por qué no busca
max(rowSums(select(eph,contains('PP02C')), na.rm=T))
unique(eph$PP02C4)
unique(eph$PP02E)
apply(select(eph,contains('PP02C')),2, FUN = function(x) sum(is.na(x)))
eph$PP02 = 0
eph$PP02[eph$PP02C1 == 1] = 1
eph$PP02[eph$PP02C2 == 1] = 1
eph$PP02[eph$PP02C3 == 1] = 1
eph$PP02[eph$PP02C4 == 1] = 1
eph$PP02[eph$PP02C5 == 1] = 1
eph$PP02[eph$PP02C6 == 1] = 1
eph$PP02[eph$PP02C7 == 1] = 1
eph$PP02[eph$PP02C8 == 1] = 1
eph$PP02[eph$PP02C1 == 1] = 1
eph$PP02[eph$PP02E == 3] = 2
eph$PP02[eph$PP02E == 4] = 3
eph$PP02[eph$PP02E == 5] = 4

eph = eph %>% select(!contains('PP02C'))
eph = eph %>% select(!'PP02E')


#subocupados
table(eph$PP03G, eph$PP03I)
table(eph$PP03G, eph$PP03J)

eph$PP03_ord = 0
eph$PP03_ord[eph$PP03H == 3] = 0
eph$PP03_ord[eph$PP03H == 2] = 1
eph$PP03_ord[eph$PP03H == 1] = 2
eph$PP03_ord[eph$PP03G == 0] = 3 #no buscó trabajar más horas en la última semana
eph$PP03_ord[eph$PP03I == 0] = 3 #
eph$PP03_ord[eph$PP03J == 0] = 3 
eph$PP03_ord[is.na(eph$PP03H)] = NA
eph$PP03_ord[is.na(eph$PP03G)] = NA
eph$PP03_ord[is.na(eph$PP03I)] = NA
eph$PP03_ord[is.na(eph$PP03J)] = NA
eph = eph %>% select(!c('PP03G','PP03H', 'PP03I', 'PP03J'))


unique(eph$INTENSI)
sum(eph$INTENSI == 4)
eph$INTENSI_ord = 0
eph$INTENSI_ord[eph$INTENSI %in% c(1,3)] = -1
eph$INTENSI_ord[is.na(eph$INTENSI)] = NA
eph = eph %>% select(!c('INTENSI'))

# unificar pp04a y pp11a
unique(eph$PP11A)
unique(eph$PP04A)
eph$PPA = 0
eph$PPA[eph$PP11A == 1] = 1
eph$PPA[eph$PP11A == 2] = 2
eph$PPA[eph$PP11A == 3] = 3
eph$PPA[is.na(eph$PP11A)] = NA
eph$PPA[eph$PP04A == 1] = 1
eph$PPA[eph$PP04A == 2] = 2
eph$PPA[eph$PP04A == 3] = 3
eph$PPA[is.na(eph$PP04A)] = NA
eph = eph %>% select(!c('PP11A', 'PP04A'))
unique(eph$PPA)

# propio
eph$PP05C = 0
eph$PP05C[eph$PP05C_1 == 1] = 1
eph$PP05C[eph$PP05C_2 == 1] = 1
eph$PP05C[eph$PP05C_3 == 1] = 1
eph$PP05C[is.na(eph$PP05C_3) & is.na(eph$PP05C_2) & is.na(eph$PP05C_1)] = NA
eph = eph %>% select(!c('PP05C_1', 'PP05C_2', 'PP05C_3'))

# algunos encadenamientos de las variables de empleo a cero
eph$caes[eph$caes %in% c("-9999", "-99999")] = 0 
eph$PP04B2[eph$PP04B2<0]= 0
eph$tiempo[eph$tiempo<0] = 0
eph$C99[eph$C99 <0] = 0
eph$PP04G[eph$PP04G<0] = 0
eph$PP05H[eph$PP05H<0] = 0
eph$PP05E = eph$PP05E + 1
eph$PP05E[eph$PP05E < 0] = 0
eph$PP05F = eph$PP05F - 5
eph$PP05F[eph$PP05F < 0] = 0
  
# independientes
eph$independientes = 0
unique(eph$PP06A)
unique(eph$PP06E)
unique(eph$PP06H)
eph$independientes[eph$PP06A == 0] = 1
eph$independientes[eph$PP06E == 1] = 2
eph$independientes[eph$PP06H == 1] = 3
eph$independientes[eph$PP06H == 0] = 4
eph = eph %>% select(!c('PP06A', 'PP06E', 'PP06H'))

# tiempo
unique(eph$PP07A)
eph$PP07A[eph$PP07A<0] = 0

unique(eph$PP07D)
eph$PP07D[eph$PP07D == 0] = 6
eph$PP07D[eph$PP07D<0] = 0

unique(eph$PP07E)
eph$PP07E[eph$PP07E<0] = 0

# especie y beneficios
unique(eph$PP07F1)
unique(eph$PP07F2)
unique(eph$PP07F3)
unique(eph$PP07F4)
eph$PP07F = eph$PP07F1 + eph$PP07F2 + eph$PP07F3 + eph$PP07F4
eph$PP07F[eph$PP07F<0] = 0
unique(eph$PP07F)
eph = eph %>% select(!c('PP07F1','PP07F2', 'PP07F3', 'PP07F4'))

eph$benef = eph$PP07G1 + eph$PP07G2 + eph$PP07G3 + eph$PP07G4
eph$benef[eph$benef<0] = 0
unique(eph$benef)
eph = eph %>% select(!c('PP07G1','PP07G2', 'PP07G3', 'PP07G4'))


unique(eph$PP07H)
unique(eph$PP07I)
eph$jub = 0
eph$jub[eph$PP07I==1] = 1
eph$jub[eph$PP07H==1] = 2
eph$jub[is.na(eph$PP07I)] = NA
eph = eph %>% select(!c('PP07H','PP07I'))

unique(eph$PP07J)
eph$PP07J[eph$PP07J<0]=0

unique(eph$PP07K)
eph$PP07K = - eph$PP07K + 6
eph$PP07K[eph$PP07K>10] = 0

# ingresos
table(eph$PP08D1, eph$PP08D4)
eph$PP08D = eph$PP08D1 + eph$PP08D4
eph$PP08D[eph$PP08D<0] = 0
eph = eph %>% select(!c('PP08D1','PP08D4'))
summary(eph$PP08D)

eph$PP08F = eph$PP08F1 + eph$PP08F2
eph$PP08F[eph$PP08F<0] = 0
eph = eph %>% select(!c('PP08F1','PP08F2'))
summary(eph$PP08F)

eph$PP08J1[eph$PP08J1<0] = 0
eph$PP08J2[eph$PP08J2<0] = 0
eph$PP08J3[eph$PP08J3<0] = 0


unique(eph$PP09A)
eph$PP09A[eph$PP09A<0] = 0

# Desocupados
unique(eph$PP10A)
eph$PP10A[eph$PP10A<0] = 0
eph$PP10A = - eph$PP10A + 5

unique(eph$PP10C)
unique(eph$PP10D)
unique(eph$PP10E)
table(eph$PP10C,eph$PP10D)
eph$PP10 = - eph$PP10E + 7  # hace cuanto hizo algo
eph$PP10[eph$PP10D == 0] = 0 # nunca trabajo
eph$PP10[eph$PP10C == 1] = 7
unique(eph$PP10)
eph$PP10[eph$PP10>1000] = 8
unique(eph$PP10)
eph = eph %>% select(!c('PP10C','PP10D','PP10E'))


unique(eph$PP11B1)
eph$PP11B1[eph$PP11B1<0] = 0

unique(eph$PP11L)
unique(eph$PP11L1)
eph$PP11L[eph$PP11L<0]=0
eph$PP11L1[eph$PP11L1<0]=0

eph$PP07E11M = eph$PP07E
unique(eph$PP07E)
unique(eph$PP11M)
eph$PP07E11M[eph$PP11M == 1] = 1
eph$PP07E11M[eph$PP11M == 2] = 2
eph$PP07E11M[eph$PP11M == 3] = 4
eph = eph%>%select(!c('PP07E','PP11M'))

unique(eph$jub)
eph$jub[eph$PP11N==1] = 2
eph = eph%>%select(!'PP11N')

eph$PP11LO = eph$PP11L
unique(eph$PP11L)
unique(eph$PP11O)
eph$PP11LO[eph$PP11O == 9] = 7 #causas personales 
eph$PP11LO[eph$PP11O == 8] = 5 # otras causas laborales
eph$PP11LO[eph$PP11O == 2] = 6
eph$PP11LO[eph$PP11O == 3] = 6
eph$PP11LO[eph$PP11O == 4] = 3
unique(eph$PP11LO)
eph$PP11LO[eph$PP11O == 1] = 8
eph$PP11LO[eph$PP11O == 5] = 9
eph$PP11LO[eph$PP11O == 6] = 10
eph$PP11LO[eph$PP11O == 7] = 11
eph = eph%>%select(!c('PP11L','PP11O'))

table(eph$PP11P, eph$PP11Q)
eph$PP11PQ = 0 
eph$PP11PQ[eph$PP11Q == 0] = 1 # no fue la única que se quedó sin trabajo
eph$PP11PQ[eph$PP11P == 1] = 2 # cerró la empresa
eph$PP11PQ[is.na(eph$PP11P)] = NA
eph$PP11PQ[is.na(eph$PP11Q)] = NA
eph = eph%>%select(!c('PP11P','PP11Q'))

unique(eph$PP11R)
eph$PP11R[eph$PP11R<0] = 0

eph$PP11ST = 0
eph$PP11ST[eph$PP11S==1] = 1
eph$PP11ST[eph$PP11T==1] = 1
eph$PP11ST[eph$PP11S==1 & eph$PP11T ==1] = 2
eph$PP11ST[is.na(eph$PP11S)]=NA
eph$PP11ST[is.na(eph$PP11T)]=NA
unique(eph$PP11ST)
sum(is.na(eph$PP11ST))
eph = eph%>%select(!c('PP11S','PP11T'))



# Ingresos
summary(eph$P21)
summary(eph$DECOCUR)
summary(eph$IDECOCUR)
summary(eph$RDECOCUR)
summary(eph$GDECOCUR)
summary(eph$ADECOCUR)
summary(eph$TOT_P12)

summary(eph$DECINDR)
summary(eph$RDECINDR)
summary(eph$GDECINDR)
summary(eph$ADECINDR)

summary(eph$ITF)
summary(eph$DECIFR)
summary(eph$RDECIFR)
summary(eph$GDECIFR)
summary(eph$ADECIFR)

summary(eph$IPCF)
summary(eph$DECCFR)
summary(eph$RDECCFR)
summary(eph$GDECCFR)
summary(eph$ADECCFR)

save(eph, file = 'eph_ord.RData')



# Leo las variables que ordené ----------
variables_usadas <- read_excel("G:/Mi unidad/TESIS ECON/Sobre la EPH/variables usadas.xlsx", 
                               col_types = c("skip", "text", "text", 
                                             "text", "text", "text", "skip", "skip"))

names(variables_usadas)
names(eph)[!names(eph) %in% c(pond, identificadores, variables_usadas$Nombre)]
variables_usadas$Nombre[!variables_usadas$Nombre %in% names(eph)]

eph = eph %>% select(!names(eph)[!names(eph) %in% c(pond, identificadores, variables_usadas$Nombre)])

save(eph, file = 'eph_ord.RData')
