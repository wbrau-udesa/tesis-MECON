setwd('G:/Mi unidad/TESIS ECON/R')
library(eph)
library(tidyverse)
library(readxl)
library(aspect)

##########################  Lista de bases a considerar
lista_bases <- list(get_microdata(year = 2019, trimester = 4,type='individual', vars = 'all'),
                    get_microdata(year = 2019, trimester = 3,type='individual', vars = 'all'),
                    get_microdata(year = 2019, trimester = 2,type='individual', vars = 'all'),
                    get_microdata(year = 2019, trimester = 1,type='individual', vars = 'all'),
                    get_microdata(year = 2018, trimester = 4,type='individual', vars = 'all'),
                    get_microdata(year = 2018, trimester = 3,type='individual', vars = 'all'),
                    get_microdata(year = 2018, trimester = 2,type='individual', vars = 'all'),
                    get_microdata(year = 2018, trimester = 1,type='individual', vars = 'all'))
lista_bases_h <- list(get_microdata(year = 2019, trimester = 4,type='hogar', vars = 'all'),
                      get_microdata(year = 2019, trimester = 3,type='hogar', vars = 'all'),
                      get_microdata(year = 2019, trimester = 2,type='hogar', vars = 'all'),
                      get_microdata(year = 2019, trimester = 1,type='hogar', vars = 'all'),
                      get_microdata(year = 2018, trimester = 4,type='hogar', vars = 'all'),
                      get_microdata(year = 2018, trimester = 3,type='hogar', vars = 'all'),
                      get_microdata(year = 2018, trimester = 2,type='hogar', vars = 'all'),
                      get_microdata(year = 2018, trimester = 1,type='hogar', vars = 'all'))


##########################
########################## 1 LIMPIEZA ##########################
########################## 

##########################  Función de limpieza ########################## 
# El argumento base_t es el número de base en la lista de bases
limpieza = function(base_t){
## UNA BASE EN PARTICULAR  ---------------------------------------


ephi <- lista_bases[[base_t]]
unique(ephi$ANO4); unique(ephi$TRIMESTRE)

# caes
ephi <- organize_caes(ephi)
ephi = ephi %>% # sólo conservamos caes_eph_label, el resto las eliminamos
  select(!c(colnames(ephi)[(ncol(ephi)-8+1):ncol(ephi)][colnames(ephi)[(ncol(ephi)-8+1):ncol(ephi)]!='caes_eph_label'],"PP04B_COD"))

# cno
ephi <- organize_cno(ephi)
ephi = ephi %>%  # solo conservamos CALIFICACION
  select(!c('CATEGORIA','JERARQUIA','TECNOLOGIA','PP04D_COD'))

ephh <- lista_bases_h[[base_t]]

## GBA ----------------------------------------------
ephi_gba = ephi %>%
  filter(REGION == 1)
ephh_gba = ephh %>%
  filter(REGION == 1)
rm(ephh, ephi)
ifelse( unique(ephi_gba$ANO4) == unique(ephh_gba$ANO4) & unique(ephi_gba$TRIMESTRE) == unique(ephh_gba$TRIMESTRE),
        print('COINCIDE PERIODO'), print('NO COINCIDE PERIODO'))



## Merge individual + hogar --------------------------
# Creamos identificadores de cada encuesta (vivienda-hogar y vivienda-hogar-componente)
ephh_gba$CODUSU_NRO_HOGAR = paste(ephh_gba$CODUSU,ephh_gba$NRO_HOGAR,sep="_")
ephi_gba$CODUSU_NRO_HOGAR = paste(ephi_gba$CODUSU,ephi_gba$NRO_HOGAR,sep="_")
ephi_gba$id = paste(ephi_gba$CODUSU_NRO_HOGAR, ephi_gba$COMPONENTE, sep="_")
identificadores = c('ANO4','TRIMESTRE','REGION', 
                    'CODUSU','NRO_HOGAR','CODUSU_NRO_HOGAR','id')

# Quitamos variables repetidas
mask = !colnames(ephi_gba) %in% colnames(ephh_gba) # eliminaremos todas las de i presentes en h
mask[length(mask)-1] = TRUE # excepto CODUSU_NRO_HOGAR, la necesitamos para el merge
ephi_gba = ephi_gba[mask]

# Merge por CODUSU_NRO_HOGAR
eph = merge(ephi_gba, ephh_gba, by = 'CODUSU_NRO_HOGAR', all.x=T, all.y = F)
rm(ephh_gba, ephi_gba, mask)
eph = eph %>% relocate(id, CODUSU_NRO_HOGAR)

# Hay hogares sin encuesta realizada?
eph = eph[which(eph$REALIZADA==1),]
eph = eph %>% select(!REALIZADA)

# Hay individuos sin encuesta realizada?
eph = eph[which(eph$H15==1),]
eph = eph %>% select(!H15)

## Limpiezas varias de variables --------------------------

# Tipos de variables
#categ = c()
#num = c()
#dummies = c()
#nd = c()

# Fechas
eph = eph%>%select(!CH05)


### Características de la vivienda ####
#categ = c(categ,'IV1')

eph$IV1_ESP = as.character(eph$IV1_ESP)
unique(eph$IV1_ESP) # sólo indica si es "casilla"
eph$IV1_ESP[eph$IV1_ESP=='CASILLA'] = 'casilla'
eph$IV1_ESP[eph$IV1_ESP=='casilla de chapa'] = 'casilla'
eph$IV1[eph$IV1_ESP == 'casilla'] = 6 # El 6 de IV1 puede referirse a casilla
eph$IV1[eph$IV1_ESP == 'duplex'] = 2 # duplex a dos 
eph = eph %>% select(!IV1_ESP)

eph$IV2[eph$IV2 == 99] = NA ## 99 son no sabe / no responde
#num = c(num,'IV2')

#categ = c(categ,'IV3')

eph$IV3_ESP = as.character(eph$IV3_ESP)
unique(eph$IV3_ESP) # ok mandarlo a suficiente
eph = eph %>% select(!IV3_ESP)

eph$IV4[eph$IV4 == 9] = NA
#categ = c(categ,'IV4')

eph$IV5[eph$IV5 == 2] = 0 # ahora es dummy 0 = No, 1 = Sí
#dummies = c(dummies,'IV5')

#categ = c(categ,'IV6')
#categ = c(categ,'IV7')

eph$IV7_ESP = as.character(eph$IV7_ESP)
eph$IV7[eph$IV7_ESP == 'no tiene'] = 5
eph = eph%>% select(!IV7_ESP)

eph$IV8[eph$IV8 == 2] = 0 # ahora es dummy 0 = No, 1 = Sí
#dummies = c(dummies,'IV8')

eph$IV9[eph$IV9 == 0] = -9999 # encadenada con IV8
#categ = c(categ,'IV9')

eph$IV10[eph$IV10 == 0] = -9999 # encadenada con IV8
#categ = c(categ,'IV10')

eph$IV11[eph$IV11 == 0] = -9999 # encadenada con IV8
#categ = c(categ,'IV11')

#encadenadas_vivienda = c('IV8', 'IV9', 'IV10', 'IV11')

eph$IV12_1[eph$IV12_1 == 2] = 0
eph$IV12_2[eph$IV12_2 == 2] = 0
eph$IV12_3[eph$IV12_3 == 2] = 0
#dummies = c(dummies, 'IV12_1', 'IV12_2', 'IV12_3')

### Características habitacionales del hogar ####

eph$II1[eph$II1 == 99] = NA
#num = c(num,'II1')
#num = c(num,'II2')

eph$II3[eph$II3 == 0] = -9999 # encadenada con II1
eph$II3[eph$II3 == 2] = 0 # y luego transformo a dummy
#dummies = c(dummies, 'II3')

eph$II3_1[eph$II3_1 == 0] = 0 # encadenada con II3, como es numérica la dejamos en 0
#num = c(num,'II3_1')
#encadenadas_vivienda =c(encadenadas_vivienda, 'II1', 'II3', 'II3_1')

eph$II4_1[eph$II4_1 == 0] = -9999 # encadenada con II1
eph$II4_2[eph$II4_2 == 0] = -9999 # encadenada con II1
eph$II4_3[eph$II4_3 == 0] = -9999 # encadenada con II1
eph$II4_1[eph$II4_1 == 2] = 0
eph$II4_2[eph$II4_2 == 2] = 0
eph$II4_3[eph$II4_3 == 2] = 0
#dummies = c(dummies, 'II4_1', 'II4_2', 'II4_3')
#encadenadas_vivienda =c(encadenadas_vivienda, 'II4_1', 'II4_2', 'II4_3')

eph$II5[eph$II5 == 0] = - 9999 # encaddenada con II4
eph$II5[eph$II5 == 2] = 0
#dummies = c(dummies, 'II5')
#encadenadas_vivienda =c(encadenadas_vivienda, 'II5')


#num = c(num, 'II5_1') # encadenada pero es numérica así que la dejamos en 0

eph$II6[eph$II6 == 0] = -9999 # encadenada con I4
eph$II6[eph$II6 == 2] = 0
#dummies = c(dummies, 'II6')
#encadenadas_vivienda =c(encadenadas_vivienda, 'II6')

#num = c(num, 'II6_1') # encadenada con I6 pero como es numérica la dejamos en 0

eph$II7[eph$II7 == 0] = -9999 # encadenada con II1
# categ = c(categ, 'II7')
# encadenadas_vivienda =c(encadenadas_vivienda, 'II7')

eph = eph %>% select(!II7_ESP) # queda como "otros"

eph$II8[eph$II8 == 0] = -9999 # encadenada con II1
#categ = c(categ, 'II8')
#encadenadas_vivienda =c(encadenadas_vivienda, 'II8')

eph$II8_ESP = as.character(eph$II8_ESP)
unique(eph$II8_ESP) # todos son electricidad, excepto un par que reemplazamos por NA:
eph$II8[eph$II8_ESP=="No dice"] = NA # 
eph$II8[eph$II8_ESP=="cocina en otro lugar"] = NA 
eph = eph %>% select(!II8_ESP) # y ahora todas la categorias 4 de II8 son electricidad

eph$II9[eph$II9 == 0] = -9999 # encadenada con II1
#categ = c(categ, 'II9')
#encadenadas_vivienda =c(encadenadas_vivienda, 'II9')

### Estrategias del hogar ####

estrat = sapply(1:18, FUN=function(x) paste('V',x, sep=''))
estrat = c(estrat, 'V21', 'V22', 'V19_A', 'V19_B')
for (var in estrat){
  eph[var][eph[var]==9] = NA
  eph[var][eph[var]==2] = 0
}

#dummies = c(dummies, estrat)
rm(estrat)


### Resumen del hogar ####
eph = eph %>% select(!IX_MAYEQ10)

#r = c('IX_TOT', 'IX_MEN10')
#num = c(num,r)
#rm(r, var)

### Organización del hogar ####

### Indica quién realiza las tareas del hogar
### Como ahora la base la tenemos por componente, armamos dummies a partir de ellas:
# W_11 Realiza tareas de la casa 
# W_12 Servicio doméstico realiza las tareas del hogar
# W_13 Otra persona que no vive en la casa realiza las tareas del hogar
eph$W_11 = 0
eph$W_11[eph$VII1_1==99] = NA
eph$W_11[eph$COMPONENTE == eph$VII1_1] = 1
eph$W_11[eph$COMPONENTE == eph$VII1_2] = 1

eph$W_12 = 0
eph$W_12[eph$VII1_1==99] = NA
eph$W_12[eph$VII1_1==96] = 1
eph$W_12[eph$VII1_2==96] = 1

eph$W_13 = 0
eph$W_13[eph$VII1_1==99] = NA
eph$W_13[eph$VII1_2==97] = 1
eph$W_13[eph$VII1_2==97] = 1

# W_21 Ayuda en tareas de la casa 
# W_22 Servicio doméstico realiza las tareas del hogar
# W_23 Otra persona que no vive en la casa realiza las tareas del hogar

eph$W_21 = 0
eph$W_21[eph$VII2_1==99] = NA
eph$W_21[eph$COMPONENTE == eph$VII2_1] = 1
eph$W_21[eph$COMPONENTE == eph$VII2_2] = 1
eph$W_21[eph$COMPONENTE == eph$VII2_3] = 1
eph$W_21[eph$COMPONENTE == eph$VII2_4] = 1

eph$W_22 = 0
eph$W_22[eph$VII2_1==99] = NA
eph$W_22[eph$VII2_1==96] = 1
eph$W_22[eph$VII2_2==96] = 1
eph$W_22[eph$VII2_3==96] = 1
eph$W_22[eph$VII2_4==96] = 1

eph$W_23 = 0
eph$W_23[eph$VII2_1==99] = NA
eph$W_23[eph$VII2_1==97] = 1
eph$W_23[eph$VII2_2==97] = 1
eph$W_23[eph$VII2_3==97] = 1
eph$W_23[eph$VII2_4==97] = 1

# dummies = c(dummies, 'W_11', 'W_12', 'W_13', 'W_21', 'W_22', 'W_23')

# eliminar las anteriores
eph = eph %>% select(!c(VII1_1,VII1_2,VII2_1,VII2_2,VII2_3,VII2_4))

# Variables por hogar
W_HOGAR = eph %>% 
  group_by(CODUSU_NRO_HOGAR) %>%
  summarise(W4 = sum(W_11, W_12, W_13), # total de personas que realizan tareas en el hogar
            W5 = sum(W_21, W_22, W_23)) # total de personas que ayudan en las tareas del hogar
eph = merge(eph, W_HOGAR, by='CODUSU_NRO_HOGAR', all.x = T, all.y = F)
#num = c(num, c('W4', 'W5'))
rm(W_HOGAR)

### Identificación ####
eph = eph %>% select(!MAS_500) # en este caso eliminamos MAS_500 pes no tiene variabilidad
identificadores = c(identificadores, 'PONDERA','COMPONENTE')

# Características de los miembros del hogar

#categ = c(categ, 'ESTADO')
#categ = c(categ, 'CH03')

eph$CH04[eph$CH04==2] = 0 # mujer es base
#dummies = c(dummies, 'CH04')

# num = c(num, 'CH06')

eph$CH07[eph$CH07 == 9] = NA
# categ = c(categ, 'CH07')

# transformo manualmente a dummy la ch08
eph$CH08.1 = 0
eph$CH08.2 = 0
eph$CH08.3 = 0
eph$CH08.4 = 0
eph$CH08.1[eph$CH08 == 1] = 1
eph$CH08.1[eph$CH08 == 12] = 1
eph$CH08.1[eph$CH08 == 13] = 1
eph$CH08.1[eph$CH08 == 123] = 1
eph$CH08.2[eph$CH08 == 2] = 1
eph$CH08.2[eph$CH08 == 12] = 1
eph$CH08.2[eph$CH08 == 23] = 1
eph$CH08.2[eph$CH08 == 123] = 1
eph$CH08.3[eph$CH08 == 3] = 1
eph$CH08.3[eph$CH08 == 13] = 1
eph$CH08.3[eph$CH08 == 23] = 1
eph$CH08.3[eph$CH08 == 123] = 1
eph$CH08.4[eph$CH08 == 4] = 1
eph$CH08.1[eph$CH08 == 9] = NA #ns/nr
eph$CH08.2[eph$CH08 == 9] = NA #ns/nr
eph$CH08.3[eph$CH08 == 9] = NA #ns/nr
eph$CH08.4[eph$CH08 == 9] = NA #ns/nr
#dummies = c(dummies,colnames(select(eph,contains('CH08.'))))
eph = eph%>%select(!CH08)

eph$CH09[eph$CH09==9] = NA
eph$CH09[eph$CH09==2] = 0 #no sabe leer y escribir
#dummies = c(dummies, 'CH09')

eph$NIVEL_ED[eph$NIVEL_ED==7] = 0
#categ = c(categ, 'NIVEL_ED')


#### Encadenadas educ ----
eph$CH10[eph$CH10==9] = NA
#categ = c(categ, 'CH10')

eph$CH11[eph$CH11 == 9] = NA
eph$CH11[eph$CH10 == 3] = -9999 # responden sólo si asiste
eph$CH11[eph$CH10 == 2] = -9999 # responden sólo si asiste
#categ = c(categ, 'CH11')

eph$CH12[eph$CH12 == 99] = NA
eph$CH12[eph$CH10 == 3] = -9999 # responden sólo si asiste o asistió
#categ = c(categ, 'CH12')

eph$CH13[eph$CH13 == 9] = NA
eph$CH13[eph$CH10 == 3] = -9999 # responden sólo si asiste o asistió
eph$CH13[eph$CH13 == 2] = 0
#dummies = c(dummies, 'CH13')

eph$CH14[which(eph$CH10 == 3)] = "-9999" # responden sólo si asiste o asistió
eph$CH14[which(eph$CH13 == 1)] = "-99999" # responden sólo si NO finalizó el nivel
eph$CH14[eph$CH14 == 99] = NA
eph$CH14 = as.numeric(eph$CH14)
#categ = c(categ, 'CH14')

#encadenadas_educ = c('CH10', 'CH11', 'CH12', 'CH13', 'CH14')

#### Encadenadas migra -----

eph$CH15[eph$CH15==9] = NA
# categ = c(categ, 'CH15')

eph$CH15_COD[which(eph$CH15==1)] = -9999 # sólo responde si en la otra respondio 3, 4 o 5
eph$CH15_COD[which(eph$CH15==2)] = -9999
table(eph$CH15, eph$CH15_COD)
#categ = c(categ,'CH15_COD')
# encadenadas_migra = c('CH15', 'CH15_cod')

eph$CH16[eph$CH16==9] = NA
#categ = c(categ, 'CH16')

eph$CH16_COD[which(eph$CH16==1)] = -9999 # sólo responde si en la otra respondio 3, 4 o 5
eph$CH16_COD[which(eph$CH16==2)] = -9999
categ = c(categ, 'CH16_COD')
  
#encadenadas_migra = c(encadenadas_migra, 'CH16', 'CH16_Cod')


### Actividad ####

# Agregamos NA luego de chequear consistencia con inactivos 
eph$ESTADO[eph$PP02E!=0 & eph$ESTADO==2] = NA # si NO buscaron trabajo son inactivos
sum(is.na(eph$ESTADO)) # SON 5 OBSERVACIONES, LAS ELIMINAMOS POR INCONSISTENTES!!!
eph = eph[!is.na(eph$ESTADO),] 

eph$CAT_OCUP[eph$CAT_OCUP == 9] = NA
eph$CAT_OCUP[eph$ESTADO == 3] = -9999 # no aplica a inactivos
eph$CAT_OCUP[eph$CAT_OCUP==0] = -99999 # no aplica a desocupados sin empleo anterior o con empleo hace más de 3 años
#categ = c(categ, 'CAT_OCUP')

eph$CAT_INAC[eph$ESTADO == 1] = -9999 
eph$CAT_INAC[eph$ESTADO == 2] = -9999 
#categ = c(categ, 'CAT_INAC')

#encadenadas_activ = c('ESTADO', 'CAT_OCUP', 'CAT_INAC')

identificadores = c(identificadores, 'IMPUTA')

### Buscar trabajo

# Desocupados (cómo buscó trabajo)
vars = colnames(select(eph, contains('PP02C')))
for (var in vars){
  eph[var][eph['ESTADO']==1] = -9999 #  si NR por ocupado, ponemos -9999
  eph[var][eph['ESTADO']==3] = -9999 # si NR por inactivo, ponemos -9999
}
for (var in vars){
  eph[var][eph[var]==0] = -999 #  no entendemos por qué va 0, le ponemos -999
  eph[var][eph[var]==2] = 0 #  la pasamos a dummy
}
#dummies = c(dummies, vars)
#encadenadas_activ = c(encadenadas_activ, vars)
rm(vars, var)


# Inactivos
eph$PP02E[eph$ESTADO==1] = -9999
eph$PP02E[eph$ESTADO==2] = -9999
# categ = c(categ, 'PP02E')

eph$PP02H[eph$ESTADO==1] = -9999
eph$PP02H[eph$ESTADO==2] = -9999
eph = eph[eph$PP02H!=0,] # eliminamos 7 inactivos que siguen teniendo 0 y no entendemos por qué no les corresponde esa parte del cuestionario
eph$PP02H[eph$PP02H==2] = 0
#dummies = c(dummies, 'PP02H')

eph$PP02I[eph$ESTADO==1] = -9999
eph$PP02I[eph$ESTADO==2] = -9999
eph$PP02I[eph$PP02I==2] = 0
#dummies = c(dummies, 'PP02I')

#encadenadas_activ = c(encadenadas_activ, 'PP02E', 'PP02H', 'PP02I')

### Ocupados ####

# En la semana de referencia
eph$PP03C[eph$ESTADO == 2] = -9999
eph$PP03C[eph$ESTADO == 3] = -9999
#sum(is.na(eph$PP03C)) # todos los NA correspondían a variable encadenada
eph$PP03D[eph$ESTADO == 2] = -9999
eph$PP03D[eph$ESTADO == 3] = -9999
#unique(eph$PP03D) # todos los NA correspondían a variable encadenada
eph$PP03D[eph$PP03C == 1] = 1 # lo mas probable es que estos tengan una ocupacion!! pues no hay 1 sino. Los que respondían que tenían 1 en la preg anterior
eph$PP03D[eph$PP03C == 0] = 0 # ocupados que NO trabajaron en la semana de referencia (ej vacaciones)
eph = eph %>% select(!PP03C)# podemos eliminar la 3C pues queda completamente explicada por la 3D
#num = c(num, 'PP03D')

eph$PP3E_TOT[eph$ESTADO == 2] = -9999
eph$PP3E_TOT[eph$ESTADO == 3] = -9999
#sum(is.na(eph$PP3E_TOT)) # todos los NA correspondían a variable encadenada
eph$PP3E_TOT[which(eph$PP3E_TOT == 999)] = NA
#num = c(num, 'PP3E_TOT')

eph$PP3F_TOT[eph$ESTADO == 2] = -9999
eph$PP3F_TOT[eph$ESTADO == 3] = -9999
#sum(is.na(eph$PP3F_TOT)) # todos los NA correspondían a variable encadenada
eph$PP3F_TOT[which(eph$PP3F_TOT == 999)] = NA
#num = c(num, 'PP3F_TOT')

eph$PP03G[eph$ESTADO == 2] = -9999
eph$PP03G[eph$ESTADO == 3] = -9999
#unique(eph$PP03G) # todos los NA correspondían a variable encadenada
eph$PP03G[which(eph$PP03G == 9)] = NA
eph$PP03G[which(eph$PP03G == 2)] = 0
#dummies = c(dummies, 'PP03G')


eph$PP03H[eph$ESTADO == 2] = -9999
eph$PP03H[eph$ESTADO == 3] = -9999
#unique(eph$PP03H) # todos los NA correspondían a variable encadenada
eph$PP03H[which(eph$PP03H == 0)] = -99999 # los ceros que quedan son x encadenada
eph$PP03H[which(eph$PP03H == 9)] = NA
#categ = c(categ, 'PP03H')

#encadenadas_activ = c(encadenadas_activ, 'PP03C', 'PP03D', 'PP3E_TOT', 'PP3F_TOT', 'PP03G', 'PP03H')

# Todos
eph$PP03I[eph$ESTADO == 2] = -9999
eph$PP03I[eph$ESTADO == 3] = -9999
#unique(eph$PP03I) # todos los NA correspondían a variable encadenada
eph$PP03I[which(eph$PP03I == 9)] = NA
eph$PP03I[which(eph$PP03I == 2)] = 0
#dummies = c(dummies,'PP03I')

eph$PP03J[eph$ESTADO == 2] = -9999
eph$PP03J[eph$ESTADO == 3] = -9999
#unique(eph$PP03J) # todos los NA correspondían a variable encadenada
eph$PP03J[which(eph$PP03J == 9)] = NA
eph$PP03J[which(eph$PP03J == 2)] = 0
#dummies = c(dummies,'PP03J')

eph$INTENSI[eph$ESTADO == 2] = -9999
eph$INTENSI[eph$ESTADO == 3] = -9999
#unique(eph$INTENSI) # todos los NA correspondían a variable encadenada
#categ = c(categ, 'INTENSI')
#encadenadas_activ = c(encadenadas_activ, 'INTENSI')


#### Ocup principal ####
vars = colnames(select(eph, contains('PP04')))

for (var in vars){
  eph[var][,1][eph$ESTADO == 2] = -9999
  eph[var][,1][eph$ESTADO == 3] = -9999
}

eph$PP04A[eph$PP04A==9] = NA
#categ = c(categ, 'PP04A')

eph$caes_eph_label = as.character(eph$caes_eph_label)
eph$caes_eph_label[eph$ESTADO == 2] = -9999
eph$caes_eph_label[eph$ESTADO == 3] = -9999
#unique(eph$caes_eph_label) # los NA correspondían a variable encadenada
#categ = c(categ, 'caes_eph_label')
#encadenadas_activ = c(encadenadas_activ, 'caes_eph_label')

eph$PP04B1[eph$caes_eph_label!='Servicio domestico' & eph$ESTADO==1] = -99999
eph$PP04B1[eph$PP04B1==2] = 0
eph$PP04B2[eph$caes_eph_label!='Servicio domestico' & eph$ESTADO==1] = -99999
eph = eph%>%select(!PP04B1) # directamente eliminamos B1 y si NO trabaja en casa de flia, en cuántas trabaja será 0
#num = c(num, 'PP04B2')

# juntamos mes año y día en un proxi de días
eph$PP04B3_ANO[eph$caes_eph_label!='Servicio domestico' & eph$ESTADO==1] = -99999
eph$PP04B3_MES[eph$caes_eph_label!='Servicio domestico' & eph$ESTADO==1] = -99999
eph$PP04B3_DIA[eph$caes_eph_label!='Servicio domestico' & eph$ESTADO==1] = -99999
eph$PP04B3_ANO[eph$PP04B3_ANO==99] = NA
eph$PP04B3_MES[eph$PP04B3_MES==99] = NA
eph$PP04B3_DIA[eph$PP04B3_DIA==99] = NA
eph$PP04B3 = eph$PP04B3_DIA + eph$PP04B3_MES * 30 + eph$PP04B3_ANO * 365
eph$PP04B3[eph$ESTADO == 2] = -9999
eph$PP04B3[eph$ESTADO == 3] = -9999
eph$PP04B3[eph$caes_eph_label!='Servicio domestico' & eph$ESTADO==1] = -99999
unique(eph$PP04B3)
#num = c(num, 'PP04B3')
eph = eph %>% select(!contains('PP04B3_'))

eph$PP04C[eph$PP04B2>0] = -99999
# Junto la pp04c con la pp04c99 total ninguna es realmente continua
eph$PP04C99[eph$PP04B2>0] = -99999
eph$PP04C99[eph$PP04C>0 & eph$PP04C<6] = 1
eph$PP04C99[eph$PP04C>5 & eph$PP04C<9] = 2
eph$PP04C99[eph$PP04C>8 & eph$PP04C<98] = 3
table(eph$PP04C, eph$PP04C99)
eph$PP04C99[eph$PP04C99==9] = NA
unique(eph$PP04C99)
eph = eph%>%select(!PP04C)
#categ = c(categ, 'PP04C99')

eph$CALIFICACION[eph$ESTADO == 2] = -9999
eph$CALIFICACION[eph$ESTADO == 3] = -9999
eph$CALIFICACION[eph$CALIFICACION=="Ns.Nc"] = NA
eph$CALIFICACION[eph$CALIFICACION=="falta informacion"] = NA
eph$CALIFICACION[eph$CALIFICACION=="otro"] = NA
#categ = c(categ, 'CALIFICACION')
#encadenadas_activ = c(encadenadas_activ,'CALIIFICACION')

eph$PP04G[eph$PP04B2>0] = -99999
eph$PP04G[eph$PP04G==99] = NA
#categ=c(categ, 'PP04G')

#vars = colnames(select(eph, contains('PP04')))
#encadenadas_activ = c(encadenadas_activ, vars)
rm(vars)

#### Ocup Independientes ####
# recordemos que CAT_OCUP es la variable que identifica al tipo de trabajador
vars = colnames(select(eph, contains('PP05')))
for (var in vars){ ######### SON SOLO OCUPADOS
  eph[var][,1][eph$ESTADO == 2] = -9999
  eph[var][,1][eph$ESTADO == 3] = -9999
}

# juntamos mes año y día en un proxi de días, y solo para aquellos con CAT_OCUP == 4 (trabajador fliar sin remuneracion)
eph$PP05B2_ANO[eph$CAT_OCUP != 4 & eph$ESTADO==1] = -99999
eph$PP05B2_MES[eph$CAT_OCUP != 4 & eph$ESTADO==1] = -99999
eph$PP05B2_DIA[eph$CAT_OCUP != 4 & eph$ESTADO==1] = -99999
eph$PP05B2 = eph$PP05B2_DIA + eph$PP05B2_MES * 30 + eph$PP05B2_ANO * 365
eph$PP05B2[eph$ESTADO == 2] = -9999
eph$PP05B2[eph$ESTADO == 3] = -9999
eph$PP05B2[eph$CAT_OCUP != 4 & eph$ESTADO==1] = -99999
#num = c(num, 'PP05B2')
eph = eph %>% select(!contains('PP05B2_'))
#encadenadas_activ = c(encadenadas_activ, 'PP05B2')


# Problemas de consistencia entre CAT_OCUP y PP05, PP06: asalariados responden el cuestionario
eph = eph[!(eph$CAT_OCUP==3 & eph$PP05C_1 > 0),] # Las eliminamos

vars = colnames(select(eph, contains('PP05C')))
for (var in vars){ ## sólo independientes (CAT OCUP == 1 o 2)
  eph[var][,1][(!eph$CAT_OCUP %in% c(1,2)) & eph$ESTADO==1] = -99999
}
#categ = c(categ, vars)
#encadenadas_activ = c(encadenadas_activ, vars)
rm(var, vars)

eph$PP05E[(!eph$CAT_OCUP %in% c(1,2)) & eph$ESTADO==1] = -99999
eph$PP05F[(!eph$CAT_OCUP %in% c(1,2)) & eph$ESTADO==1] = -99999
eph$PP05H[(!eph$CAT_OCUP %in% c(1,2)) & eph$ESTADO==1] = -99999

eph$PP05E[eph$CAT_OCUP == 1 & eph$ESTADO==1] = -99999
eph$PP05E[eph$PP05E == 2] = 0
#dummies = c(dummies, 'PP05E')
#encadenadas_activ = c(encadenadas_activ, 'PP05E')

eph$PP05H[eph$PP05H==9] = NA      
#categ = c(categ, 'PP05F', 'PP05H')
#encadenadas_activ = c(encadenadas_activ, 'PP05F', 'PP05H')

# Ingresos de la ocup princ de trabajadores independientes
vars = colnames(select(eph, contains('PP06')))
for (var in vars){ ## sólo independientes
  eph[var][,1][eph$ESTADO==2] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][(!eph$CAT_OCUP %in% c(1,2)) & eph$ESTADO==1] = -99999
}
rm(var,vars)

eph$PP06A[eph$PP06A == 9] = NA
eph$PP06A[eph$PP06A == 2] = 0
#dummies = c(dummies, 'PP06A')
#encadenadas_activ = c(encadenadas_activ, 'PP06A')

# sumamos PP06C y PP06D, total ya tenemos la dummy de si tiene o no socios
# Es -7 si no tenía esa ocupación en el mes de referencia
# Es -8 si no tuvo ingresos en el mes de referencia
# El -9 es el NA
eph$PP06C[eph$PP06C== -7] = -999999
eph$PP06D[eph$PP06D== -7] = -999999
eph$PP06C[eph$PP06C== -8] = -999999
eph$PP06D[eph$PP06D== -8] = -999999
eph$PP06B = eph$PP06C + eph$PP06D
eph$PP06B[eph$PP06C < -9999] = -99999
eph$PP06B[eph$PP06D < -9999] = -99999
eph$PP06B[eph$ESTADO==2] = -9999
eph$PP06B[eph$ESTADO==3] = -9999

eph$PP06B[eph$PP06B == -9] = NA
eph = eph%>%select(!c(PP06C, PP06D))


eph$PP06E[eph$PP06A == 0] = -999999
eph$PP06E[eph$PP06B < 0] = -999999
eph$PP06E[eph$ESTADO==2] = -9999
eph$PP06E[eph$ESTADO==3] = -9999
eph$PP06E[eph$PP06E==9] = NA
#categ = c(categ, 'PP06E')

eph$PP06H[eph$PP06E == 1] = -999999 #encadenada de pp06e
eph$PP06H[eph$PP06E ==  -999999 ] = -999999 #encadenada de pp06e
eph$PP06H[eph$PP06H == 9] = NA
eph$PP06H[eph$PP06H == 2] = 0
#dummies = c(dummies, 'PP06H')
#encadenadas_activ = c(encadenadas_activ, 'PP06E', 'PP06H')

#### Ocup Asalariados ####
vars = colnames(select(eph, contains('PP07')))
for (var in vars){ 
  eph[var][,1][eph$ESTADO==2] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$CAT_OCUP != 3 & eph$ESTADO==1] = -99999
}


# A C D E: Excluyendo servicio doméstico que trabaja en casas particulares
eph$PP07A[eph$ESTADO==1 &
            eph$CAT_OCUP==3 & 
            eph$caes_eph_label == "Servicio domestico" &
            eph$PP04B2>0] = -999999
eph$PP07A[eph$PP07A==9] = NA
unique(eph$PP07A)
#categ = c(categ, 'PP07A')
#encadenadas_activ = c(encadenadas_activ, 'PP07A')

eph$PP07C[eph$ESTADO==1 &
            eph$CAT_OCUP==3 & 
            eph$caes_eph_label == "Servicio domestico" &
            eph$PP04B2>0] = -999999
eph$PP07C[eph$PP07C==9] = NA
eph$PP07C[eph$PP07C==2] = 0

eph$PP07D[eph$ESTADO==1 &
            eph$CAT_OCUP==3 & 
            eph$caes_eph_label == "Servicio domestico" &
            eph$PP04B2>0] = -999999
eph = eph %>% select(!PP07C)
eph$PP07D[eph$PP07D==9] = NA
#categ = c(categ, 'PP07D')
#encadenadas_activ = c(encadenadas_activ, 'PP07D')

eph$PP07E[eph$ESTADO==1 &
            eph$CAT_OCUP==3 & 
            eph$caes_eph_label == "Servicio domestico" &
            eph$PP04B2>0] = -999999
eph$PP07E[eph$PP07D==0] = -999999
eph$PP07E[eph$PP07E==9] = NA
#categ = c(categ, 'PP07E')
#encadenadas_activ = c(encadenadas_activ, 'PP07E')

# Incluyendo servicio doméstico

rm(var,vars)
eph = eph %>% select(!c(PP07F5, PP07G_59))# eliminamos variables perfectamente colineales a las anteriores
vars = colnames(select(eph, contains('PP07F')))
for (var in vars){ 
 eph[var][,1][eph[var][,1]==9] = NA
 eph[var][,1][eph[var][,1]==2] = 0
}
#dummies = c(dummies, vars)
#encadenadas_activ = c(encadenadas_activ, vars)

vars = colnames(select(eph, contains('PP07G')))
for (var in vars){ 
  eph[var][,1][eph[var][,1]==9] = NA
  eph[var][,1][eph[var][,1]==2] = 0
}
#dummies = c(dummies, vars)
#encadenadas_activ = c(encadenadas_activ, vars)
rm(var, vars)

eph$PP07H[eph$PP07H==2] = 0
#dummies = c(dummies, 'PP07H')
#encadenadas_activ = c(encadenadas_activ, 'PP07H')

eph$PP07I[eph$PP07H==1] = -999999
eph$PP07I[eph$PP07I==9] = NA
eph$PP07I[eph$PP07I==2] = 0
#dummies = c(dummies, 'PP07I')
#encadenadas_activ = c(encadenadas_activ, 'PP07I')

eph$PP07J[eph$PP07J==9] = NA
eph$PP07K[eph$PP07K==9] = NA
#categ = c(categ, 'PP07J', 'PP07K')
#encadenadas_activ = c(encadenadas_activ, 'PP07J', 'PP07K')


# Ingresos de la ocupación principal de los asalariados
vars = colnames(select(eph, contains('PP08')))
for (var in vars){ 
  eph[var][,1][eph$ESTADO==2] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$CAT_OCUP != 3 & eph$ESTADO==1] = -99999
}
for (var in vars){
  eph[var][,1][eph[var][,1]==-9] = NA
  print(var)
  print(unique(eph[var][,1]))
}

# dejamos para después ver que hacer con las variables de ingreso


#### Interurbanos (OCUPADOS) ####
rm(var, vars)
eph$PP09A[eph$ESTADO==2] = -9999
eph$PP09A[eph$ESTADO==3] = -9999
eph$PP09A[eph$PP09A == 0] = -999
eph$PP09A[eph$PP09A == 9] = NA
#categ = c(categ, 'PP09A')
#encadenadas_activ = c(encadenadas_activ, 'PP09A')

eph = eph %>% select(!PP09A_ESP) # la eliminamos y que sea el otros

# eliminamos porque no corresponden a GBA y CABA
unique(eph$PP09B)
unique(eph$PP09C)
unique(eph$PP09C_ESP)
eph = eph %>% select(!c(PP09B, PP09C, PP09C_ESP))


### Desocupados ####
vars = colnames(select(eph, contains('PP10')))
for (var in vars){ 
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
}
#categ = c(categ, 'PP10A')
#encadenadas_activ = c(encadenadas_activ, 'PP10A')

eph$PP10C[eph$PP10C == 2] = 0
#dummies = c(dummies, 'PP10C')
#encadenadas_activ = c(encadenadas_activ, 'PP10C')

eph$PP10D[eph$PP10C == 1] = -99999
eph$PP10D[eph$PP10D == 2] = 0
#dummies = c(dummies, 'PP10D')
#encadenadas_activ = c(encadenadas_activ, 'PP10D')

eph$PP10E[eph$PP10D == 0] = -999999
eph$PP10E[eph$PP10E == 9] = NA
#categ = c(categ, 'PP10E')
#encadenadas_activ = c(encadenadas_activ, 'PP10E')

#### Desocupados con empleo anterior finalizada hace 3 años o menos #####
# Es decir, no aplica a:
# - Ocupados e inactivos
# - Los que nunca trabajaron PP10D == 0
# - O los que trabajaron por ultima vez hace mas de 3 años PP10E == 6

vars = colnames(select(eph, contains('PP11')))
for (var in vars[1]){ 
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$PP10D == 0] = -99999
  eph[var][,1][is.na(eph$PP10D)] = -99999
  eph[var][,1][eph$PP10E == 6] = -99999
  eph[var][,1][is.na(eph$PP10E)] = -99999
}
eph$PP11A[eph$PP11A==9] = NA
#categ = c(categ, 'PP11A')
#encadenadas_activ = c(encadenadas_activ, 'PP11A')

eph$PP11B_COD = as.character(eph$PP11B_COD)# lo pasamos a CAES y la unificamos con la CAES de ocupados
for (var in vars[2]){ 
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$PP10D == 0] = -99999
  eph[var][,1][is.na(eph$PP10D)] = -99999
  eph[var][,1][eph$PP10E == 6] = -99999
  eph[var][,1][is.na(eph$PP10E)] = -99999
}
caes_char = caes[c('PP04B_COD','caes_eph_label')]
caes_char$caes_eph_label = as.character(caes_char$caes_eph_label)
caes_char = caes_char[complete.cases(caes_char),]
f = nrow(eph)
eph = merge(eph, caes_char,
      by.x='PP11B_COD', by.y='PP04B_COD', all.x = T)
f == nrow(eph)
rm(caes_char, f)
eph$caes_eph_label.y[eph$PP11B_COD=="-9999"] = "-9999"
eph$caes_eph_label.y[eph$PP11B_COD=="-99999"] = "-99999"

# Unificamos variables de CAES
eph$caes = eph$caes_eph_label.x
eph$caes[eph$caes_eph_label.y != "-9999"] = eph$caes_eph_label.y[eph$caes_eph_label.y != "-9999"]
eph = eph %>% select(!c(caes_eph_label.x, caes_eph_label.y, PP11B_COD))
#categ[categ=="caes_eph_label"] = 'caes'
#encadenadas_activ[encadenadas_activ=="caes_eph_label"] = 'caes'


# Avanzamos
for (var in c("PP11B1", "PP11B2_MES", "PP11B2_ANO", "PP11B2_DIA" ,"PP11C", "PP11C99")){ 
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$PP10D == 0] = -99999
  eph[var][,1][is.na(eph$PP10D)] = -99999
  eph[var][,1][eph$PP10E == 6] = -99999
  eph[var][,1][is.na(eph$PP10E)] = -99999
}

# Servicio doméstico

# Casa flia
eph$PP11B1[!eph$caes %in% c("Servicio domestico", "-99999", "-9999")] = -999999
eph$PP11B1[eph$PP11B1==2] = 0
#dummies = c(dummies, 'PP11B1')
#encadenadas_activ = c(encadenadas_activ, 'PP11B1')

# Tiempo de trabajo
eph$PP11B2_ANO[!eph$caes %in% c("Servicio domestico", "-99999", "-9999")] = -999999
eph$PP11B2_MES[!eph$caes %in% c("Servicio domestico", "-99999", "-9999")] = -999999
eph$PP11B2_DIA[!eph$caes %in% c("Servicio domestico", "-99999", "-9999")] = -999999
eph$PP11B2 = eph$PP11B2_DIA + eph$PP11B2_MES * 30 + eph$PP11B2_ANO * 365
vars = 'PP11B2'
for (var in vars){ 
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$PP10D == 0] = -99999
  eph[var][,1][is.na(eph$PP10D)] = -99999
  eph[var][,1][eph$PP10E == 6] = -99999
  eph[var][,1][is.na(eph$PP10E)] = -99999
}
eph$PP11B2[!eph$caes %in% c("Servicio domestico", "-99999", "-9999")] = -999999
eph$PP11B2[eph$PP11B1==0] = -9999999 # solo es mayor a cero si trabajaba en casa de familia!!!
eph = eph %>% select(!contains('PP11B2_'))
# unificamos con PP04B3 (días de trabajo de servicio doméstico ocupado)
eph$tiempo_dom = eph$PP04B3
eph$tiempo_dom[eph$PP11B2!= -9999] =  eph$PP11B2[eph$PP11B2!= -9999]
eph = eph %>% select(!c('PP04B3','PP11B2'))
#num[num == 'PP04B3'] = 'tiempo_dom'
#encadenadas_activ[encadenadas_activ == 'PP04B3'] = 'tiempo_dom'

# la juntamos con c99 total ninguna es realmente continua
eph$PP11C99[eph$PP11C>0 & eph$PP11C<6] = 1
eph$PP11C99[eph$PP11C>5 & eph$PP11C<9] = 2
eph$PP11C99[eph$PP11C>8 & eph$PP11C<98] = 3
eph$PP11C99[eph$PP11C99==9] = NA
eph = eph%>%select(!PP11C)
# combinamos P11C99 y P04C99
eph$PP11C99[is.na(eph$PP11C99)] =9
eph$PP04C99[is.na(eph$PP04C99)] =9
eph$C99 = eph$PP04C99
eph$C99[eph$PP11C99 != -9999] = eph$PP11C99[eph$PP11C99 != -9999]
eph$C99[eph$C99==9] = NA
eph = eph %>%select(!c(PP04C99,PP11C99))
#categ[categ == 'PP04C99'] = 'C99'
#encadenadas_activ[encadenadas_activ == 'PP04C99'] = 'C99'
rm(var, vars)

eph['PP11D_COD'][,1][eph$ESTADO==1] = -9999
eph['PP11D_COD'][,1][eph$ESTADO==3] = -9999
eph['PP11D_COD'][,1][eph$PP10D == 0] = -99999
eph['PP11D_COD'][,1][is.na(eph$PP10D)] = -99999
eph['PP11D_COD'][,1][eph$PP10E == 6] = -99999
eph['PP11D_COD'][,1][is.na(eph$PP10E)] = -99999

CNO_char = CNO
CNO_char = CNO_char[CNO_char$variable == 'Calificación',]
eph$PP11D_COD = substr(eph$PP11D_COD,5,5)
CNO_char = CNO_char[c('value','label')]
eph = merge(eph, CNO_char,
            by.x='PP11D_COD', by.y='value', all.x = T)
rm(CNO_char)
eph['PP11D_COD'][,1][eph$ESTADO==1] = -9999
eph['PP11D_COD'][,1][eph$ESTADO==3] = -9999
eph['PP11D_COD'][,1][eph$PP10D == 0] = -99999
eph['PP11D_COD'][,1][is.na(eph$PP10D)] = -99999
eph['PP11D_COD'][,1][eph$PP10E == 6] = -99999
eph['PP11D_COD'][,1][is.na(eph$PP10E)] = -99999
eph$label[eph$PP11D_COD=="-9999"] = "-9999"
eph$label[eph$PP11D_COD=="-99999"] = "-99999"
# juntamos con eph$CALIFICACION
eph$CALIFICACION[!is.na(eph$CALIFICACION) &
                   eph$CALIFICACION == "-9999"] = eph$label[!is.na(eph$CALIFICACION) &
                                                              eph$CALIFICACION == "-9999"]
eph = eph %>% select(!c(label, PP11D_COD))

vars = colnames(select(eph, contains('PP11G')))
eph$PP11G_MES[is.na(eph$PP11G_MES)] = 999
for (var in vars){
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$PP10D == 0] = -99999
  eph[var][,1][is.na(eph$PP10D)] = -99999
  eph[var][,1][eph$PP10E == 6] = -99999
  eph[var][,1][is.na(eph$PP10E)] = -99999
  eph[var][,1][eph[var][,1]==99] = NA
}
eph$PP11G = eph$PP11G_DIA + eph$PP11G_MES * 30 + eph$PP11G_ANO * 365

vars = 'PP11G'
for (var in vars){ 
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$PP10D == 0] = -99999
  eph[var][,1][is.na(eph$PP10D)] = -99999
  eph[var][,1][eph$PP10E == 6] = -99999
  eph[var][,1][is.na(eph$PP10E)] = -99999
}

eph$PP11G[eph$caes == 'Servicio domestico' & eph$PP11B1 == 1] = -999999 # NO APLICA A SERV DOM EN CASA DE FLIA
rm(var,vars)
eph = eph%>%select(!c(PP11G_DIA, PP11G_ANO, PP11G_MES))
# unificamos con PP05B2 
eph$tiempo = eph$PP05B2
eph$tiempo[eph$PP05B2 == -9999] = eph$PP11G[eph$PP05B2 == -9999]
eph = eph %>% select(!c('PP05B2','PP11G'))
#num[num == 'PP05B2'] = 'tiempo'
#encadenadas_activ[encadenadas_activ == 'PP05B2'] = 'tiempo'

# unificamos con tiempo_dom: aquellos que no corresponden a serv dom en casas particulares
eph$tiempo[(eph$caes == 'Servicio domestico' & eph$PP11B1 == 1) |
             (eph$caes == 'Servicio domestico' & eph$PP04B2 > 0)  ] =
  eph$tiempo_dom[(eph$caes == 'Servicio domestico' & eph$PP11B1 == 1) |
               (eph$caes == 'Servicio domestico' & eph$PP04B2 > 0)  ]
eph = eph%>%select(!tiempo_dom)
#num = num[!num %in% c("tiempo_dom")]
#encadenadas_activ = encadenadas_activ[!encadenadas_activ %in% c("tiempo_dom")]
#num = num[!num %in% c('PP04B3','PP11B2','PP05B2','PP11G')]
#encadenadas_activ = encadenadas_activ[!encadenadas_activ %in% c('PP04B3','PP11B2','PP05B2','PP11G')]

vars = colnames(select(eph, contains('PP11')))
vars = vars[3:length(vars)]
for (var in vars){ 
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$PP10D == 0] = -99999
  eph[var][,1][is.na(eph$PP10D)] = -99999
  eph[var][,1][eph$PP10E == 6] = -99999
  eph[var][,1][is.na(eph$PP10E)] = -99999
}
#encadenadas_activ = c(encadenadas_activ, vars)

# PP11L y PP11O parecen estar diferenciadas según categoría ocupacional
# PP11L corresponderá a CAT_OCUP == 1 2 y NA
# PP11O corresponderá a CAT_OCUP == 3
# Eliminamos a los asalariados que responden a PP11L
eph$inconsistentes = 0
eph[which((eph$CAT_OCUP==3 & !is.na(eph$CAT_OCUP) ) & eph$PP11L>0), 'inconsistentes'] = 1 # en el resto los asalariados tienen cero
eph = eph[eph$inconsistentes == 0 ,]
eph = eph%>%select(!inconsistentes)
eph$PP11L[(!eph$CAT_OCUP %in% c(NA, 1, 2)) & (!eph$PP11L %in% c(-9999, -99999))] = -999999
eph$PP11L[eph$PP11L == 9] = NA
#categ = c(categ, 'PP11L')

# L1 y M no parecen estar directamente relacionadas con CAT_OCUP
eph$PP11L1[eph$PP11L1 == 0] = -999 # no corresponde secuencia pero no sé por qué
eph$PP11L1[eph$PP11L1 == 3] = NA
eph$PP11M[eph$PP11M == 0] = -999 # no corresponde secuencia pero no sé por qué
#categ = c(categ, 'PP11L1', 'PP11M')

# PP11N y PP11O corresponderá a CAT_OCUP == 3
eph$PP11N[(eph$CAT_OCUP !=3 | is.na(eph$CAT_OCUP))  & (!eph$PP11N %in% c(-9999, -99999))] = -999999
eph$PP11O[(eph$CAT_OCUP !=3 | is.na(eph$CAT_OCUP))  & (!eph$PP11O %in% c(-9999, -99999))] = -999999
eph$PP11N[eph$PP11N == 9] = NA
eph$PP11N[eph$PP11N == 2] = 0
#dummies = c(dummies, 'PP11N')

table(eph$CAT_OCUP, eph$PP11O)
#categ = c(categ, 'PP11O')

# Las restantes también corresponden sólo a asalariados
vars= vars[6: length(vars)]
for(var in vars){
  eph[var][,1][(eph$CAT_OCUP !=3 | is.na(eph$CAT_OCUP))  &
               (!eph[var][,1] %in% c(-9999, -99999))] = -999999
}

# P Q R Y S parecen corresponder solo a PP11O = 1
vars = vars[1:length(vars)-1]
for(var in vars){
  eph[var][,1][(eph$PP11O != 1)  & (eph[var][,1] >-1)] = -999999
}
ph$PP11P[eph$PP11P==9] = NA
eph$PP11P[eph$PP11P==2] = 0
#dummies = c(dummies, 'PP11P')

eph$PP11Q[eph$PP11Q==9] = NA
eph$PP11Q[eph$PP11Q==0] = -999 # no corresponde pero no sé por qué (tal vez responde que solo trabaja 1 persona)
eph$PP11Q[eph$PP11Q==2] = 0
#dummies = c(dummies, 'PP11Q')

eph$PP11R[eph$PP11R==9] = NA
eph$PP11R[eph$PP11R==2] = 0
#dummies = c(dummies, 'PP11R')

eph$PP11S[eph$PP11S==9] = NA
eph$PP11S[eph$PP11S==2] = 0
#dummies = c(dummies, 'PP11S')

eph$PP11T[eph$PP11T==0] = -999
eph$PP11T[eph$PP11T==9] = NA
eph$PP11T[eph$PP11T==2] = 0
#dummies = c(dummies, 'PP11T')

rm(var,vars)

### Ingresos ####
identificadores = c(identificadores, 'PONDII', 'PONDIIO', 'PONDIH')
deciles = c('DECIFR', 'IDECIFR', 'RDECIFR', 'GDECIFR', 'PDECIFR', 'ADECIFR',# total fliar
            'DECCFR', 'IDECCFR', 'RDECCFR', 'GDECCFR', 'PDECCFR', 'ADECCFR', # per cap fliar
            'DECOCUR', 'IDECOCUR', 'RDECOCUR', 'GDECOCUR', 'PDECOCUR', 'ADECOCUR', # ocup principal
            'DECINDR', 'IDECINDR', 'RDECINDR', 'GDECINDR', 'PDECINDR', 'ADECINDR') # total individual

# Eliminamos los deciles que corresponden al interior y a aglomerados pequeños
eph = eph %>% select(!c(IDECIFR , PDECIFR , IDECCFR , PDECCFR , IDECOCUR , PDECOCUR,  IDECINDR , PDECINDR))
deciles = setdiff(deciles,c('IDECIFR' , 'PDECIFR', 'IDECCFR' , 'PDECCFR' ,
                            'IDECOCUR', 'PDECOCUR',  'IDECINDR' , 'PDECINDR')) 


eph[deciles] = apply(eph[deciles],2,as.numeric)

## TRABAJAMOS CON INGRESO FAMILIAR: ASIGNAR BIEN LOS NO RESPUESTA
eph$ITF[eph$DECIFR == 12] = NA
eph$IPCF[eph$DECCFR == 12] = NA

# dado que el ITF tiene tantos más NAs que el ingreso total indiv, a partir del cual se calcula,
# calculamos a mano ITF a ver si obtenemos la misma cantidad de NA

eph$P47T[eph$P47T==-9] = NA
#num = c(num, 'ITF', 'IPCF')
eph[deciles][eph[deciles] == 12] = NA

# OCUPACION PRINCIPAL

# podemos eliminar PP06B porque esta incluida en P21:
eph = eph%>%select(!PP06B)

# las pp08 no son inguales a p21, así que las dejamos
# num = c(num, 'P21', colnames(select(eph,contains('PP08'))))
#encadenadas_activ = c(encadenadas_activ, colnames(select(eph,contains('PP08'))))

eph$P21[eph$P21==-9] = NA

# OTRAS OCUPACIONES
eph$TOT_P12[eph$TOT_P12==-9] = NA
#num = c(num, 'TOT_P12')

# NO LABORALES
# todas terminan con _M, eliminando el monto total
nolab = select(eph,contains(c('_M','_AM')))
nolab = nolab %>% select(!IX_MEN10)
nolab = colnames(nolab)

eph[nolab][eph[nolab]==-9] = NA
eph$T_VI[eph$T_VI==-9] = NA
eph =  eph%>% select(!T_VI) # eliminamos T_VI por ser combilación lineal de los _M
#num = c(num, nolab)

eph =  eph%>% select(!P47T)

#puede que las discrepancias tengan que ver con el uso de ponderadores para corregir por no respuesta
rm(nolab)


# Chequeos  --------------------------------
## Chequeamos que en los vectores de tipos de variables hemos incluido a todas
# categ = unique(categ)
# dummies = unique(dummies)
# num = unique(num)
# ncol(eph)
# length(categ) + length(num) + length(dummies) + length(deciles) + length(pond)
# colnames(eph)[! colnames(eph) %in% c(categ, num, dummies, deciles, pond)]
# identificadores = c( "CODUSU_NRO_HOGAR","id","COMPONENTE" )
# c(categ, num, dummies, deciles, pond)[! c(categ, num, dummies, deciles, pond) %in% colnames(eph)]
# length(categ) + length(num) + length(dummies) # 174 variables
# length(categ) + length(num) + length(dummies) + length(deciles) # 198 variables

## Eliminamos variables que no tengan variablilidad
# colnames(eph)[apply(eph, 2, FUN=function(x) length(unique(x))<2)]
eph = eph %>% select(!V19_AM)
#num = num[num!='V19_AM']

## Aglomerado la escribo como dummy 
eph$AGLOMERADO32 = 0
eph$AGLOMERADO32[eph$AGLOMERADO==32] = 1
eph = eph%>%select(!AGLOMERADO)
#dummies = c(dummies, 'AGLOMERADO32')

# Devolver eph-------
return(eph)
}

########################## Guardo eph limpia ########################## 
saveRDS(eph, file = 'eph_limpia_3T2019.rds')
saveRDS(identificadores, file = 'identificadores_3T2019.rds')
#save(categ, deciles, dummies, num, file = 'tipos_variables.RData')


########################## 
########################## 2 ORDEN ##########################
##########################

identificadores <- readRDS('identificadores_3T2019.rds')
eph <-  readRDS('eph_limpia_3T2019.rds')

##########################  Función para ordenar ########################## 
# toma como argumento la eph y devuelve otra

ordenar = function(eph){ 
  # eph EDO, SOSA & SVARC---------------
  ## ---- Income, property and wealth ----
  # Propietario de la vivienda
  eph$II7_propietario = 0
  eph$II7_propietario[eph$II7 %in% c(1,2)] = 1
  
  # Para subsidios invierto el orden de la dummy: 1 es que no neceesita subsidios o ayuda social
  eph$V5 = eph$V5 * (-1) + 1
  # Para comprar en cuotas invierto el orden de la dummy: 1 si no necesita subsidios para comprar bienes
#  eph$V16 = eph$V16*(-1) + 1
  
  ## ---- Empleo y educación del cabeza de familia ----
  jefes = eph %>%
    group_by(CODUSU_NRO_HOGAR)%>%
    summarise(jefes = sum(CH03 ==1))
  
  # Transformo calificación en una variable ordenada
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
  eph$piso = NA
  eph$piso[eph$IV3 == 3] = 0 # insuf
  eph$piso[eph$IV3 == 2] = 1 # parcialm suf
  eph$piso[eph$IV3 == 1] = 2 # suf
  eph$piso[eph$IV3 == 4] = 2
  unique(eph$piso)
  
  eph$techo = NA
  eph$techo[eph$IV4 %in% c(1,2,3)  & eph$IV5 == 1] = 2
  eph$techo[eph$IV4 %in% c(4,5)  & eph$IV5 == 1] = 2
  eph$techo[is.na(eph$IV4)  & eph$IV5 == 1] = 2
  eph$techo[eph$IV4 %in% c(1,2,3)  & eph$IV5 == 0] = 1
  eph$techo[is.na(eph$IV4)  & eph$IV5 == 0] = 1
  eph$techo[eph$IV4 %in% c(4,5)  & eph$IV5 == 0] = 0
  eph$techo[eph$IV4 %in% c(6,7)] = 0
  
  eph$materiales = NA
  eph$materiales[eph$piso == 2 & eph$techo == 2] = 2
  eph$materiales[eph$piso == 1 ] = 1 ; eph$materiales[eph$techo == 1 ] = 1
  eph$materiales[eph$piso == 0 ] = 0 ; eph$materiales[eph$techo == 0 ] = 0
  unique(eph$materiales)
  
  eph= eph %>% select(!c(IV3, piso, IV4, IV5, techo))
  
  # Acceso al agua
  eph$agua = NA
  eph$agua[eph$IV6 %in% c(2, 3)] = 0
  eph$agua[eph$IV6 == 1 & eph$IV7 %in% c(3, 4)] = 1
  eph$agua[eph$IV6 == 1 & eph$IV7 == 2] = 2
  eph$agua[eph$IV6 == 1 & eph$IV7 == 1] = 3
  eph = eph %>% select(!c(IV6, IV7))
  
  # Sewage
  sum(eph$IV10==2)
  eph$saneamiento = 1 # con baño dentro de la vivienda con arrastre de agua a red publica a camara 
  eph$saneamiento[eph$IV8== 0] = 0
  eph$saneamiento[eph$IV8== 1 & eph$IV9 >1] = 0
  eph$saneamiento[eph$IV8== 1 & eph$IV11 >2] = 0
  eph$saneamiento[eph$IV8== 1 & eph$IV10 > 2] = 0
  eph = eph %>% select(!c(IV8, IV9, IV10, IV11))
  
  # Location
  eph$IV12 = eph$IV12_1 + eph$IV12_2 + eph$IV12_3
  # lo ordeno de menor a mayor bienestar
  eph$IV12 = eph$IV12 * (-1) 
  eph = eph %>% select(!c(IV12_1, IV12_2, IV12_3))
  
  ## ---- Domestic employee ----
  eph$W_domestico = eph$W_12 + eph$W_22
  eph = eph %>% select(!c(W_12, W_22))
  
  # ## Base de Edo et al. (2020), sólo 19 variables y a nivel de hogar ----
  # eph_edo = eph%>%
  #   filter(CH03 == 1) %>% # son todas variables nivel de hogar, me quedo con los jefes
  #   select(CODUSU_NRO_HOGAR, 
  #          CH09, NIVEL_ED, ESTADO, CALIFICACION_ord,
  #          V8, V8_M, V9, V9_M, V10, V10_M,
  #          II7_propietario, V5, V16,
  #          materiales, agua, saneamiento, IV12,
  #          W_domestico)
  # 
  # length(unique(eph_edo$CODUSU_NRO_HOGAR)) / nrow(eph_edo)
  # 
  # nrow(eph_edo[complete.cases(eph_edo),]) / nrow(eph_edo) #muy pocos missings, así que los borramos
  # eph_edo = eph_edo[complete.cases(eph_edo),]
  # save(eph_edo, file = 'eph_edo.Rdata')
  
  # eph_ordinal ----------
  
  # Vivienda
  eph$IV1_ord = 0
  eph$IV1_ord[eph$IV1 == 1] = 1
  eph$IV1_ord[eph$IV1 == 2] = 1
  
  # Hacinamiento
  eph$hacinamiento = eph$II1 / eph$IX_TOT
  
  # Hacienamiento vivienda
  hacinamiento_v = eph %>% 
    group_by(CODUSU) %>%
    summarise(hacinamiento_v = IV2 / sum(unique(IX_TOT)))
  eph = merge(eph, hacinamiento_v, by='CODUSU', all.x=T)
  rm(hacinamiento_v)
  
  # Usos: cuartos extra ademas de para dormir y trabajar
  eph = eph %>% select(!II3)
  eph$usos = eph$II1 / (eph$II2 + eph$II3_1) 
  
  # cocina_lavadero_garage
  eph$II4_1[eph$II4_1==-9999] = 0
  eph$II4_2[eph$II4_2==-9999] = 0
  eph$II4_3[eph$II4_3==-9999] = 0
  eph$II4= eph$II4_1 + eph$II4_2 + eph$II4_3
  
  # usos_II4
  eph$usos_II4 =  eph$II4 - (eph$II5_1 + eph$II6_1) 
  eph = eph %>% select(!c(II1, II2, II3_1,II4_1, II4_2, II4_3,
                          II5, II6, II5_1, II6_1,
                          IV2))
  
  
  # tenencia
  eph$II7[eph$II7<0] = 0
  
  
  # combustible
  eph$II8_ord = NA
  eph$II8_ord[eph$II8 == -9999] = 0
  eph$II8_ord[eph$II8 == 3] = 1
  eph$II8_ord[eph$II8 == 2] = 2
  eph$II8_ord[eph$II8 %in% c(1,4)] = 3
  eph = eph %>% select(!c(II8))
  
  
  # baño
  eph$II9_ord = NA
  eph$II9[eph$II9==-9999] = 5
  eph$II9_ord = eph$II9 * (-1) + 5
  eph = eph %>% select(!'II9')
  
  # V1 + V2 o V22
  eph$V21[eph$V22 == 1] = 1
  eph$V2 = eph$V2 + eph$V21
  eph = eph %>% select(!c(V21, V22))
  
  # V3 o V4
  eph$V3V4 = eph$V3 + eph$V4
  eph = eph %>% select(!c(V3, V4))
  
  # ayuda
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
  
  unique(eph$V19_A)
  unique(eph$V19_B)
  eph$V19_ord = NA
  eph$V19_ord[eph$V19_A == 0 & eph$V19_B == 0] = 1
  eph$V19_ord[eph$V19_A == 1] = 0
  eph$V19_ord[eph$V19_B == 1] = 0
  eph = eph %>% select(!c(V19_A, V19_B))
  
  eph$V3V4_M = eph$V3_M + eph$V4_M
  eph = eph %>% select(!c(V3_M, V4_M))
  
  
  # tareas del hogar
  eph$W4 = eph$W4 / eph$IX_TOT
  eph$W5 = eph$W5 / eph$IX_TOT
  
  eph$W_externo = 0
  eph$W_externo[eph$W_13==1 | eph$W_23==1] = 1 
  eph$W_externo[is.na(eph$W_13)| is.na(eph$W_23)] = NA 
  eph = eph %>% select(!c(W_13, W_23))
  
  
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
  
  eph$CH10_ord = 0
  eph$CH10_ord[eph$CH10==1 & eph$CH06<17] = 1
  eph$CH10_ord[eph$CH10 %in% c(1,2) & eph$CH06>=17] = 1
  eph$CH10_ord[is.na(eph$CH10)] = NA
  eph = eph %>% select(!'CH10')
  
  eph$CH11[eph$CH11==-9999] = 0
  
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
  sum(is.na(eph$CAT))
  eph = eph %>% select(!c(CAT_OCUP, CAT_INAC))
  
  # la línea entre el activo y el inactivo: cómo busca o por qué no busca
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
  eph$PP07A[eph$PP07A<0] = 0
  
  eph$PP07D[eph$PP07D == 0] = 6
  eph$PP07D[eph$PP07D<0] = 0
  
  unique(eph$PP07E)
  eph$PP07E[eph$PP07E<0] = 0
  
  # especie y beneficios
  eph$PP07F = eph$PP07F1 + eph$PP07F2 + eph$PP07F3 + eph$PP07F4
  eph$PP07F[eph$PP07F<0] = 0
  unique(eph$PP07F)
  eph = eph %>% select(!c('PP07F1','PP07F2', 'PP07F3', 'PP07F4'))
  
  eph$benef = eph$PP07G1 + eph$PP07G2 + eph$PP07G3 + eph$PP07G4
  eph$benef[eph$benef<0] = 0
  unique(eph$benef)
  eph = eph %>% select(!c('PP07G1','PP07G2', 'PP07G3', 'PP07G4'))
  
  
  eph$jub = 0
  eph$jub[eph$PP07I==1] = 1
  eph$jub[eph$PP07H==1] = 2
  eph$jub[is.na(eph$PP07I)] = NA
  eph = eph %>% select(!c('PP07H','PP07I'))
  
  eph$PP07J[eph$PP07J<0]=0
  
  eph$PP07K = - eph$PP07K + 6
  eph$PP07K[eph$PP07K>10] = 0
  
  # ingresos
  eph$PP08D = eph$PP08D1 + eph$PP08D4
  eph$PP08D[eph$PP08D<0] = 0
  eph = eph %>% select(!c('PP08D1','PP08D4'))
  
  eph$PP08F = eph$PP08F1 + eph$PP08F2
  eph$PP08F[eph$PP08F<0] = 0
  eph = eph %>% select(!c('PP08F1','PP08F2'))
  
  eph$PP08J1[eph$PP08J1<0] = 0
  eph$PP08J2[eph$PP08J2<0] = 0
  eph$PP08J3[eph$PP08J3<0] = 0
  
  eph$PP09A[eph$PP09A<0] = 0
  
  # Desocupados
  eph$PP10A[eph$PP10A<0] = 0
  eph$PP10A = - eph$PP10A + 5
  
  eph$PP10 = - eph$PP10E + 7  # hace cuanto hizo algo
  eph$PP10[eph$PP10D == 0] = 0 # nunca trabajo
  eph$PP10[eph$PP10C == 1] = 7
  unique(eph$PP10)
  eph$PP10[eph$PP10>1000] = 8
  eph = eph %>% select(!c('PP10C','PP10D','PP10E'))
  
  eph$PP11B1[eph$PP11B1<0] = 0
  
  eph$PP11L[eph$PP11L<0]=0
  eph$PP11L1[eph$PP11L1<0]=0
  
  eph$PP07E11M = eph$PP07E
  eph$PP07E11M[eph$PP11M == 1] = 1
  eph$PP07E11M[eph$PP11M == 2] = 2
  eph$PP07E11M[eph$PP11M == 3] = 4
  eph = eph%>%select(!c('PP07E','PP11M'))
  
  eph$jub[eph$PP11N==1] = 2
  eph = eph%>%select(!'PP11N')
  
  eph$PP11LO = eph$PP11L
  eph$PP11LO[eph$PP11O == 1] = 8
  eph$PP11LO[eph$PP11O == 2] = 6
  eph$PP11LO[eph$PP11O == 3] = 6
  eph$PP11LO[eph$PP11O == 4] = 3
  eph$PP11LO[eph$PP11O == 5] = 9
  eph$PP11LO[eph$PP11O == 6] = 10
  eph$PP11LO[eph$PP11O == 7] = 11
  eph$PP11LO[eph$PP11O == 8] = 5 # otras causas laborales
  eph$PP11LO[eph$PP11O == 9] = 7 #causas personales 
  eph = eph%>%select(!c('PP11L','PP11O'))
  
  eph$PP11PQ = 0 
  eph$PP11PQ[eph$PP11Q == 0] = 1 # no fue la única que se quedó sin trabajo
  eph$PP11PQ[eph$PP11P == 1] = 2 # cerró la empresa
  eph$PP11PQ[is.na(eph$PP11P)] = NA
  eph$PP11PQ[is.na(eph$PP11Q)] = NA
  eph = eph%>%select(!c('PP11P','PP11Q'))
  
  eph$PP11R[eph$PP11R<0] = 0
  
  eph$PP11ST = 0
  eph$PP11ST[eph$PP11S==1] = 1
  eph$PP11ST[eph$PP11T==1] = 1
  eph$PP11ST[eph$PP11S==1 & eph$PP11T ==1] = 2
  eph$PP11ST[is.na(eph$PP11S)]=NA
  eph$PP11ST[is.na(eph$PP11T)]=NA
  eph = eph%>%select(!c('PP11S','PP11T'))
  
  

  
  # DEVUELVO EPH ----
  return(eph)
}

eph = ordenar(eph)
############# Leo las variables que ordené y guardo eph ordenada #############
variables_usadas <- read_excel("G:/Mi unidad/TESIS ECON/Sobre la EPH/variables usadas.xlsx", 
                               col_types = c("skip", "text", "text", 
                                             "text", "text", "text", "skip", "skip"))

names(eph)[!names(eph) %in% c(identificadores, variables_usadas$Nombre)] # variables que no están entres las listadas en variables usadas
variables_usadas$Nombre[!variables_usadas$Nombre %in% names(eph)]
eph = eph %>% select(!names(eph)[!names(eph) %in% c(identificadores, variables_usadas$Nombre)])
saveRDS(eph, file = 'eph_ord_3T2019.rds')



########################## 
########################## 3 NULOS ##########################
########################## 

# Cargo datos
eph <- readRDS('eph_ord_3T2019.rds')
identificadores <- readRDS('identificadores_3T2019.rds')
variables_usadas <- read_excel("G:/Mi unidad/TESIS ECON/Sobre la EPH/variables usadas.xlsx", 
                               col_types = c("skip", "text", "text", 
                                             "text", "text", "text", "skip","skip"))


# eph_complete
# Nos quedamos sólo con las observaciones con datos completos para TODAS las variables
nrow(eph[complete.cases(eph),]) / nrow(eph)
eph_complete = eph[complete.cases(eph),]

# Volvemos a eliminar variables sin variabilidad ahora que sacamos los NA
sin_variab = colnames(eph_complete)[apply(eph_complete, 2, FUN=function(x) length(unique(x))<2)]
sin_variab
#sin_variab = sin_variab[sin_variab!='V19_ord']
eph_complete = eph_complete[!colnames(eph_complete) %in% sin_variab]
rm(sin_variab)

# guardamos
saveRDS(eph_complete, file = 'eph_complete_3T2019.rds')

########################## 
########################## 4 ASPECT ##########################
########################## 
eph = eph_complete
rm(eph_complete)
eph = readRDS('eph_complete_3T2019.rds')

# Encadenadas ---------------

# Eliminar la indicadora de -999 (pues una categoria por raiz o hija ya fue eliminada)
nums <- unlist(lapply(eph, is.numeric))  
tienen9 = names(eph[,nums])[apply(eph[,nums], 2, FUN= function(x) mean(x < -100, na.rm=T) > 0)]
eph[,nums][eph[,nums] < -900] = 0
rm(nums, tienen9)


# ASPECT ---------------
# base para PCA sin identificadores ni ponderadores
eph_scaled = eph[!colnames(eph) %in% c(identificadores)]

variables_usadas$Nombre[!variables_usadas$Nombre %in%colnames(eph)]
variables_usadas =  variables_usadas[variables_usadas$Nombre %in%colnames(eph),]
eph_scaled = eph_scaled[variables_usadas$Nombre]

# Tipos de variable
variables_usadas$Tipo[variables_usadas$Tipo=="categ"] = "nominal"
variables_usadas$Tipo[variables_usadas$Tipo=="num"] = "numerical"
num = variables_usadas$Nombre[variables_usadas$Tipo == "numerical"]
ord = variables_usadas$Nombre[variables_usadas$Tipo == "ordinal"]
categ = c(variables_usadas$Nombre[variables_usadas$Tipo == "nominal"])

unique(variables_usadas$Tipo)

# categóricas a factores
str(eph_scaled[categ])
for (var in categ){
  eph_scaled[,var] = factor(eph_scaled[,var])
}
rm(var)
str(eph_scaled[categ])

# ordinales y numéricas a enteros
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
# saveRDS(scaled_categord$catscores, file='catscores.rds')

# eph_scaled
eph_scaled = cbind(scaled_categord$scoremat,eph_scaled[num])
summary(eph_scaled)
saveRDS(eph_scaled, file='eph_scaled_3T2019.rds')


