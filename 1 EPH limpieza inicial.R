setwd('G:/Mi unidad/TESIS ECON/R')
library(eph)
library(tidyverse)
#library(sjlabelled)
#library(tables)

------------------------------------------------------
# Carga y limpieza individual de variables -----------
------------------------------------------------------
## 4T 2019 ------------------------------------------------

# ephi <- get_microdata(year = 2019,
#                      trimester = 4,
#                      type='individual',
#                      vars = 'all')

lista_bases <- list(get_microdata(year = 2019, trimester = 4,type='individual', vars = 'all'),
                    get_microdata(year = 2019, trimester = 3,type='individual', vars = 'all'),
                    get_microdata(year = 2019, trimester = 2,type='individual', vars = 'all'),
                    get_microdata(year = 2019, trimester = 1,type='individual', vars = 'all'),
                    get_microdata(year = 2018, trimester = 4,type='individual', vars = 'all'),
                    get_microdata(year = 2018, trimester = 3,type='individual', vars = 'all'),
                    get_microdata(year = 2018, trimester = 2,type='individual', vars = 'all'),
                    get_microdata(year = 2018, trimester = 1,type='individual', vars = 'all'))
names(lista_bases[[1]])
ephi <- organize_panels(bahses = lista_bases,
                                   variables = names(lista_bases[[1]]),
                                    window = "trimestral")

unique(ephi$TRIMESTRE)

ephi

# caes
ephi <- organize_caes(ephi)
colnames(ephi)[178:185]
table(ephi$caes_eph_label,ephi$PP04B_COD) # agrupa varias categorías, menos variables discretas :D
ephi = ephi %>% # sólo conservamos caes_eph_label, el resto las eliminamos
  select(!c(colnames(ephi)[178:185][colnames(ephi)[178:185]!='caes_eph_label'],"PP04B_COD"))


# cno
ephi <- organize_cno(ephi)
colnames(ephi)[178:181]
#unique(ephi$CATEGORIA) # la eliminamos
#unique(ephi$JERARQUIA) # la eliminamos pues tenemos la variable CAT_OCUP
#unique(ephi$TECNOLOGIA) # la eliminamos
#unique(ephi$CALIFICACION)
ephi = ephi %>% # sólo conservamos caes_eph_label, el resto las eliminamos
  select(!c('CATEGORIA','JERARQUIA','TECNOLOGIA','PP04D_COD'))


# ephi <- organize_labels(ephi, type='individual') # No siempre coinciden, son viejas

ephh <- get_microdata(year = 2019,
                     trimester = 4,
                     type='hogar',
                     vars = 'all') 
# ephh <- organize_labels(ephh, type='hogar')
mean(is.na(ephh$DECIFR))
mean(is.na(ephi$DECIFR))

## GBA ----------------------------------------------
unique(ephi$REGION)
unique(ephh$REGION)

ephi_gba = ephi %>%
  filter(REGION == 1)

ephh_gba = ephh %>%
  filter(REGION == 1)

rm(ephh, ephi)
mean(is.na(ephh_gba$DECIFR))
mean(is.na(ephi_gba$DECIFR))

# Quitamos año, trimestre, y region

ephi_gba = ephi_gba %>%
  select(!c(ANO4,TRIMESTRE, REGION))

ephh_gba = ephh_gba %>%
  select(!c(ANO4,TRIMESTRE, REGION))

## Merge individual + hogar --------------------------
colnames(ephh_gba)
colnames(ephi_gba)

# Creamos identificadores de cada encuesta (vivienda-hogar y vivienda-hogar-componente)
ephh_gba$CODUSU_NRO_HOGAR = paste(ephh_gba$CODUSU,ephh_gba$NRO_HOGAR,sep="_")
ephi_gba$CODUSU_NRO_HOGAR = paste(ephi_gba$CODUSU,ephi_gba$NRO_HOGAR,sep="_")
ephi_gba$id = paste(ephi_gba$CODUSU_NRO_HOGAR, ephi_gba$COMPONENTE, sep="_")

sum(is.na(ephi_gba$COMPONENTE))
sum(is.na(ephi_gba$CODUSU))
sum(is.na(ephi_gba$NRO_HOGAR))
sum(is.na(ephi_gba$CODUSU_NRO_HOGAR))

sum(is.na(ephh_gba$CODUSU))
sum(is.na(ephh_gba$NRO_HOGAR))
sum(is.na(ephh_gba$CODUSU_NRO_HOGAR))


length(unique(ephh_gba$CODUSU_NRO_HOGAR))
nrow(ephh_gba)


mean(ephh_gba$CODUSU_NRO_HOGAR %in% ephi_gba$CODUSU_NRO_HOGAR)
mean(ephi_gba$CODUSU_NRO_HOGAR %in% ephh_gba$CODUSU_NRO_HOGAR)

# Quitamos variables identificadoras por separado
ephi_gba = ephi_gba %>%
  select(!c(CODUSU,NRO_HOGAR))

ephh_gba = ephh_gba %>%
  select(!c(CODUSU,NRO_HOGAR))

# Quitamos variables repetidas
colnames(ephh_gba)[colnames(ephh_gba) %in% colnames(ephi_gba)]
colnames(ephi_gba)[colnames(ephi_gba) %in% colnames(ephh_gba)]

#mask = !colnames(ephh_gba) %in% colnames(ephi_gba) # eliminaremos todas las de h presentes en i
mask = !colnames(ephi_gba) %in% colnames(ephh_gba) # eliminaremos todas las de i presentes en ih

#mask[length(mask)] = TRUE # excepto la última que corresponde a CODUSU_NRO_HOGAR y la necesitamos para el merge
mask[length(mask)-1] = TRUE

#ephh_gba = ephh_gba[mask]
ephi_gba = ephi_gba[mask]

# Merge por CODUSU_NRO_HOGAR
eph = merge(ephi_gba, ephh_gba, by = 'CODUSU_NRO_HOGAR', all.x=T, all.y = F)
rm(ephh_gba, ephi_gba, mask)

sum(is.na(eph$COMPONENTE))
sum(is.na(eph$CODUSU_NRO_HOGAR))

eph = eph %>% relocate(id, CODUSU_NRO_HOGAR)


# Hay hogares sin encuesta realizada?
unique(eph$REALIZADA) # todos realizaron la encuesta
eph = eph[which(eph$REALIZADA==1),]
eph = eph %>% select(!REALIZADA)

# Hay individuos sin encuesta realizada?
unique(eph$H15)
eph[which(eph$H15==0),]
eph[which(eph$H15==2),]

# Nos quedamos con individuos que realizaron la encuesta
eph = eph[which(eph$H15==1),]
eph = eph %>% select(!H15)

## Limpiezas varias de variables --------------------------


# Fechas
eph$CH05 = lubridate::dmy(eph$CH05) # el dato de edad es colineal y suele ser más acertado que el de fecha de nacimiento
eph = eph%>%select(!CH05)

# Tipos de variables
categ = c()
num = c()
dummies = c()
pond = c()


### Características de la vivienda ####
str(select(eph, contains('IV')))

class(eph$IV1)
unique(eph$IV1)
categ = c(categ,'IV1')

class(eph$IV1_ESP)
levels(eph$IV1_ESP)
unique(eph$IV1_ESP)
eph$IV1_ESP = as.character(eph$IV1_ESP)
unique(eph$IV1_ESP) # sólo indica si es "casilla"
eph$IV1_ESP[eph$IV1_ESP=='CASILLA'] = 'casilla'
eph$IV1_ESP[eph$IV1_ESP=='casilla de chapa'] = 'casilla'
unique(eph$IV1_ESP)
table(eph$IV1,eph$IV1_ESP) # El 6 de IV1 puede referirse a casilla
eph = eph %>% select(!IV1_ESP)
#eph$IV1 = replace_labels(eph$IV1, labels = c("Casilla" = 6))
unique(eph$IV1)


class(eph$IV2)
unique(eph$IV2) ## 99 son no sabe / no responde
eph$IV2[eph$IV2 == 99] = NA
num = c(num,'IV2')

class(eph$IV3)
unique(eph$IV3)
categ = c(categ,'IV3')

class(eph$IV3_ESP)
levels(eph$IV3_ESP)
unique(eph$IV3_ESP)
eph$IV3_ESP = as.character(eph$IV3_ESP)
unique(eph$IV3_ESP)
table(eph$IV3,eph$IV3_ESP) # El 4 de IV3 puede referise a mármol o flexiplast
eph = eph %>% select(!IV3_ESP)
# eph$IV3 = replace_labels(eph$IV3, labels = c("Flexiplast/Marmol" = 4))
unique(eph$IV3)

class(eph$IV4)
unique(eph$IV4)
mean(eph$IV4==9)
eph$IV4[eph$IV4 == 9] = NA
# eph$IV4 = replace_labels(eph$IV4, labels = c("N/S. Depto en propiedad horizontal." = NA))
categ = c(categ,'IV4')

class(eph$IV5)
unique(eph$IV5)
eph$IV5[eph$IV5 == 2] = 0 # ahora es dummy 0 = No, 1 = Sí
dummies = c(dummies,'IV5')

class(eph$IV6)
unique(eph$IV6)
categ = c(categ,'IV6')

class(eph$IV7)
unique(eph$IV7)
categ = c(categ,'IV7')

class(eph$IV7_ESP)
levels(eph$IV7_ESP)
unique(eph$IV7_ESP)
eph$IV7_ESP = as.character(eph$IV7_ESP)
unique(eph$IV7_ESP)
table(eph$IV7,eph$IV7_ESP) 
eph$IV7_ESP[eph$IV7_ESP=='cooperativa privada'] = 'privada'
eph$IV7_ESP[eph$IV7_ESP=='empresa privada'] = 'privada'
# eph$IV7_ESP = add_labels(eph$IV4, labels = c("N/S. Depto en propiedad horizontal." = NA))
eph$IV7[eph$IV7_ESP == 'no tiene'] = 5
eph = eph%>% select(!IV7_ESP)

class(eph$IV8)
unique(eph$IV8)
eph$IV8[eph$IV8 == 2] = 0 # ahora es dummy 0 = No, 1 = Sí
dummies = c(dummies,'IV8')

class(eph$IV9)
unique(eph$IV9)
eph$IV9[eph$IV9 == 0] = -9999 # encadenada con IV8
categ = c(categ,'IV9')

class(eph$IV10)
unique(eph$IV10)
eph$IV10[eph$IV10 == 0] = -9999 # encadenada con IV8
categ = c(categ,'IV10')

class(eph$IV11)
unique(eph$IV11)
eph$IV11[eph$IV11 == 0] = -9999 # encadenada con IV8
categ = c(categ,'IV11')

encadenadas_vivienda = c('IV8', 'IV9', 'IV10', 'IV11')

class(eph$IV12_1)
class(eph$IV12_2)
class(eph$IV12_3)
unique(eph$IV12_1)
unique(eph$IV12_2)
unique(eph$IV12_3)
eph$IV12_1[eph$IV12_1 == 2] = 0
eph$IV12_2[eph$IV12_2 == 2] = 0
eph$IV12_3[eph$IV12_3 == 2] = 0
dummies = c(dummies, 'IV12_1', 'IV12_2', 'IV12_3')


### Características habitacionales del hogar ####
str(select(eph, contains('II')))

class(eph$II1)
unique(eph$II1)
eph$II1[eph$II1 == 99] = NA
num = c(num,'II1')

class(eph$II2)
unique(eph$II2)
table(eph$II1, eph$II2) # encadenadas pero la dejamos así pues es numérica
num = c(num,'II2')

class(eph$II3)
unique(eph$II3)
table(eph$II3, eph$II1)
eph$II3[eph$II3 == 0] = -9999 # encadenada con II1
eph$II3[eph$II3 == 2] = 0 # y luego transformo a dummy
dummies = c(dummies, 'II3')
encadenadas_vivienda =c(encadenadas_vivienda, 'II1', 'II3')

class(eph$II3_1)
unique(eph$II3_1)
eph$II3_1[eph$II3_1 == 0] = 0 # encadenada con II3 (si utiliza alguno como lugar de trabajo...)
# como es numérica la dejamos en 0
table(eph$II3,eph$II3_1)
num = c(num,'II3_1')
encadenadas_vivienda =c(encadenadas_vivienda, 'II1', 'II3', 'II3_1')

class(eph$II4_1)
class(eph$II4_2)
class(eph$II4_3)
unique(eph$II4_1)
unique(eph$II4_2)
unique(eph$II4_3)
table(eph$II1, eph$II4_1)
table(eph$II1, eph$II4_2)
table(eph$II1, eph$II4_3)
eph$II4_1[eph$II4_1 == 0] = -9999 # encadenada con II1
eph$II4_2[eph$II4_2 == 0] = -9999 # encadenada con II1
eph$II4_3[eph$II4_3 == 0] = -9999 # encadenada con II1
eph$II4_1[eph$II4_1 == 2] = 0
eph$II4_2[eph$II4_2 == 2] = 0
eph$II4_3[eph$II4_3 == 2] = 0
dummies = c(dummies, 'II4_1', 'II4_2', 'II4_3')
encadenadas_vivienda =c(encadenadas_vivienda, 'II4_1', 'II4_2', 'II4_3')

class(eph$II5)
unique(eph$II5)
table(eph$II5, eph$II4_1)
table(eph$II5, eph$II4_2)
table(eph$II5, eph$II4_3)
eph$II5[eph$II5 == 0] = - 9999 # encaddenada con II4
eph$II5[eph$II5 == 2] = 0
dummies = c(dummies, 'II5')
encadenadas_vivienda =c(encadenadas_vivienda, 'II5')

class(eph$II5_1)
unique(eph$II5_1)
table(eph$II5, eph$II5_1)
num = c(num, 'II5_1') # encadenada pero es numérica así que la dejamos en 0

class(eph$II6)
unique(eph$II6)
table(eph$II6, eph$II4_1)
table(eph$II6, eph$II4_2)
table(eph$II6, eph$II4_3)
eph$II6[eph$II6 == 0] = -9999 # encadenada con I4
eph$II6[eph$II6 == 2] = 0
dummies = c(dummies, 'II6')
encadenadas_vivienda =c(encadenadas_vivienda, 'II6')

class(eph$II6_1)
unique(eph$II6_1)
table(eph$II6, eph$II6_1)
num = c(num, 'II6_1') # encadenada con I6 pero como es numérica la dejamos en 0

class(eph$II7)
unique(eph$II7)
table(eph$II1, eph$II7) 
eph$II7[eph$II7 == 0] = -9999 # encadenada con II1
categ = c(categ, 'II7')
encadenadas_vivienda =c(encadenadas_vivienda, 'II7')

class(eph$II7_ESP)
unique(eph$II7_ESP) # bien de familia o judicial por divorcio tomada, que quede en "otros
eph = eph %>% select(!II7_ESP)

class(eph$II8)
unique(eph$II8)
table(eph$II8, eph$II1)
eph$II8[eph$II8 == 0] = -9999 # encadenada con II1
categ = c(categ, 'II8')
encadenadas_vivienda =c(encadenadas_vivienda, 'II8')

class(eph$II8_ESP)
eph$II8_ESP = as.character(eph$II8_ESP)
unique(eph$II8_ESP) # todos son electricidad, uno es "no dice"
eph$II8_ESP[eph$II8_ESP!="" & eph$II8_ESP!="No dice"] = "electricidad"
eph$II8[eph$II8_ESP=="No dice"] = NA # el "no dice lo reemplazamos por NA
eph = eph %>% select(!II8_ESP) # y ahora todas la categorias 4 de II8 son electricidad

class(eph$II9)
unique(eph$II9)
table(eph$II9, eph$II1)
eph$II9[eph$II9 == 0] = -9999 # encadenada con II1
categ = c(categ, 'II9')
encadenadas_vivienda =c(encadenadas_vivienda, 'II9')

### Estrategias del hogar ####

estrat = sapply(1:18, FUN=function(x) paste('V',x, sep=''))
estrat = c(estrat, 'V21', 'V22', 'V19_A', 'V19_B')
for (var in estrat){
  print(unique(eph[var]))
  eph[var][eph[var]==9] = NA
  eph[var][eph[var]==2] = 0
}

for (var in estrat){
  print(unique(eph[var]))
}

dummies = c(dummies, estrat)
rm(estrat)


### Resumen del hogar ####
r = c('IX_TOT', 'IX_MEN10','IX_MAYEQ10')
for (var in r){
  print(unique(eph[var]))
}
mean(eph$IX_TOT == eph$IX_MEN10 + eph$IX_MAYEQ10)
eph = eph %>% select(!IX_MAYEQ10)

r = c('IX_TOT', 'IX_MEN10')
num = c(num,r)
rm(r, var)

### Organización del hogar ####

### Indica quién realiza las tareas del hogar
### Como ahora la base la tenemos por componente, armamos dummies a partir de ellas:

# W_11 Realiza tareas de la casa 
# W_12 Servicio doméstico realiza las tareas del hogar
# W_13 Otra persona que no vive en la casa realiza las tareas del hogar

unique(eph$COMPONENTE)
unique(eph$VII1_1) # 99 es NR
unique(eph$VII1_2) # 0 es que no hay otro componente más que esté ayudando, simplemente no matcheará
mean(eph$VII1_1 == 99)

eph$W_11 = 0
eph$W_11[eph$VII1_1==99] = NA
eph$W_11[eph$COMPONENTE == eph$VII1_1] = 1
eph$W_11[eph$COMPONENTE == eph$VII1_2] = 1
mean(is.na(eph$W_11))

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

unique(eph$VII2_1) # 99 es NR
unique(eph$VII2_3) #  0 es que no hay otra persona más ayudando
unique(eph$VII2_4) #  0 es que no hay otra persona más ayudando
unique(eph$VII2_2) #  0 es que no hay otra persona más ayudando


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

dummies = c(dummies, 'W_11', 'W_12', 'W_13', 'W_21', 'W_22', 'W_23')


# chequeo
eph[which(eph$W_11==1),][1:5,c('COMPONENTE','VII1_1','VII1_2')]
eph[which(eph$W_13==1),][c('COMPONENTE','VII1_1','VII1_2')]
sum(is.na(eph[which(eph$W_13==1),][c('COMPONENTE','VII1_1','VII1_2')]))

#data = eph[which(eph$W_13==1),][c('COMPONENTE','VII1_1','VII1_2')]
#eph[grep('^NA', rownames(eph)),]


# eliminar las anteriores
eph = eph %>% select(!c(VII1_1,VII1_2,VII2_1,VII2_2,VII2_3,VII2_4))



# Variables por hogar

W_HOGAR = eph %>% 
  group_by(CODUSU_NRO_HOGAR) %>%
  summarise(W4 = sum(W_11, W_12, W_13), # total de personas que realizan tareas en el hogar
            W5 = sum(W_21, W_22, W_23)) # total de personas que ayudan en las tareas del hogar

eph = merge(eph, W_HOGAR, by='CODUSU_NRO_HOGAR', all.x = T, all.y = F)
num = c(num, c('W4', 'W5'))
rm(W_HOGAR)

### Identificación ####

class(eph$MAS_500)
unique(eph$MAS_500)
eph = eph %>% select(!MAS_500) # en este caso eliminamos MAS_500 pes no tiene variabilidad


sum(is.na(eph$PONDERA))
pond = c(pond, 'PONDERA')

# Características de los miembros del hogar
unique(eph$ESTADO)
categ = c(categ, 'ESTADO')
table(eph$ESTADO)
class(eph$CH03)
unique(eph$CH03)
categ = c(categ, 'CH03')

unique(eph$CH04)
eph$CH04[eph$CH04==2] = 0 # mujer es base
dummies = c(dummies, 'CH04')

min(eph$CH06)
max(eph$CH06)
mean(eph$CH06 == 99)
num = c(num, 'CH06')

unique(eph$CH07)
eph$CH07[eph$CH07 == 9] = NA
categ = c(categ, 'CH07')

# transformo manualmente a dummy la ch08
unique(eph$CH08)
class(eph$CH08)
eph$CH08.1 = 0
eph$CH08.2 = 0
eph$CH08.3 = 0
eph$CH08.4 = 0
eph$CH08.1[eph$CH08 == 1] = 1
eph$CH08.1[eph$CH08 == 12] = 1
eph$CH08.2[eph$CH08 == 2] = 1
eph$CH08.2[eph$CH08 == 12] = 1
eph$CH08.3[eph$CH08 == 3] = 1
eph$CH08.4[eph$CH08 == 4] = 1
eph$CH08.1[eph$CH08 == 9] = NA #ns/nr
eph$CH08.2[eph$CH08 == 9] = NA #ns/nr
eph$CH08.3[eph$CH08 == 9] = NA #ns/nr
eph$CH08.4[eph$CH08 == 9] = NA #ns/nr
unique(eph$CH08.1)

dummies = c(dummies,colnames(select(eph,contains('CH08.'))))
eph = eph%>%select(!CH08)

unique(eph$CH09)
eph$CH09[eph$CH09==2] = 0 #no sabe leer y escribir
dummies = c(dummies, 'CH09')

unique(eph$NIVEL_ED)
eph$NIVEL_ED[eph$NIVEL_ED==7] = 0
categ = c(categ, 'NIVEL_ED')


#### Encadenadas educ ----

unique(eph$CH10)
categ = c(categ, 'CH10')
table(eph$CH10)

unique(eph$CH11)
eph$CH11[eph$CH11 == 9] = NA
eph$CH11[eph$CH10 == 3] = -9999 # responden sólo si asiste
eph$CH11[eph$CH10 == 2] = -9999 # responden sólo si asiste
table(eph$CH10, eph$CH11) 
categ = c(categ, 'CH11')

unique(eph$CH12)
eph$CH12[eph$CH12 == 99] = NA
eph$CH12[eph$CH10 == 3] = -9999 # responden sólo si asiste o asistió
table(eph$CH10, eph$CH12) 
categ = c(categ, 'CH12')

unique(eph$CH13)
eph$CH13[eph$CH13 == 9] = NA
eph$CH13[eph$CH10 == 3] = -9999 # responden sólo si asiste o asistió
table(eph$CH10, eph$CH13) 
eph$CH13[eph$CH13 == 2] = 0
dummies = c(dummies, 'CH13')


class(eph$CH14) # está como character
unique(eph$CH14) 
eph$CH14[which(eph$CH10 == 3)] = "-9999" # responden sólo si asiste o asistió
table(eph$CH13, eph$CH14)
eph$CH14[which(eph$CH13 == 1)] = "-99999" # responden sólo si NO finalizó el nivel
unique(eph$CH14) 
eph$CH14[eph$CH14 == 99] = NA
table(eph$CH10, eph$CH14) 
eph$CH14 = as.numeric(eph$CH14)
categ = c(categ, 'CH14')


encadenadas_educ = c('CH10', 'CH11', 'CH12', 'CH13', 'CH14')

#### Encadenadas migra -----

unique(eph$CH15)
eph$CH15[eph$CH15==9] = NA
categ = c(categ, 'CH15')

unique(eph$CH15_COD)
# sólo responde si en la otra respondio 3, 4 o 5
eph$CH15_COD[which(eph$CH15==1)] = -9999
eph$CH15_COD[which(eph$CH15==2)] = -9999
table(eph$CH15, eph$CH15_COD)
categ = c(categ,'CH15_COD')

encadenadas_migra = c('CH15', 'CH15_cod')

unique(eph$CH16)
eph$CH16[eph$CH16==9] = NA
categ = c(categ, 'CH16')

unique(eph$CH16_COD)
# sólo responde si en la otra respondio 3, 4 o 5
eph$CH16_COD[which(eph$CH16==1)] = -9999
eph$CH16_COD[which(eph$CH16==2)] = -9999
table(eph$CH16, eph$CH16_COD)
categ = c(categ, 'CH16_COD')
  
encadenadas_migra = c(encadenadas_migra, 'CH16', 'CH16_Cod')


### Actividad ####

# Agregamos NA luego de chequear consistencia con inactivos 
# (PP02E por qué NO buscó trabajo en los últimos 30 días)
#  Más adelante chequeamos consistencia con activos, PP02C y vemos que es todo consistente)
table(eph$ESTADO, eph$PP02E)
eph$ESTADO[eph$PP02E!=0 & eph$ESTADO==2] = NA # si NO buscaron trabajo son inactivos
sum(is.na(eph$ESTADO)) # SON 5 OBSERVACIONES, LAS ELIMINAMOS POR INCONSISTENTES!!!
eph = eph[!is.na(eph$ESTADO),] 
table(eph$ESTADO, eph$PP02E)

unique(eph$CAT_OCUP)
eph$CAT_OCUP[eph$CAT_OCUP == 9] = NA
sum(is.na(eph$CAT_OCUP)) # hay un solo nulo
eph$CAT_OCUP[eph$ESTADO == 3] = -9999 # no aplica a inactivos
table(eph$ESTADO, eph$CAT_OCUP)
# quedan 0, desocupados pero que no tienen ocupación anterior 
# ---> tener ocupación anterior o no se refleja en alguna variable? sí, esta PP10D
table(eph$CAT_OCUP, eph$PP10D)
# hay 59 que nunca trabajaron
table(eph$CAT_OCUP, eph$PP10E)
# hay 60 con empleo antrior hace más de 3 años
# 1 que NR cuándo termino su empleo anterior
# 59 para los que PP10E no aplica pues nunca trabajaron
# como no tener ocupación anterior está reflejado en otras varibles, CAT_OCUP es encadenada de ellas y asignamos un -999
unique(eph$CAT_OCUP)
eph$CAT_OCUP[eph$CAT_OCUP==0] = -99999 # no aplica a desocupados sin empleo anterior o con empleo hace más de 3 años
class(eph$CAT_OCUP)
categ = c(categ, 'CAT_OCUP')

unique(eph$CAT_INAC)
table(eph$ESTADO, eph$CAT_INAC) # 0 corresponde a los activos, a los que no aplica, paso a -9999
eph$CAT_INAC[eph$ESTADO == 1] = -9999 
eph$CAT_INAC[eph$ESTADO == 2] = -9999 
unique(eph$CAT_INAC)
table(eph$ESTADO, eph$CAT_INAC) 
categ = c(categ, 'CAT_INAC')

encadenadas_activ = c('ESTADO', 'CAT_OCUP', 'CAT_INAC')

unique(eph$IMPUTA)
dummies = c(dummies, 'IMPUTA')

### Buscar trabajo

# Sólo algunas variables aplican según el estado
select(eph, c(ESTADO,contains('PP02'))) %>% 
  group_by(ESTADO) %>%
  summarize_each(funs(sum)) %>%
  t


# Desocupados (cómo buscó trabajo)
vars = colnames(select(eph, contains('PP02C')))

for (var in vars){
  print(table(eph$ESTADO, eph[var][,1]))
}

for (var in vars){
  eph[var][eph['ESTADO']==1] = -9999 #  si NR por ocupado, ponemos -9999
  eph[var][eph['ESTADO']==3] = -9999 # si NR por inactivo, ponemos -9999
  print(unique(eph[var]))
}


table(eph$ESTADO, eph$PP02C1)
table(eph$ESTADO, eph$PP02C2)
table(eph$ESTADO, eph$PP02C3)
table(eph$IMPUTA, eph$PP02C1)
table(eph$PP02E, eph$PP02C1)

eph[eph$PP02C1 == 0,] # Hay 31 desocupados que tienen 0 en esta variable, como si no les correspondiese responder de qué manera buscó trabajo
for (var in vars){
  eph[var][eph[var]==0] = -999 #  no entendemos por qué va 0, le ponemos -999
  eph[var][eph[var]==2] = 0 #  la pasamos a dummy
  print(unique(eph[var]))
}

dummies = c(dummies, vars)
encadenadas_activ = c(encadenadas_activ, vars)
rm(vars, var)


# Inactivos
unique(eph$PP02E)
table(eph$ESTADO, eph$PP02E)
eph$PP02E[eph$ESTADO==1] = -9999
eph$PP02E[eph$ESTADO==2] = -9999
table(eph$ESTADO, eph$PP02E) # hay 3669 observaciones a las que no corresponde la pregunta, será que sí buscaron en los últimos 30 aunque no en la semana de referencia?

#  no teine que ver con ninguna de la siguientes variables:
table(eph$PP02H, eph$PP02E) #
table(eph$CAT_INAC, eph$PP02E)
table(eph$CAT_INAC, eph$IMPUTA)
categ = c(categ, 'PP02E')

unique(eph$PP02H)
unique(eph$PP02I)
table(eph$ESTADO, eph$PP02H) 
table(eph$ESTADO, eph$PP02I)

eph$PP02H[eph$ESTADO==1] = -9999
eph$PP02H[eph$ESTADO==2] = -9999
table(eph$ESTADO, eph$PP02H)
eph = eph[eph$PP02H!=0,] # eliminamos 7 inactivos que siguen teniendo 0 y no entendemos por qué no les corresponde esa parte del cuestionario
eph$PP02H[eph$PP02H==2] = 0
table(eph$ESTADO, eph$PP02H)
dummies = c(dummies, 'PP02H')

eph$PP02I[eph$ESTADO==1] = -9999
eph$PP02I[eph$ESTADO==2] = -9999
table(eph$ESTADO, eph$PP02I)
eph$PP02I[eph$PP02I==2] = 0
table(eph$ESTADO, eph$PP02I)
dummies = c(dummies, 'PP02I')

encadenadas_activ = c(encadenadas_activ, 'PP02E', 'PP02H', 'PP02I')

### Ocupados ####

# En la semana de referencia

table(eph$ESTADO, eph$PP03C)
unique(eph$PP03C)
eph$PP03C[eph$ESTADO == 2] = -9999
eph$PP03C[eph$ESTADO == 3] = -9999
sum(is.na(eph$PP03C)) # todos los NA correspondían a variable encadenada
table(eph$ESTADO, eph$PP03C) # sigue habiendo ceros: asumo que son ocupados que NO trabajaron en la semana de referencia

table(eph$PP03C, eph$PP03D)
table(eph$ESTADO, eph$PP03D)
unique(eph$PP03D)
eph$PP03D[eph$ESTADO == 2] = -9999
eph$PP03D[eph$ESTADO == 3] = -9999
unique(eph$PP03D) # todos los NA correspondían a variable encadenada
table(eph$PP03C, eph$PP03D)
eph$PP03D[eph$PP03C == 1] = 1 # lo mas probable es que estos tengan una ocupacion!! pues no hay 1 sino. Los que respondían que tenían 1 en la preg anterior
eph$PP03D[eph$PP03C == 0] = 0 # ocupados que NO trabajaron en la semana de referencia (ej vacaciones)
table(eph$ESTADO, eph$PP03D) 
table(eph$PP03C, eph$PP03D) # podemos eliminar la 3C pues queda completamente explicada por la 3D
eph = eph %>% select(!PP03C)
num = c(num, 'PP03D')
table(eph$PP03D, eph$INTENSI) # los que no trabajaron en la semana de referncia

table(eph$ESTADO, eph$PP3E_TOT)
unique(eph$PP3E_TOT)
eph$PP3E_TOT[eph$ESTADO == 2] = -9999
eph$PP3E_TOT[eph$ESTADO == 3] = -9999
unique(eph$PP3E_TOT) # todos los NA correspondían a variable encadenada
table(eph$PP03D, eph$PP3E_TOT) # ok, si no trabaja en la semana de referencia dice cero horas
eph$PP3E_TOT[which(eph$PP3E_TOT == 999)] = NA
num = c(num, 'PP3E_TOT')

table(eph$ESTADO, eph$PP3F_TOT)
table(eph$PP03D, eph$PP3F_TOT) # los que tienen 0 ocupaciones, trabajaron 0 horas y también todos los que tienen una sola ocupación :D
unique(eph$PP3F_TOT)
eph$PP3F_TOT[eph$ESTADO == 2] = -9999
eph$PP3F_TOT[eph$ESTADO == 3] = -9999
unique(eph$PP3F_TOT) # todos los NA correspondían a variable encadenada
eph$PP3F_TOT[which(eph$PP3F_TOT == 999)] = NA
table(eph$ESTADO, eph$PP3F_TOT)
num = c(num, 'PP3F_TOT')


table(eph$ESTADO, eph$PP03G)
unique(eph$PP03G)
eph$PP03G[eph$ESTADO == 2] = -9999
eph$PP03G[eph$ESTADO == 3] = -9999
unique(eph$PP03G) # todos los NA correspondían a variable encadenada
eph$PP03G[which(eph$PP03G == 9)] = NA
eph$PP03G[which(eph$PP03G == 2)] = 0
dummies = c(dummies, 'PP03G')


table(eph$ESTADO, eph$PP03H) # ENCADENADA CON PP03G
unique(eph$PP03H)
eph$PP03H[eph$ESTADO == 2] = -9999
eph$PP03H[eph$ESTADO == 3] = -9999
unique(eph$PP03H) # todos los NA correspondían a variable encadenada
table(eph$PP03G, eph$PP03H)
eph$PP03H[which(eph$PP03H == 0)] = -99999 # los ceros que quedan son x encadenada
eph$PP03H[which(eph$PP03H == 9)] = NA
categ = c(categ, 'PP03H')

encadenadas_activ = c(encadenadas_activ, 
                      'PP03C', 'PP03D', 'PP3E_TOT', 'PP3F_TOT',
                      'PP03G', 'PP03H')

# Todos

table(eph$ESTADO, eph$PP03I)
unique(eph$PP03I)
eph$PP03I[eph$ESTADO == 2] = -9999
eph$PP03I[eph$ESTADO == 3] = -9999
unique(eph$PP03I) # todos los NA correspondían a variable encadenada
eph$PP03I[which(eph$PP03I == 9)] = NA
eph$PP03I[which(eph$PP03I == 2)] = 0
table(eph$ESTADO, eph$PP03I)
dummies = c(dummies,'PP03I')


table(eph$ESTADO, eph$PP03J)
unique(eph$PP03J)
eph$PP03J[eph$ESTADO == 2] = -9999
eph$PP03J[eph$ESTADO == 3] = -9999
unique(eph$PP03J) # todos los NA correspondían a variable encadenada
eph$PP03J[which(eph$PP03J == 9)] = NA
eph$PP03J[which(eph$PP03J == 2)] = 0
table(eph$ESTADO, eph$PP03I)
dummies = c(dummies,'PP03J')

table(eph[,c('PP03I','PP03J','INTENSI')])
unique(eph$INTENSI)
table(eph$ESTADO, eph$INTENSI)
eph$INTENSI[eph$ESTADO == 2] = -9999
eph$INTENSI[eph$ESTADO == 3] = -9999
unique(eph$INTENSI) # todos los NA correspondían a variable encadenada
table(eph$ESTADO, eph$INTENSI)
categ = c(categ, 'INTENSI')

encadenadas_activ = c(encadenadas_activ, 'INTENSI')


#### Ocup principal ####
vars = colnames(select(eph, contains('PP04')))

for (var in vars){
  eph[var][,1][eph$ESTADO == 2] = -9999
  eph[var][,1][eph$ESTADO == 3] = -9999
  print(table(eph$ESTADO, eph[var][,1]))
}

vars[1]
unique(eph$PP04A)
eph$PP04A[eph$PP04A==9] = NA
unique(eph$PP04A)
categ = c(categ, 'PP04A')

unique(eph$caes_eph_label) # ex PP04B_COD
sum(is.na(eph$caes_eph_label))
table(eph$ESTADO, eph$caes_eph_label)
eph$caes_eph_label = as.character(eph$caes_eph_label)
eph$caes_eph_label[eph$ESTADO == 2] = -9999
eph$caes_eph_label[eph$ESTADO == 3] = -9999
unique(eph$caes_eph_label) # los NA correspondían a variable encadenada
categ = c(categ, 'caes_eph_label')
encadenadas_activ = c(encadenadas_activ, 'caes_eph_label')

vars[2] # SOLO RESPONDEN LOS DE servicio doméstico, encadenada de encadenada!!!
unique(eph$PP04B1)
table(eph$caes_eph_label, eph$PP04B1)
eph$PP04B1[eph$caes_eph_label!='Servicio domestico' & eph$ESTADO==1] = -99999
table(eph$caes_eph_label, eph$PP04B1)
eph$PP04B1[eph$PP04B1==2] = 0
unique(eph$PP04B2) # Encadenada de B1 ---> sólo los que trabajan en casas!!!
table(eph$PP04B2, eph$PP04B1)
eph$PP04B2[eph$caes_eph_label!='Servicio domestico' & eph$ESTADO==1] = -99999
table(eph$PP04B2, eph$PP04B1)
eph = eph%>%select(!PP04B1) # directamente eliminamos B1 y si NO trabaja en casa de flia, en cuántas trabaja será 0
num = c(num, 'PP04B2')

# juntamos mes año y día en un proxi de días
eph$PP04B3_ANO[eph$caes_eph_label!='Servicio domestico' & eph$ESTADO==1] = -99999
eph$PP04B3_MES[eph$caes_eph_label!='Servicio domestico' & eph$ESTADO==1] = -99999
eph$PP04B3_DIA[eph$caes_eph_label!='Servicio domestico' & eph$ESTADO==1] = -99999
eph$PP04B3_ANO[eph$PP04B3_ANO==99] = NA
eph$PP04B3_MES[eph$PP04B3_MES==99] = NA
eph$PP04B3_DIA[eph$PP04B3_DIA==99] = NA
unique(eph[c('PP04B3_ANO','PP04B3_MES','PP04B3_DIA')])
eph$PP04B3 = eph$PP04B3_DIA + eph$PP04B3_MES * 30 + eph$PP04B3_ANO * 365
eph$PP04B3[eph$ESTADO == 2] = -9999
eph$PP04B3[eph$ESTADO == 3] = -9999
eph$PP04B3[eph$caes_eph_label!='Servicio domestico' & eph$ESTADO==1] = -99999
unique(eph$PP04B3)
num = c(num, 'PP04B3')
eph = eph %>% select(!contains('PP04B3_'))
table(eph$PP04B2, eph$PP04B3) # ok, los que trabajan en 0 casas tienen 0 hs trabajando en casas

table(eph$PP04B2,eph$PP04C) # NO APLICA A PERSONAL DOMESTICO QUE TRABAJA EN CASA DE FLIA
unique(eph$PP04C[eph$PP04B2>1])
eph$PP04C[eph$PP04B2>0] = -99999
unique(eph$PP04C)
table(eph$PP04C, eph$PP04C99) # ENCADENADA CON LOS NA DE LA VARIABLE ANTERIOR
# Junto la pp04c con la pp04c99 total ninguna es realmente continua
unique(eph$PP04C)
unique(eph$PP04C99)
table(eph$PP04B2,eph$PP04C99)
unique(eph$PP04C99[eph$PP04B2>1])
eph$PP04C99[eph$PP04B2>0] = -99999
eph$PP04C99[eph$PP04C>0 & eph$PP04C<6] = 1
eph$PP04C99[eph$PP04C>5 & eph$PP04C<9] = 2
eph$PP04C99[eph$PP04C>8 & eph$PP04C<98] = 3
table(eph$PP04C, eph$PP04C99)
eph$PP04C99[eph$PP04C99==9] = NA
unique(eph$PP04C99)
eph = eph%>%select(!PP04C)
categ = c(categ, 'PP04C99')


table(eph$ESTADO,eph$CALIFICACION) # ex PP04D_COD
unique(eph$CALIFICACION)
eph$CALIFICACION[eph$ESTADO == 2] = -9999
eph$CALIFICACION[eph$ESTADO == 3] = -9999
unique(eph$CALIFICACION) # los NA eran por variable encadenada
table(eph$PP04B2,eph$CALIFICACION)
eph$CALIFICACION[eph$CALIFICACION=="Ns.Nc"] = NA
eph$CALIFICACION[eph$CALIFICACION=="falta informacion"] = NA
eph$CALIFICACION[eph$CALIFICACION=="otro"] = NA
unique(eph$CALIFICACION)
categ = c(categ, 'CALIFICACION')
encadenadas_activ = c(encadenadas_activ,'CALIIFICACION')
sum(is.na(eph$CALIFICACION))

table(eph$ESTADO,eph$PP04G) 
unique(eph$PP04G)
table(eph$PP04B2,eph$PP04G) # NO APLICA A PERSONAL DOMESTICO QUE TRABAJA EN CASA DE FLIA
eph$PP04G[eph$PP04B2>0] = -99999
eph$PP04G[eph$PP04G==99] = NA
unique(eph$PP04G)
categ=c(categ, 'PP04G')

vars = colnames(select(eph, contains('PP04')))
vars
encadenadas_activ = c(encadenadas_activ, vars)

#### Ocup Independientes ####
# recordemos que CAT_OCUP es la variable que identifica al tipo de trabajador

vars = colnames(select(eph, contains('PP05')))
for (var in vars){ ######### SON SOLO OCUPADOS
  eph[var][,1][eph$ESTADO == 2] = -9999
  eph[var][,1][eph$ESTADO == 3] = -9999
  print(var)
  print(table(eph$ESTADO, eph[var][,1]))
}



# juntamos mes año y día en un proxi de días, y solo para aquellos con CAT_OCUP == 4 (trabajador fliar sin remuneracion)
for (var in vars[1:3]){ 
  print(var)
  print(table(eph$CAT_OCUP, eph[var][,1]))
}
eph$PP05B2_ANO[eph$CAT_OCUP != 4 & eph$ESTADO==1] = -99999
eph$PP05B2_MES[eph$CAT_OCUP != 4 & eph$ESTADO==1] = -99999
eph$PP05B2_DIA[eph$CAT_OCUP != 4 & eph$ESTADO==1] = -99999
for (var in vars[1:3]){ 
  print(var)
  print(table(eph$CAT_OCUP, eph[var][,1]))
}
unique(eph[c(vars[1:3])])
eph$PP05B2 = eph$PP05B2_DIA + eph$PP05B2_MES * 30 + eph$PP05B2_ANO * 365
unique(eph$PP05B2)
eph$PP05B2[eph$ESTADO == 2] = -9999
eph$PP05B2[eph$ESTADO == 3] = -9999
eph$PP05B2[eph$CAT_OCUP != 4 & eph$ESTADO==1] = -99999
unique(eph$PP05B2)
num = c(num, 'PP05B2')
eph = eph %>% select(!contains('PP05B2_'))
encadenadas_activ = c(encadenadas_activ, 'PP05B2')


# Problemas de consistencia entre CAT_OCUP y PP05, PP06: asalariados responden el cuestionario
table(eph$CAT_OCUP, eph$PP05C_1) # tampoco responden trabajadores familiares (CAT OCUP == 4)
table(eph$CAT_OCUP, eph$PP05C_2)
table(eph$CAT_OCUP, eph$PP05C_3)
table(eph$CAT_OCUP, eph$PP06A) # y sucede con todo el formulario de independientes
nrow(eph[eph$CAT_OCUP==3 & eph$PP05C_1 > 0,]) # son 10 observaciones
eph = eph[!(eph$CAT_OCUP==3 & eph$PP05C_1 > 0),] # Las eliminamos

vars = colnames(select(eph, contains('PP05C')))
vars
for (var in vars){ ## sólo independientes (CAT OCUP == 1 o 2)
  eph[var][,1][(!eph$CAT_OCUP %in% c(1,2)) & eph$ESTADO==1] = -99999
  print(var)
  print(unique(eph[var][,1]))
  print(table(eph$CAT_OCUP, eph[var][,1]))
}
categ = c(categ, vars)
encadenadas_activ = c(encadenadas_activ, vars)
rm(var, vars)



eph$PP05E[(!eph$CAT_OCUP %in% c(1,2)) & eph$ESTADO==1] = -99999
eph$PP05F[(!eph$CAT_OCUP %in% c(1,2)) & eph$ESTADO==1] = -99999
eph$PP05H[(!eph$CAT_OCUP %in% c(1,2)) & eph$ESTADO==1] = -99999



print(table(eph$CAT_OCUP, eph$PP05E)) # sólo aplica a los cuenta propia, no a los patrones
eph$PP05E[eph$CAT_OCUP == 1 & eph$ESTADO==1] = -99999
unique(eph$PP05E)
eph$PP05E[eph$PP05E == 2] = 0
dummies = c(dummies, 'PP05E')
encadenadas_activ = c(encadenadas_activ, 'PP05E')

print(table(eph$CAT_OCUP, eph$PP05F))
print(table(eph$CAT_OCUP, eph$PP05H))
unique(eph$PP05F)
unique(eph$PP05H)
eph$PP05H[eph$PP05H==9] = NA      
categ = c(categ, 'PP05F', 'PP05H')
encadenadas_activ = c(encadenadas_activ, 'PP05F', 'PP05H')

# Ingresos de la ocup princ de trabajadores independientes
vars = colnames(select(eph, contains('PP06')))
vars
for (var in vars){ ## sólo independientes
  eph[var][,1][eph$ESTADO==2] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][(!eph$CAT_OCUP %in% c(1,2)) & eph$ESTADO==1] = -99999
  print(var)
  print(unique(eph[var][,1]))
  print(table(eph$ESTADO, eph[var][,1]))
  print(table(eph$CAT_OCUP, eph[var][,1]))
}
rm(var,vars)

unique(eph$PP06A)
eph$PP06A[eph$PP06A == 2] = 0
dummies = c(dummies, 'PP06A')
encadenadas_activ = c(encadenadas_activ, 'PP06A')



# sumamos PP06C y PP06D, total ya tenemos la dummy de si tiene o no socios
table(eph$PP06A,eph$PP06C) # si tiene familiares asociados, no aplica ---> sumaremos
table(eph$PP06A,eph$PP06D) # si tiene familiares asociados, no aplica ---> sumaremos
# Es -7 si no tenía esa ocupación en el mes de referencia
# Es -8 si no tuvo ingresos en el mes de referencia
# El -9 es el NA
eph$PP06C[eph$PP06C== -7] = -999999
eph$PP06D[eph$PP06D== -7] = -999999
eph$PP06C[eph$PP06C== -8] = -999999
eph$PP06D[eph$PP06D== -8] = -999999

unique(eph$PP06C)
unique(eph$PP06D)
unique(eph$PP06C + eph$PP06D)
eph$PP06B = eph$PP06C + eph$PP06D

eph$PP06B[eph$PP06C < -9999] = -99999
eph$PP06B[eph$PP06D < -9999] = -99999
eph$PP06B[eph$ESTADO==2] = -9999
eph$PP06B[eph$ESTADO==3] = -9999

unique(eph$PP06B)
eph$PP06B[eph$PP06B == -9] = NA
eph = eph%>%select(!c(PP06C, PP06D))
# Dejamos para después ver qué hacer con las variables de ingreso



unique(eph$PP06E)
table(eph$PP06E, eph$ESTADO)
table(eph$PP06E, eph$CAT_OCUP)
table(eph$PP06E, eph$PP06A) # no aplica a los que NO tienen socios
eph$PP06E[eph$PP06A == 0] = -999999
table(eph$PP06B, eph$PP06E) # no aplica a los que tenian -7 o -8 en la respuesta anteriror
eph$PP06E[eph$PP06B < 0] = -999999
eph$PP06E[eph$ESTADO==2] = -9999
eph$PP06E[eph$ESTADO==3] = -9999
unique(eph$PP06E)
categ = c(categ, 'PP06E')

table(eph$PP06E, eph$PP06H)
eph$PP06H[eph$PP06E == 1] = -999999 #encadenada de pp06e
eph$PP06H[eph$PP06E ==  -999999 ] = -999999 #encadenada de pp06e
unique(eph$PP06H)
eph$PP06H[eph$PP06H == 2] = 0
dummies = c(dummies, 'PP06H')

encadenadas_activ = c(encadenadas_activ, 'PP06E', 'PP06H')

#### Ocup Asalariados ####
vars = colnames(select(eph, contains('PP07')))
vars


table(eph$PP07A, eph$ESTADO)
table(eph$PP07A, eph$CAT_OCUP)

for (var in vars){ 
  eph[var][,1][eph$ESTADO==2] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$CAT_OCUP != 3 & eph$ESTADO==1] = -99999
  print(var)
  print(unique(eph[var][,1]))
}


# A C D E: Excluyendo servicio doméstico que trabaja en casas particulares
vars[1:4]
unique(eph$caes_eph_label)

unique(eph$PP07A)
table(eph$caes_eph_label, eph$PP07A)
eph$PP07A[eph$ESTADO==1 &
            eph$CAT_OCUP==3 & 
            eph$caes_eph_label == "Servicio domestico" &
            eph$PP04B2>0] = -999999
table(eph$caes_eph_label, eph$PP07A)
eph$PP07A[eph$PP07A==9] = NA
unique(eph$PP07A)
categ = c(categ, 'PP07A')
encadenadas_activ = c(encadenadas_activ, 'PP07A')

unique(eph$PP07C)
eph$PP07C[eph$ESTADO==1 &
            eph$CAT_OCUP==3 & 
            eph$caes_eph_label == "Servicio domestico" &
            eph$PP04B2>0] = -999999
unique(eph$PP07C)
eph$PP07C[eph$PP07C==9] = NA
eph$PP07C[eph$PP07C==2] = 0
unique(eph$PP07D)
eph$PP07D[eph$ESTADO==1 &
            eph$CAT_OCUP==3 & 
            eph$caes_eph_label == "Servicio domestico" &
            eph$PP04B2>0] = -999999
unique(eph$PP07D)
table(eph$PP07C, eph$PP07D) # la C está contenida en la D así que la podemos eliminar
eph = eph %>% select(!PP07C)
eph$PP07D[eph$PP07D==9] = NA
categ = c(categ, 'PP07D')
encadenadas_activ = c(encadenadas_activ, 'PP07D')

unique(eph$PP07E)
eph$PP07E[eph$ESTADO==1 &
            eph$CAT_OCUP==3 & 
            eph$caes_eph_label == "Servicio domestico" &
            eph$PP04B2>0] = -999999
unique(eph$PP07E)
table(eph$PP07E, eph$PP07D) # ENCADENADA CON LA D
eph$PP07E[eph$PP07D==0] = -999999
unique(eph$PP07E)
eph$PP07E[eph$PP07E==9] = NA
unique(eph$PP07E)
categ = c(categ, 'PP07E')
encadenadas_activ = c(encadenadas_activ, 'PP07E')

# Incluyendo servicio doméstico
rm(var,vars)
vars = colnames(select(eph, contains('PP07')))
vars[4:length(vars)]

for (var in vars){ 
  print(var)
  print(table(eph$ESTADO, eph[var][,1]))
  print(table(eph$CAT_OCUP, eph[var][,1]))
}



# eliminamos variables perfectamente colineales a las anteriores
eph = eph %>% select(!c(PP07F5, PP07G_59))


vars = colnames(select(eph, contains('PP07F')))
vars
for (var in vars){ 
  print(unique(eph[var][,1]))
}
for (var in vars){ 
 eph[var][,1][eph[var][,1]==9] = NA
 eph[var][,1][eph[var][,1]==2] = 0
 print(var)
 print(unique(eph[var][,1]))
}
dummies = c(dummies, vars)
encadenadas_activ = c(encadenadas_activ, vars)

vars = colnames(select(eph, contains('PP07G')))
vars
for (var in vars){ 
  print(unique(eph[var][,1]))
}
for (var in vars){ 
  eph[var][,1][eph[var][,1]==9] = NA
  eph[var][,1][eph[var][,1]==2] = 0
  print(var)
  print(unique(eph[var][,1]))
}
dummies = c(dummies, vars)
encadenadas_activ = c(encadenadas_activ, vars)
rm(var, vars)

unique(eph$PP07H)
eph$PP07H[eph$PP07H==2] = 0
dummies = c(dummies, 'PP07H')
encadenadas_activ = c(encadenadas_activ, 'PP07H')

table(eph$PP07H, eph$PP07I)
eph$PP07I[eph$PP07H==1] = -999999
eph$PP07I[eph$PP07I==9] = NA
eph$PP07I[eph$PP07I==2] = 0
dummies = c(dummies, 'PP07I')
encadenadas_activ = c(encadenadas_activ, 'PP07I')

unique(eph$PP07J)
eph$PP07J[eph$PP07J==9] = NA
unique(eph$PP07K)
eph$PP07K[eph$PP07K==9] = NA
categ = c(categ, 'PP07J', 'PP07K')
encadenadas_activ = c(encadenadas_activ, 'PP07J', 'PP07K')


# Ingresos de la ocupación principal de los asalariados

vars = colnames(select(eph, contains('PP08')))
vars
for (var in vars){ 
  eph[var][,1][eph$ESTADO==2] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$CAT_OCUP != 3 & eph$ESTADO==1] = -99999
  print(var)
  print(unique(eph[var][,1]))
  print(table(eph$ESTADO, eph[var][,1]))
  print(table(eph$CAT_OCUP, eph[var][,1]))
}

# quiero ver si alguna es la suma de las otras, pero no
unique(eph$PP08D1)
PP08D = eph[eph$PP08D1>-1 & !is.na(eph$PP08D1), 'PP08D1'] 
resto = eph[eph$PP08D1>-1 & !is.na(eph$PP08D1),  vars[2:length(vars)]] 
resto = rowSums(resto, na.rm =T)
length(PP08D)
length(resto)
mean(PP08D==resto)
eph$PP08D1[eph$PP08D1>-1 & !is.na(eph$PP08D1)][PP08D==resto]
mean(PP08D>=resto)
eph[eph$PP08D1>-1 & !is.na(eph$PP08D1), vars][PP08D<resto,]
rm(PP08D, resto)

vars
for (var in vars){
  print(unique(eph[var][,1]))
}
for (var in vars){
  eph[var][,1][eph[var][,1]==-9] = NA
  print(var)
  print(unique(eph[var][,1]))
}

# dejamos para después ver que hacer con las variables de ingreso


#### Interurbanos (OCUPADOS) ####
rm(var, vars)

table(eph$ESTADO, eph$PP09A)
table(eph$CAT_OCUP, eph$PP09A) # para todas las categorías ocupacionales
eph$PP09A[eph$ESTADO==2] = -9999
eph$PP09A[eph$ESTADO==3] = -9999
table(eph$ESTADO, eph$PP09A)
table(eph$AGLOMERADO, eph$PP09A)
eph[eph$PP09A==0,] #sigue habiendo ceros
eph$PP09A[eph$PP09A == 0] = -999
unique(eph$PP09A)
eph$PP09A[eph$PP09A == 9] = NA
unique(eph$PP09A)
categ = c(categ, 'PP09A')
encadenadas_activ = c(encadenadas_activ, 'PP09A')

unique(eph$PP09A_ESP)
eph = eph %>% select(!PP09A_ESP) # la eliminamos y que sea el otros

# eliminamos porque no corresponden a GBA y CABA
unique(eph$PP09B)
unique(eph$PP09C)
unique(eph$PP09C_ESP)
eph = eph %>% select(!c(PP09B, PP09C, PP09C_ESP))



### Desocupados ####

vars = colnames(select(eph, contains('PP10')))
vars

table(eph$ESTADO, eph$PP10A)

for (var in vars){ 
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  print(var)
  print(unique(eph[var][,1])) # ya no hay NA
}

categ = c(categ, 'PP10A')
encadenadas_activ = c(encadenadas_activ, 'PP10A')

eph$PP10C[eph$PP10C == 2] = 0
dummies = c(dummies, 'PP10C')
encadenadas_activ = c(encadenadas_activ, 'PP10C')

table(eph$PP10C, eph$PP10D) # la D está encadenada con la C
eph$PP10D[eph$PP10C == 1] = -99999
unique(eph$PP10D)
eph$PP10D[eph$PP10D == 2] = 0
dummies = c(dummies, 'PP10D')
encadenadas_activ = c(encadenadas_activ, 'PP10D')

table(eph$PP10D, eph$PP10E) # la E está encadenada con la D (solo responden los que sí trabajaron alguna vez)
eph$PP10E[eph$PP10D == 0] = -999999
unique(eph$PP10E)
eph$PP10E[eph$PP10E == 9] = NA
categ = c(categ, 'PP10E')
encadenadas_activ = c(encadenadas_activ, 'PP10E')

#### Desocupados con empleo anterior finalizada hace 3 años o menos #####
table(eph$PP10E, eph$PP11A)
table(eph$ESTADO, eph$PP11A)

# Es decir, no aplica a:
# - Ocupados e inactivos
# - Los que nunca trabajaron PP10D == 0
# - O los que trabajaron por ultima vez hace mas de 3 años PP10E == 6

vars = colnames(select(eph, contains('PP11')))
vars

vars[1]
for (var in vars[1]){ 
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$PP10D == 0] = -99999
  eph[var][,1][is.na(eph$PP10D)] = -99999
  eph[var][,1][eph$PP10E == 6] = -99999
  eph[var][,1][is.na(eph$PP10E)] = -99999
}
unique(eph$PP11A)
table(eph$PP10E, eph$PP11A)
table(eph$ESTADO, eph$PP11A)
eph$PP11A[eph$PP11A==9] = NA
categ = c(categ, 'PP11A')
encadenadas_activ = c(encadenadas_activ, 'PP11A')


unique(eph$PP11B_COD) # lo pasamos a CAES y la unificamos con la CAES de ocupados
eph$PP11B_COD = as.character(eph$PP11B_COD)
vars[2]
for (var in vars[2]){ 
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$PP10D == 0] = -99999
  eph[var][,1][is.na(eph$PP10D)] = -99999
  eph[var][,1][eph$PP10E == 6] = -99999
  eph[var][,1][is.na(eph$PP10E)] = -99999
}
unique(eph$PP11B_COD)
caes_char = caes[c('PP04B_COD','caes_eph_label')]
caes_char$caes_eph_label = as.character(caes_char$caes_eph_label)
caes_char = caes_char[complete.cases(caes_char),]
nrow(caes_char)
f = nrow(eph)
eph = merge(eph, caes_char,
      by.x='PP11B_COD', by.y='PP04B_COD', all.x = T)
f == nrow(eph)
rm(caes_char, f)
eph$caes_eph_label.y[eph$PP11B_COD=="-9999"] = "-9999"
eph$caes_eph_label.y[eph$PP11B_COD=="-99999"] = "-99999"
unique(eph$caes_eph_label.y)

# Unificamos variables de CAES
eph$caes = eph$caes_eph_label.x
table(eph$caes_eph_label.x, eph$caes_eph_label.y)
eph$caes[eph$caes_eph_label.y != "-9999"] = eph$caes_eph_label.y[eph$caes_eph_label.y != "-9999"]
unique(eph$caes)  
table(eph$caes,eph$ESTADO)  
table(eph$caes,eph$PP10D) 
table(eph$caes,eph$PP10E) 
unique(eph$caes)
eph = eph %>% select(!c(caes_eph_label.x, caes_eph_label.y, PP11B_COD))
categ[categ=="caes_eph_label"] = 'caes'
encadenadas_activ[encadenadas_activ=="caes_eph_label"] = 'caes'
unique(eph$caes)


# Avanzamos
vars
for (var in vars[3:8]){ 
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$PP10D == 0] = -99999
  eph[var][,1][is.na(eph$PP10D)] = -99999
  eph[var][,1][eph$PP10E == 6] = -99999
  eph[var][,1][is.na(eph$PP10E)] = -99999
}

# Servicio doméstico

# Casa flia
vars[3]
unique(eph$PP11B1)
table(eph$caes, eph$PP11B1)
eph$PP11B1[!eph$caes %in% c("Servicio domestico", "-99999", "-9999")] = -999999
unique(eph$PP11B1)
eph$PP11B1[eph$PP11B1==2] = 0
dummies = c(dummies, 'PP11B1')
encadenadas_activ = c(encadenadas_activ, 'PP11B1')

# Tiempo de trabajo
table(eph$caes,eph$PP11B2_ANO)
unique(eph$PP11B2_ANO)
unique(eph$PP11B2_MES)
unique(eph$PP11B2_DIA)
eph$PP11B2_ANO[!eph$caes %in% c("Servicio domestico", "-99999", "-9999")] = -999999
eph$PP11B2_MES[!eph$caes %in% c("Servicio domestico", "-99999", "-9999")] = -999999
eph$PP11B2_DIA[!eph$caes %in% c("Servicio domestico", "-99999", "-9999")] = -999999
# cual es la rrelación con las PP11G_ que también preguntan por el tiempo??
unique(eph$PP11G_ANO[eph$PP11B2_ANO>-1])
unique(eph$PP11G_MES[eph$PP11B2_MES>-1])
unique(eph$PP11G_DIA[eph$PP11B2_DIA>-1])
table(eph$PP11G_ANO[eph$PP11B2_ANO>-1], eph$PP11B2_ANO[eph$PP11B2_ANO>-1])
table(eph$PP11G_MES[eph$PP11B2_MES>-1], eph$PP11B2_MES[eph$PP11B2_MES>-1])
table(eph$PP11G_DIA[eph$PP11B2_DIA>-1], eph$PP11B2_DIA[eph$PP11B2_DIA>-1])
table(eph$PP11G_ANO[eph$PP11B2_ANO>-1], eph$PP11B1[eph$PP11B2_ANO>-1])
table(eph$PP11G_MES[eph$PP11B2_MES>-1], eph$PP11B1[eph$PP11B2_MES>-1])
table(eph$PP11G_DIA[eph$PP11B2_DIA>-1], eph$PP11B1[eph$PP11B2_DIA>-1])
# solo son mayores a cero cuando NO corresponden a casas de familia, PP11B1 = 0


unique(eph[c('PP11B2_ANO','PP11B2_MES','PP11B2_DIA')])
eph$PP11B2 = eph$PP11B2_DIA + eph$PP11B2_MES * 30 + eph$PP11B2_ANO * 365
unique(eph$PP11B2)
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
unique(eph$PP11B2)
table(eph$PP11B1, eph$PP11B2) # encadenada!!!!
eph$PP11B2[eph$PP11B1==0] = -9999999 # solo es mayor a cero si trabajaba en casa de familia!!!
unique(eph$PP11B2)
eph = eph %>% select(!contains('PP11B2_'))
# unificamos con PP04B3 (días de trabajo de servicio doméstico ocupado)
unique(eph$PP11B2) # el -9999999 son los que no trabajaban en casas de flias
unique(eph$PP04B3) # el -999999 eran los que no trabajaban en casas de flias
table(eph$PP11B2, eph$PP04B3)
eph$tiempo_dom = eph$PP04B3
eph$tiempo_dom[eph$PP11B2!= -9999] =  eph$PP11B2[eph$PP11B2!= -9999]
unique(eph$tiempo_dom)
table(eph$ESTADO, eph$tiempo_dom)
table(eph$caes, eph$tiempo_dom)
eph = eph %>% select(!c('PP04B3','PP11B2'))
num[num == 'PP04B3'] = 'tiempo_dom'
encadenadas_activ[encadenadas_activ == 'PP04B3'] = 'tiempo_dom'


table(eph$caes,eph$PP11C)
table(eph$PP11C, eph$PP11C99) # ENCADENADA CON LOS NA DE LA VARIABLE ANTERIOR
# la juntamos con c99 total ninguna es realmente continua
unique(eph$PP11C)
eph$PP11C99[eph$PP11C>0 & eph$PP11C<6] = 1
eph$PP11C99[eph$PP11C>5 & eph$PP11C<9] = 2
eph$PP11C99[eph$PP11C>8 & eph$PP11C<98] = 3
table(eph$PP11C, eph$PP11C99)
eph$PP11C99[eph$PP11C99==9] = NA
unique(eph$PP11C99)
eph = eph%>%select(!PP11C)
# combinamos P11C99 y P04C99
table(eph$PP11C99,eph$PP04C99)
eph$PP11C99[is.na(eph$PP11C99)] =9
eph$PP04C99[is.na(eph$PP04C99)] =9
eph$C99 = eph$PP04C99
eph$C99[eph$PP11C99 != -9999] = eph$PP11C99[eph$PP11C99 != -9999]
table(eph$PP11C99,eph$C99)
table(eph$C99,eph$PP04C99)
eph = eph %>%select(!c(PP04C99,PP11C99))
categ[categ == 'PP04C99'] = 'C99'
encadenadas_activ[encadenadas_activ == 'PP04C99'] = 'C99'

rm(var, vars)
unique(eph$PP11D_COD)
eph['PP11D_COD'][,1][eph$ESTADO==1] = -9999
eph['PP11D_COD'][,1][eph$ESTADO==3] = -9999
eph['PP11D_COD'][,1][eph$PP10D == 0] = -99999
eph['PP11D_COD'][,1][is.na(eph$PP10D)] = -99999
eph['PP11D_COD'][,1][eph$PP10E == 6] = -99999
eph['PP11D_COD'][,1][is.na(eph$PP10E)] = -99999

CNO_char = CNO
CNO_char = CNO_char[CNO_char$variable == 'Calificación',]
class(eph$PP11D_COD)
eph$PP11D_COD = substr(eph$PP11D_COD,5,5)
unique(eph$PP11D_COD)
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
unique(eph$label)
unique(eph$PP11D_COD[is.na(eph$label)])
# juntamos con eph$CALIFICACION
unique(eph$CALIFICACION)
table(eph$label, eph$CALIFICACION)
eph$CALIFICACION[!is.na(eph$CALIFICACION) &
                   eph$CALIFICACION == "-9999"] = eph$label[!is.na(eph$CALIFICACION) &
                                                              eph$CALIFICACION == "-9999"]
table(eph$label, eph$CALIFICACION)
eph = eph %>% select(!c(label, PP11D_COD))
table(eph$CALIFICACION, eph$ESTADO)

vars = colnames(select(eph, contains('PP11G')))
vars
sum(is.na(eph$PP11G_MES))
max(eph$PP11G_MES, na.rm=T)
eph$PP11G_MES[is.na(eph$PP11G_MES)] = 999
sum(is.na(eph$PP11G_DIA))
sum(is.na(eph$PP11G_ANO))
for (var in vars){
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$PP10D == 0] = -99999
  eph[var][,1][is.na(eph$PP10D)] = -99999
  eph[var][,1][eph$PP10E == 6] = -99999
  eph[var][,1][is.na(eph$PP10E)] = -99999
  eph[var][,1][eph[var][,1]==99] = NA
}
table(eph$CAT_OCUP, eph$PP11G_MES) # no se relaciona con cat_ocup
table(eph$caes,eph$PP11G_MES) # ni con caes
unique(eph$PP11G_MES[eph$caes=='Servicio domestico'])
unique(eph$PP11G_ANO[eph$caes=='Servicio domestico'])
unique(eph$PP11G_DIA[eph$caes=='Servicio domestico'])
unique(eph$PP11G_MES[eph$caes=='Servicio domestico' & eph$PP11B1 == 1])
unique(eph$PP11G_ANO[eph$caes=='Servicio domestico' & eph$PP11B1 == 1])
unique(eph$PP11G_DIA[eph$caes=='Servicio domestico' & eph$PP11B1 == 1])
unique(eph$PP11G_MES)
sum(is.na(eph$PP11G_MES))
sum(is.na(eph$PP11G_DIA))
sum(is.na(eph$PP11G_ANO))
unique(eph[c('PP11G_ANO','PP11G_MES','PP11G_DIA')])
eph$PP11G = eph$PP11G_DIA + eph$PP11G_MES * 30 + eph$PP11G_ANO * 365
unique(eph$PP11G)
vars = 'PP11G'
for (var in vars){ 
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$PP10D == 0] = -99999
  eph[var][,1][is.na(eph$PP10D)] = -99999
  eph[var][,1][eph$PP10E == 6] = -99999
  eph[var][,1][is.na(eph$PP10E)] = -99999
}
unique(eph$PP05B2)
eph$PP11G[eph$caes == 'Servicio domestico' & eph$PP11B1 == 1] = -999999 # NO APLICA A SERV DOM EN CASA DE FLIA
rm(var,vars)
eph = eph%>%select(!c(PP11G_DIA, PP11G_ANO, PP11G_MES))
# unificamos con PP05B2 
unique(eph$PP05B2)
eph$tiempo = eph$PP05B2
eph$tiempo[eph$PP05B2 == -9999] = eph$PP11G[eph$PP05B2 == -9999]
unique(eph$tiempo)
eph = eph %>% select(!c('PP05B2','PP11G'))
num[num == 'PP05B2'] = 'tiempo'
encadenadas_activ[encadenadas_activ == 'PP05B2'] = 'tiempo'

# unificamos con tiempo_dom: aquellos que no corresponden a serv dom en casas particulares
length(eph$tiempo[(eph$caes == 'Servicio domestico' & eph$PP11B1 == 1) |
            (eph$caes == 'Servicio domestico' & eph$PP04B2 > 0)  ])
length(eph$tiempo_dom[eph$tiempo_dom>0])

eph$tiempo[(eph$caes == 'Servicio domestico' & eph$PP11B1 == 1) |
             (eph$caes == 'Servicio domestico' & eph$PP04B2 > 0)  ] =
  eph$tiempo_dom[(eph$caes == 'Servicio domestico' & eph$PP11B1 == 1) |
               (eph$caes == 'Servicio domestico' & eph$PP04B2 > 0)  ]
unique(eph$tiempo)
eph = eph%>%select(!tiempo_dom)
num = num[!num %in% c("tiempo_dom")]
encadenadas_activ = encadenadas_activ[!encadenadas_activ %in% c("tiempo_dom")]

# chequeamos que ya eliminamos todas las variables de tiempo
select(eph,contains('PP04B3')) # ocupados domestico casas
select(eph,contains('PP11B2')) # desocupados domestico casas
select(eph,contains('PP05B2')) # ocupados otros
select(eph,contains('PP11G')) #desocupados otros
num = num[!num %in% c('PP04B3','PP11B2','PP05B2','PP11G')]
encadenadas_activ = encadenadas_activ[!encadenadas_activ %in% c('PP04B3','PP11B2','PP05B2','PP11G')]



table(eph$ESTADO, eph$PP11L)

vars = colnames(select(eph, contains('PP11')))
vars = vars[3:length(vars)]
vars
for (var in vars){ 
  eph[var][,1][eph$ESTADO==1] = -9999
  eph[var][,1][eph$ESTADO==3] = -9999
  eph[var][,1][eph$PP10D == 0] = -99999
  eph[var][,1][is.na(eph$PP10D)] = -99999
  eph[var][,1][eph$PP10E == 6] = -99999
  eph[var][,1][is.na(eph$PP10E)] = -99999
}
encadenadas_activ = c(encadenadas_activ, vars)

# PP11L y PP11O parecen estar diferenciadas según categoría ocupacional
unique(eph$PP11L)
unique(eph$CAT_OCUP)
eph$PP11L[is.na(eph$CAT_OCUP)]  # el que tienen NA en CAT OCUP, tiene 9 en PP11L 
eph$PP11O[is.na(eph$CAT_OCUP)] # el que tienen NA en CAT OCUP, tiene 0 en PP11o 
table(eph$CAT_OCUP, eph$PP11L) # excepto por 3 asalariados, sólo responden independientes
table(eph$CAT_OCUP, eph$PP11O) # sólo responden asalariados, excepto 3
# PP11L corresponderá a CAT_OCUP == 1 2 y NA
# PP11O corresponderá a CAT_OCUP == 3
# Eliminamos a los asalariados que responden a PP11L
eph$inconsistentes = 0
eph[which((eph$CAT_OCUP==3 & !is.na(eph$CAT_OCUP) ) & eph$PP11L>0), 'inconsistentes'] = 1 # en el resto los asalariados tienen cero
eph = eph[eph$inconsistentes == 0 ,]
eph = eph%>%select(!inconsistentes)
table(eph$CAT_OCUP, eph$PP11L) # ahora sólo responden independientes
table(eph$CAT_OCUP, eph$PP11O) # ahora responden todos los asalariados
unique(eph$PP11L)
eph$PP11L[(!eph$CAT_OCUP %in% c(NA, 1, 2)) & (!eph$PP11L %in% c(-9999, -99999))] = -999999
unique(eph$PP11L)
eph$PP11L[eph$PP11L == 9] = NA
categ = c(categ, 'PP11L')


# L1 y M no parecen estar directamente relacionadas con CAT_OCUP
table(eph$CAT_OCUP, eph$PP11L1) # solo responden asalariados pero muchos asalariados no responden
table(eph$CAT_OCUP, eph$PP11M) # sólo responden asalariados pero muchos asalariados no responden
table(eph$PP11L1, eph$PP11M) # no son exluyentes
eph$PP11L1[eph$PP11L1 == 0] = -999 # no corresponde secuencia pero no sé por qué
eph$PP11L1[eph$PP11L1 == 3] = NA
eph$PP11M[eph$PP11M == 0] = -999 # no corresponde secuencia pero no sé por qué
categ = c(categ, 'PP11L1', 'PP11M')


# PP11N y PP11O corresponderá a CAT_OCUP == 3
table(eph$CAT_OCUP, eph$PP11N)
table(eph$CAT_OCUP, eph$PP11O)
eph$PP11N[(eph$CAT_OCUP !=3 | is.na(eph$CAT_OCUP))  & (!eph$PP11N %in% c(-9999, -99999))] = -999999
eph$PP11O[(eph$CAT_OCUP !=3 | is.na(eph$CAT_OCUP))  & (!eph$PP11O %in% c(-9999, -99999))] = -999999
table(eph$CAT_OCUP, eph$PP11N)
unique(eph$PP11N)
eph$PP11N[eph$PP11N == 9] = NA
eph$PP11N[eph$PP11N == 2] = 0
dummies = c(dummies, 'PP11N')

table(eph$CAT_OCUP, eph$PP11O)
categ = c(categ, 'PP11O')

# Las restantes también corresponden sólo a asalariados
table(eph$CAT_OCUP, eph$PP11P)
table(eph$CAT_OCUP, eph$PP11Q)
table(eph$CAT_OCUP, eph$PP11R)
table(eph$CAT_OCUP, eph$PP11S)
table(eph$CAT_OCUP, eph$PP11T)
vars
vars= vars[6: length(vars)]
vars
for(var in vars){
  eph[var][,1][(eph$CAT_OCUP !=3 | is.na(eph$CAT_OCUP))  &
               (!eph[var][,1] %in% c(-9999, -99999))] = -999999
}

# P Q R Y S parecen corresponder solo a PP11O = 1
table(eph$PP11O, eph$PP11P)
table(eph$PP11O, eph$PP11Q) # sí aunque también quedan 0
table(eph$PP11O, eph$PP11R)
table(eph$PP11O, eph$PP11S)
vars = vars[1:length(vars)-1]
vars
for(var in vars){
  eph[var][,1][(eph$PP11O != 1)  & (eph[var][,1] >-1)] = -999999
}

unique(eph$PP11P)
eph$PP11P[eph$PP11P==9] = NA
eph$PP11P[eph$PP11P==2] = 0
dummies = c(dummies, 'PP11P')

unique(eph$PP11Q)
eph$PP11Q[eph$PP11Q==9] = NA
eph$PP11Q[eph$PP11Q==0] = -999 # no corresponde pero no sé por qué (tal vez responde que solo trabaja 1 persona)
eph$PP11Q[eph$PP11Q==2] = 0
dummies = c(dummies, 'PP11Q')

unique(eph$PP11R)
eph$PP11R[eph$PP11R==2] = 0
dummies = c(dummies, 'PP11R')

unique(eph$PP11S)
eph$PP11S[eph$PP11S==2] = 0
dummies = c(dummies, 'PP11S')

table(eph$PP11O, eph$PP11T)
eph$PP11T[eph$PP11T==0] = -999
eph$PP11T[eph$PP11T==9] = NA
eph$PP11T[eph$PP11T==2] = 0
dummies = c(dummies, 'PP11T')

rm(var,vars)

### Ingresos ####
pond = c(pond, 'PONDII', 'PONDIIO', 'PONDIH')
deciles = c('DECIFR', 'IDECIFR', 'RDECIFR', 'GDECIFR', 'PDECIFR', 'ADECIFR',# total fliar
            'DECCFR', 'IDECCFR', 'RDECCFR', 'GDECCFR', 'PDECCFR', 'ADECCFR', # per cap fliar
            'DECOCUR', 'IDECOCUR', 'RDECOCUR', 'GDECOCUR', 'PDECOCUR', 'ADECOCUR', # ocup principal
            'DECINDR', 'IDECINDR', 'RDECINDR', 'GDECINDR', 'PDECINDR', 'ADECINDR') # total individual

# Eliminamos los deciles que corresponden al interior y a aglomerados pequeños
eph = eph %>% select(!c(IDECIFR , PDECIFR , IDECCFR , PDECCFR , IDECOCUR , PDECOCUR,  IDECINDR , PDECINDR))
deciles = setdiff(deciles,c('IDECIFR' , 'PDECIFR', 'IDECCFR' , 'PDECCFR' ,
                            'IDECOCUR', 'PDECOCUR',  'IDECINDR' , 'PDECINDR')) 


eph[deciles]
class(eph$DECIFR)
mean(is.na(eph[deciles]))
mean(is.na(apply(eph[deciles],2,as.numeric)))
eph[deciles] = apply(eph[deciles],2,as.numeric)
class(eph$DECIFR)
mean(eph[deciles] == 13)
mean(eph[deciles] == 12) # hay varios NAs en variables de deciles de ingreo


# NAs en INGRESO FLIAR
mean(eph$ITF == -9)
mean(is.na(eph$ITF))
table(eph$ITF, eph$DECIFR) # Los no respuesta de ingresos tienen 0 en ITF

# NAs en INGRESO PER CAP  FLIAR
mean(eph$IPCF == -9)
mean(is.na(eph$IPCF))
table(eph$IPCF, eph$DECCFR) # Los no respuesta de ingresos tienen 0 en IPCF

# NAs en ocup principal
mean(eph$P21 == -9) # acá si hay NA
table(eph$P21, eph$DECOCUR) # y están bien codificados, con 12 en los deciles

# NAs en ingreso total individual
mean(eph$P47T == -9) # acá si hay NA
table(eph$P47, eph$DECINDR) # y están bien codificados, con 12 en los deciles


## TRABAJAMOS CON INGRESO FAMILIAR: ASIGNAR BIEN LOS NO RESPUESTA
eph$ITF[eph$DECIFR == 12] = NA
eph$IPCF[eph$DECCFR == 12] = NA
mean(is.na(eph$ITF)) # muchos NAs, el 37%
mean(is.na(eph$IPCF)) # muchas NAs, el 37%

# dado que el ITF tiene tantos más NAs que el ingreso total indiv, a partir del cual se calcula,
# calculamos a mano ITF a ver si obtenemos la misma cantidad de NA

eph$P47T[eph$P47T==-9] = NA

fliar_manual = eph%>%
  group_by(CODUSU_NRO_HOGAR)%>%
  summarise(ITF_manual = sum(P47T),
            IPCF_manual = mean(P47T))

mean(is.na(fliar_manual$ITF_manual)) # se propaga pues, si no hay respuesta de un miembro, no hay respuesta para todo el hogar
mean(is.na(merge(eph,fliar_manual,by='CODUSU_NRO_HOGAR', all.x = T, all.y = F)$ITF_manual)) # apenas menos NAs

# lo dejamos tal cual está, pues da la misma cantidad de NAs prácticamente
# puede que las discrepancias tengan que ver con el uso de ponderadores para corregir por no respuesta
rm(fliar_manual)

num = c(num, 'ITF', 'IPCF')
eph[deciles][eph[deciles] == 12] = NA

# OCUPACION PRINCIPAL

# podemos eliminar PP06B porque esta incluida en P21:
unique(eph$PP06B)
mean(eph$PP06B[eph$PP06B>-1 & !(is.na(eph$PP06B))] == eph$P21[eph$PP06B>-1 & !(is.na(eph$PP06B))])
eph = eph%>%select(!PP06B)


# las pp08 no son inguales a p21, así que las dejamos
colnames(select(eph,contains('PP08')))
unique(eph$PP08D1)
mean(eph$PP08D1[eph$PP08D1>0 & !(is.na(eph$PP08D1))] +
       eph$PP08D4[eph$PP08D1>0 & !(is.na(eph$PP08D1))] == eph$P21[eph$PP08D1>0 & !(is.na(eph$PP08D1))]) # es muy parecida pero no s igual
pp08 = rowSums(select(eph,contains('PP08')), na.rm=T)
sum(is.na(pp08))
unique(pp08)
sum(pp08[pp08 > 0] == eph$p21[pp08 > 0])
rm(pp08)
  
colnames(select(eph,contains('PP08')))
num = c(num, 'P21', colnames(select(eph,contains('PP08'))))
encadenadas_activ = c(encadenadas_activ, colnames(select(eph,contains('PP08'))))

min(eph$P21)
eph$P21[eph$P21==-9] = NA

table(eph$ESTADO, eph$P21) # tendrá 0 ingresos si no stá ocupado

# OTRAS OCUPACIONES
min(eph$TOT_P12)
eph$TOT_P12[eph$TOT_P12==-9] = NA
num = c(num, 'TOT_P12')
table(eph$ESTADO, eph$TOT_P12)


# NO LABORALES
# todas terminan con _M, eliminando el monto total
nolab = select(eph,contains(c('_M','_AM')))
colnames(nolab)
nolab = nolab %>% select(!IX_MEN10)
nolab = colnames(nolab)

eph[nolab][eph[nolab]==-9] = NA
eph$T_VI[eph$T_VI==-9] = NA
a = rowSums(eph[nolab], na.rm=T)
b = eph$T_VI
eph[a[!is.na(b)]!=b[!is.na(b)], c(nolab,'T_VI')]
rm(a,b)
eph =  eph%>% select(!T_VI) # eliminamos T_VI por ser combilación lineal de los _M
num = c(num, nolab)

# TOTAL INDIVIDUAL
unique(eph$P47T)
mean(is.na(eph$P47T))
which(eph$P47T==-9) # ya los habíamos convertido a NA

mask = (rowSums(eph[nolab], na.rm=T) + eph$P21 + eph$TOT_P12)[!is.na(eph$P47T)] != eph$P47T[!is.na(eph$P47T)]
length(mask)
length(eph$P47T[!is.na(eph$P47T)])
problemas = eph[!is.na(eph$P47T),][mask,][c('P21', 'TOT_P12', 'P47T')]

problemas$nolab = rowSums(eph[nolab], na.rm=T)[!is.na(eph$P47T)][mask]
problemas
mean(mask) # son poquitas las observaciones con problemas, podemos eliminar P47T 
rm(problemas,mask)

eph =  eph%>% select(!P47T)

#puede que las discrepancias tengan que ver con el uso de ponderadores para corregir por no respuesta
rm(nolab)

------------------------------------------------------
# Chequeos y guardado --------------------------------
------------------------------------------------------
## Chequeamos que en los vectores de tipos de variables hemos incluido a todas
categ = unique(categ)
dummies = unique(dummies)
num = unique(num)

ncol(eph)
length(categ) + length(num) + length(dummies) + length(deciles) + length(pond)
colnames(eph)[! colnames(eph) %in% c(categ, num, dummies, deciles, pond)]
identificadores = c( "CODUSU_NRO_HOGAR","id","COMPONENTE" )
c(categ, num, dummies, deciles, pond)[! c(categ, num, dummies, deciles, pond) %in% colnames(eph)]

length(categ) + length(num) + length(dummies) # 174 variables
length(categ) + length(num) + length(dummies) + length(deciles) # 198 variables

## Eliminamos variables que no tengan variablilidad
colnames(eph)[apply(eph, 2, FUN=function(x) length(unique(x))<2)]
eph = eph %>% select(!V19_AM)
num = num[num!='V19_AM']

## Aglomerado la escribo como dummy 
unique(eph$AGLOMERADO)
eph$AGLOMERADO32 = 0
eph$AGLOMERADO32[eph$AGLOMERADO==32] = 1
eph = eph%>%select(!AGLOMERADO)
dummies = c(dummies, 'AGLOMERADO32')

## MISSINGS
missings = apply(eph, 2, FUN= function(x) mean(is.na(x)))
sort(missings, decreasing = T)
rm(missings)

# Variables que tienen más de un 1% de missings
# 37%   DECIFR       RDECIFR     GDECIFR      ADECIFR       DECCFR       RDECCFR    GDECCFR     ADECCFR   ITF IPCF       
# 17 % DECINDR       RDECINDR         GDECINDR         ADECINDR             
# 14% P21          DECOCUR         RDECOCUR       GDECOCUR       ADECOCUR        
# entre 1% y 10% IV4   PP08D1            PP07D             V2_M             CH14          TOT_P12   

## Guardo eph limpia ------------
save(eph, file = 'eph_limpia.RData')
save(pond,identificadores, file = 'pond_identificadores.RData')
save(categ, deciles, dummies, num, file = 'tipos_variables.RData')


