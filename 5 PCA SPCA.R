library(tidyverse)
library(readxl)
library(ggrepel)
library(gridExtra)
library(viridis)
library(rgl)
library(scatterplot3d) 
library(scales)
library(elasticnet)
library(PMA)
library(factoextra)
library(FactoMineR)


setwd('G:/Mi unidad/TESIS ECON/R')

# -------------------------------------
# Cargar bases y otros  ------------------------
# -------------------------------------
eph = readRDS('eph_scaled.rds')
eph_3T2019 = readRDS('eph_scaled_3T2019.rds')
variables_usadas <- read_excel("G:/Mi unidad/TESIS ECON/Sobre la EPH/variables usadas.xlsx", 
                               col_types = c("skip", "text", "text", "text", "text", "text", "skip","skip"))

sacar = colnames(eph)[!colnames(eph) %in% colnames(eph_3T2019)]
eph = eph[colnames(eph)!=sacar]
rm(sacar)

# escalamos
eph_scaled = scale(eph)
dim(eph_scaled)
# Matriz de correlación
matriz_cor = cor(eph_scaled)


# Scores de categorías del optimal scaling---------
catscores <- readRDS('catscores.rds')
catscores$caes
rownames(catscores$caes)[order(catscores$caes)]
catscores$PP03_ord
catscores$INTENSI_ord
catscores$RDECOCUR
catscores$V16
catscores$PP07J
catscores$ESTADO
catscores$materiales
catscores$CH03
catscores$PP07K
catscores$PPA
catscores$C99
catscores$independientes
catscores$PP05E
catscores$W_domestico
catscores$CAT
rownames(catscores$CAT)[order(catscores$CAT)]

# --------------------------------------
# DESCRIPTIVA --------------------------
# # --------------------------------------
# str(eph)
# summary(eph)[,1:10]
# 
# # Matriz de distancias para ver si hay clusters
# #gc()
# #memory.limit(1024*400)
# #fviz_dist(dist(eph), show_labels = FALSE) 
# 
# # Grafico matriz de correlación
# heatmap(matriz_cor)
# image(z=matriz_cor)


# -----------------------------------
# REDUZCO LA DIMENSION --------------
# -----------------------------------

#### --------- PCA ---------
set.seed(9)
pca = prcomp(eph, center = T, scale = T)

names(pca)
pca$center # media de la variable
pca$scale  # desvío estandar de las variables
pca$sdev # desvíos de los componentes principales
pca$var_explained <- pca$sdev^2/sum(pca$sdev^2) # varianza explicada por cada componente
pca$rotation[c('RDECOCUR','V16'),c('PC1','PC2')]

#### --------- SPCA ---------
# Centrando y normalizando manualmente con scale(eph)
# Parámetros siguiendo a Zhao et al 2006

# Lambda óptimo a mano
lambdas = c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5)
lambdas_pev = matrix(ncol = 15, nrow= 9)
lambdas_maxl = matrix(ncol = 15, nrow= 9)
lambdas_zerol = matrix(ncol = 15, nrow= 9)
set.seed(9)
for (i in 1:9){
        spca_fit = spca(matriz_cor, 15, rep(lambdas[i], 15), type="Gram",
                        sparse="penalty", use.corr=FALSE, lambda=1e-6,
                        max.iter=200, trace=TRUE, eps.conv=1e-3)
        lambdas_pev[i,] = spca_fit$pev
        lambdas_maxl[i,] = apply(spca_fit$loadings, 2, FUN = function(x) max(abs(x)))
        lambdas_zerol[i,] = apply(spca_fit$loadings, 2, FUN = function(x) sum(x==0))
        
}
# par(mfrow=c(3,3))
# for (cp in 1:3){
#       plot(lambdas, lambdas_pev[,cp], main = 'PEV', ylab = paste('PC',cp))
#       plot(lambdas, lambdas_maxl[,cp], main = 'max loading', ylab ='')
#       plot(lambdas, lambdas_zerol[,cp], main= 'zero loadings', ylab ='')
# }
par(mfrow=c(1,3))
for (cp in 1:3){
  plot(lambdas_zerol[,cp], lambdas_pev[,cp]*100, 
       ylim=c(0, max(lambdas_pev[,cp]*100)+3), xlim = c(0,129),
       main =  paste('CP',cp), 
       ylab = 'Porcentaje de varianza explicada por el componente (%)', 
       xlab = 'Cant. pesos iguales a cero',
       frame.plot=F)
  text(lambdas_zerol[,cp], lambdas_pev[,cp]*100, lambdas, pos=3)
}



par(mfrow=c(1,3))
for (cp in 1:3){
  plot(lambdas_zerol[,cp], lambdas_pev[,cp]*100, 
       ylim=c(0, max(lambdas_pev[,cp]*100)+3), xlim = c(0,129),
       main =  paste('PC',cp), 
       ylab = 'Percentage of variance explained (%)', 
       xlab = 'Number of zero loadings',
       frame.plot=F)
  text(lambdas_zerol[,cp], lambdas_pev[,cp]*100, lambdas, pos=3)
}

dev.off()


rm(lambdas, lambdas_maxl, lambdas_pev, lambdas_zerol)
par(mfrow=c(1,1))

# Con el lambda que mas o menos resuelve bien el trade off varianza - sparsity
set.seed(9)
spca_fit_lambda = spca(matriz_cor, 15, rep(0.1, 15), type="Gram",
                sparse="penalty", use.corr=FALSE, lambda=1e-6,
                max.iter=1000, trace=TRUE, eps.conv=1e-3)
# Otra opción es, en vez de indicar lambda, indicar número de loadings distintos a cero por componente
spca_fit = spca(matriz_cor, 15, rep(15:1), type="Gram",
               sparse="varnum", use.corr=FALSE, lambda=1e-6,
               max.iter=1000, trace=TRUE, eps.conv=1e-3)




#### --------- SPC ---------
set.seed(9)
cv.out <- SPC.cv(eph_scaled, sumabsvs = seq(1.2, sqrt(ncol(eph)), len = 30))
cv.out$bestsumabsv # 8.1
cv.out$bestsumabsv1se # 6.4
spc_fit <- SPC(eph_scaled, 
               sumabsv=cv.out$bestsumabsv1se, 
            #   sumabsv=6.4, 
               center = T, 
               K=15, v = cv.out$v.init,
               cnames = names(eph))

## Esparsitud óptima a mano

sumabsvs = c(1.4, 2.4, 3, 3.4, 4, 4.4,5, 5.4, 6, 6.4, 7.4)
sumabsv_pev = matrix(ncol = 15, nrow= 11)
sumabsv_zerol = matrix(ncol = 15, nrow= 11)
set.seed(11)
for (i in 1:11){
  spc_fit_esparso <- SPC(eph_scaled, sumabsv=sumabsvs[i], center = T, 
                              K=15,
                              cnames = names(eph))
  sumabsv_pev[i,] = spc_fit_esparso$prop.var.explained
  sumabsv_zerol[i,] = apply(spc_fit_esparso$v, 2, FUN = function(x) sum(x==0))
  
}

par(mfrow=c(1,3))
for (cp in 1:3){
  plot(sumabsv_zerol[,cp], sumabsv_pev[,cp]*100, 
       ylim=c(0, max(sumabsv_pev[,cp]*100)+3), xlim = c(0,129),
       main =  paste('CP',cp), 
       ylab = 'Porcentaje de varianza acumulada hasta ese componente (%)', 
       xlab = 'Cant. pesos iguales a cero',
       frame.plot=F)
  text(sumabsv_zerol[,cp], sumabsv_pev[,cp]*100, sumabsvs, pos=3)
}


par(mfrow=c(1,3))
for (cp in 1:3){
  plot(sumabsv_zerol[,cp], sumabsv_pev[,cp]*100, 
       ylim=c(0, max(sumabsv_pev[,cp]*100)+3), xlim = c(0,129),
       main =  paste('PC',cp), 
       ylab = 'Percentage of variance explained (%)', 
       xlab = 'Number of zero loadings',
       frame.plot=F)
  text(sumabsv_zerol[,cp], sumabsv_pev[,cp]*100, sumabsvs, pos=3)
}

dev.off()


set.seed(9)
spc_fit_esparso <- SPC(eph_scaled, sumabsv=3.4, center = T, 
               K=15, v = cv.out$v.init,
               cnames = names(eph))

# Ortogonalidad

set.seed(9)
cv.out <- SPC.cv(eph_scaled, sumabsvs = seq(1.2, sqrt(ncol(eph)), len = 30),
                 orth = TRUE)
cv.out$bestsumabsv  #8.1
cv.out$bestsumabsv1se #6.4
spc_fit_orth <- SPC(eph_scaled, sumabsv=cv.out$bestsumabsv1se, center = T, 
               K=15, v = cv.out$v.init, orth = T,
               cnames = names(eph))

## Esparsitud óptima a mano
sumabsvs = c(1.4, 2.4, 3, 3.4, 4, 4.4,5, 5.4, 6, 6.4, 7.4)
sumabsv_pev = matrix(ncol = 15, nrow= 11)
sumabsv_zerol = matrix(ncol = 15, nrow= 11)
set.seed(11)
for (i in 1:11){
  spc_fit_orth_esparso <- SPC(eph_scaled, sumabsv=sumabsvs[i], center = T, 
                              K=15, orth=T,
                              cnames = names(eph))
  sumabsv_pev[i,] = spc_fit_orth_esparso$prop.var.explained
  sumabsv_zerol[i,] = apply(spc_fit_orth_esparso$v, 2, FUN = function(x) sum(x==0))
  
}
par(mfrow=c(1,3))
for (cp in 1:3){
  plot(sumabsv_zerol[,cp], sumabsv_pev[,cp]*100, 
       ylim=c(0, max(sumabsv_pev[,cp]*100)+3), xlim = c(0,129),
       main =  paste('CP',cp), 
       ylab = 'Porcentaje de varianza acumulada hasta ese componente (%)', 
       xlab = 'Cant. pesos iguales a cero',
       frame.plot=F)
  text(sumabsv_zerol[,cp], sumabsv_pev[,cp]*100, sumabsvs, pos=3)
}


for (cp in 1:3){
  plot(sumabsv_zerol[,cp], sumabsv_pev[,cp]*100, 
       ylim=c(0, max(sumabsv_pev[,cp]*100)+3), xlim = c(0,129),
       main =  paste('PC',cp), 
       ylab = 'Percentage of variance explained (%)', 
       xlab = 'Number of zero loadings',
       frame.plot=F)
  text(sumabsv_zerol[,cp], sumabsv_pev[,cp]*100, sumabsvs, pos=3)
}





rm(sumabsvs, sumabsv_pev, sumabsv_zerol)
par(mfrow=c(1,1))

set.seed(9)
spc_fit_orth_esparso <- SPC(eph_scaled, sumabsv=3.4, center = T, 
                            K=15, orth=T,
                            cnames = names(eph))


max_esparsitud_ortog <- SPC(eph_scaled, sumabsv=3.4, center = T, 
                            orth = T,
                            K=25, cnames = names(eph))

sum(rowSums(max_esparsitud_ortog$v[,1:25])==0)
ceros = sum(max_esparsitud_ortog$v[,1]==0)
for (i in 2:25){ceros[i]=sum(rowSums(max_esparsitud_ortog$v[,1:i])==0)}
plot(max_esparsitud_ortog$prop.var.explained, 126-ceros, 
     main = 'SPC ortog. esparso', xlab='Proporción de varianza explicada',
     ylab='Cantidad de variables originales usadas')

plot(max_esparsitud_ortog$prop.var.explained, 126-ceros, 
     main = 'SPC orthog. sparse', xlab='Proportion of explained variance',
     ylab='Number of original variables used')


#### ---------  SAVE ---------
#save(pca, spca_fit, spca_fit_lambda,
#     spc_fit, spc_fit_esparso, 
#     spc_fit_orth, spc_fit_orth_esparso,
#     file='PCASPCASPC.RData')

#### --------- SPC POSITIVOS ---------
#set.seed(9)
#cv.out <- SPC.cv(eph_scaled, sumabsvs = seq(1.2, sqrt(ncol(eph)), len = 30), orth=T, vpos=T)
#cv.out$bestsumabsv1se #1.2
#pos_spc_fit_orth <- SPC(eph_scaled, sumabsv=cv.out$bestsumabsv1se, center = T,
#               K=15, orth = T, vpos=T,
#               cnames = names(eph))

sumabsvs = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2)
sumabsv_pev = matrix(ncol = 15, nrow= 7)
sumabsv_zerol = matrix(ncol = 15, nrow= 7)
set.seed(9)
for (i in 1:7){
  pos_spc_fit_orth_esparso <- SPC(eph_scaled, sumabsv=sumabsvs[i], center = T, 
                                  K=15, orth=T, vpos=T,
                                  cnames = names(eph))
  sumabsv_pev[i,] = pos_spc_fit_orth_esparso$prop.var.explained
  sumabsv_zerol[i,] = apply(pos_spc_fit_orth_esparso$v, 2, FUN = function(x) sum(x==0))
  
}
par(mfrow=c(1,3))
for (cp in 1:3){
  plot(sumabsv_zerol[,cp], sumabsv_pev[,cp], ylim = c(0,0.4),
       main =  paste('PC',cp), ylab = 'Varianza acumulada hasta ese componenete', xlab = 'Cant. zero loadings')
  text(sumabsv_zerol[,cp], sumabsv_pev[,cp], sumabsvs, pos=3)
}
rm(sumabsvs, sumabsv_pev, sumabsv_zerol)
par(mfrow=c(1,1))


set.seed(9)
pos_spc_fit_orth_esparso <- SPC(eph_scaled, sumabsv=3.2, center = T,
                                K=15, orth = T, vpos=T,
                                cnames = names(eph))
set.seed(9)
pos_spc_fit_orth <- SPC(eph_scaled, sumabsv=5.2, center = T,
                        K=15, orth=T, vpos = T,
                        cnames = names(eph))








#### ---------  LOAD ---------
load(file='PCASPCASPC.RData')



# ------------------------------------
# VISUALIZO RESULTADOS  --------------
# ------------------------------------

# (X) Data Frames de loadings y características de variables ---------------
nrow(pca$rotation)
nrow(spc_fit_orth$v)
nrow(spc_fit_orth_esparso$v)

loadings_pca = as_tibble(pca$rotation,rownames= 'variable')
loadings_spca = as_tibble(spca_fit_lambda$loadings)
loadings_spc_orth_esparso = as_tibble(spc_fit_orth_esparso$v)
loadings_spc_orth = as_tibble(spc_fit_orth$v)

colnames(loadings_pca)
colnames(loadings_spca)
colnames(loadings_spc_orth)
colnames(loadings_spc_orth_esparso)
colnames(loadings_spc_orth_esparso) = gsub("V","PC",colnames(loadings_spc_orth_esparso))
colnames(loadings_spc_orth) = gsub("V","PC",colnames(loadings_spc_orth))

loadings_spca$variable = spca_fit_lambda$vn
loadings_spc_orth_esparso$variable = spc_fit_orth_esparso$cnames
loadings_spc_orth$variable = spc_fit_orth$cnames

#colnames(eph_scaled) == loadings_spca$variable 
#colnames(eph_scaled) == loadings_spc_orth_esparso$variable 
#colnames(eph_scaled) == loadings_spc_orth$variable 

# # POSITIVOS
# loadings_pos_spc_orth = as_tibble(pos_spc_fit_orth$v)
# loadings_pos_spc_orth_esparso = as_tibble(pos_spc_fit_orth_esparso$v)
# colnames(loadings_pos_spc_orth_esparso) = gsub("V","PC",colnames(loadings_pos_spc_orth_esparso))
# colnames(loadings_pos_spc_orth) = gsub("V","PC",colnames(loadings_pos_spc_orth))
# loadings_pos_spc_orth_esparso$variable = pos_spc_fit_orth_esparso$cnames
# loadings_pos_spc_orth$variable = pos_spc_fit_orth$cnames


# CALCULO DE X (PC SCORES) -------------

#PCA
pairs(pca$x[,c(1,2,3)], main = 'PCA')
scatterplot3d(x = pca$x[,1], y = pca$x[,2],
              z = pca$x[,3], 
              xlab='PC1', ylab='PC2', zlab='PC3',
              main = 'PCA',
              highlight.3d = T,
              angle = 45)

# SPC orth 
dim(eph_scaled)
dim(spc_fit_orth$v)
spc_fit_orth$x = eph_scaled %*% spc_fit_orth$v
dim(spc_fit_orth$x)
plot(x = spc_fit_orth$x[,1], y= spc_fit_orth$x[,2],
     xlab = 'PC1', ylab='PC2', main='SPC ortog')
abline(h=0,v=0)

scatterplot3d(x = spc_fit_orth$x[,1], y = spc_fit_orth$x[,2],
              z = spc_fit_orth$x[,3], 
              xlab='PC1', ylab='PC2', zlab='PC3',
              main = 'SPC ortog',
              highlight.3d = T,
              angle = 120)
scatterplot3d(x = spc_fit_orth$x[,1], y = spc_fit_orth$x[,2],
              z = spc_fit_orth$x[,3], 
              xlab='PC1', ylab='PC2', zlab='PC3',
              main = 'SPC ortog',
              highlight.3d = T,
              angle = 45)
pairs(spc_fit_orth$x[,c(1,2,3)], main = 'SPC ortog.')

# SPC orth esparso
dim(eph_scaled)
dim(spc_fit_orth_esparso$v)
spc_fit_orth_esparso$x = eph_scaled %*% spc_fit_orth_esparso$v
plot(x = spc_fit_orth_esparso$x[,1], y= spc_fit_orth_esparso$x[,2],
     xlab = 'PC1', ylab='PC2', main='SPC ortog. esparso')
abline(h=0,v=0)
scatterplot3d(x = spc_fit_orth_esparso$x[,1], y = spc_fit_orth_esparso$x[,2],
              z = spc_fit_orth_esparso$x[,3], 
              xlab='PC1', ylab='PC2', zlab='PC3',
              main = 'SPC ortog esparso',
              highlight.3d = T,
              angle = 45)

pairs(spc_fit_orth_esparso$x[,c(1,2,3)], main = 'SPC ortog. esparso')

# SPCA lambda
spca_fit_lambda$x = eph_scaled %*% as.matrix(loadings_spca[,c('PC1','PC2')])
spca_fit_lambda$x = cbind(spca_fit_lambda$x,
                          lm(PC2 ~ PC1, data = as.data.frame(spca_fit_lambda$x))$residuals)
colnames(spca_fit_lambda$x)[3] = 'PC2.resid'


# CARACTERÍSTICAS VARIABLES --------------
names(variables_usadas)
variables_usadas$Grupo = factor(variables_usadas$Grupo)
variables_usadas$Nivel = factor(variables_usadas$Nivel)
variables_usadas$Bienestar = factor(variables_usadas$Bienestar)

loadings_pca =  merge(loadings_pca, variables_usadas[c('Nombre', 'Grupo','Nivel','Bienestar')],
                      by.x='variable',by.y='Nombre',all.x = T  )
loadings_spca =  merge(loadings_spca, variables_usadas[c('Nombre', 'Grupo','Nivel','Bienestar')],
                      by.x='variable',by.y='Nombre',all.x = T  )
loadings_spc_orth =  merge(loadings_spc_orth, variables_usadas[c('Nombre', 'Grupo','Nivel','Bienestar')],
                           by.x='variable',by.y='Nombre',all.x = T  )


loadings_spc_orth_esparso =  merge(loadings_spc_orth_esparso, variables_usadas[c('Nombre', 'Grupo','Nivel','Bienestar')],
                                   by.x='variable',by.y='Nombre',all.x = T  )

# loadings_spca$PC2.resid = lm(PC2 ~ PC1, data = loadings_spca)$residuals



# (A) Elección de modelo -----

## --- PCA PEV ----
plot(cumsum(pca$var_explained)*100,
     main = 'PCA - porcentaje de varianza explicada acumulando componentes',
     xlab= 'Cantidad de componentes principales',
     ylab = 'Porcentaje acumulado de varianza explicada (%)',
     ylim = c(0,100),
     pch=20) 

plot(pca$var_explained*100,
     main = 'PCA - porcentaje de varianza explicada por cada componente',
     xlab= 'Cantidad de componentes principales',
     ylab = 'Porcentaje de varianza explicada (%)',
     ylim = c(0,21),
     pch=20) 

plot(pca$var_explained*100,
     main = 'PCA: variance explained by each component',
     xlab= 'Principal component',
     ylab = 'Percentage of variance explained (%)',
     ylim = c(0,21),
     pch=20) 
dev.off()


pca$var_explained[1]/mean(pca$var_explained)
pca$var_explained[2]/mean(pca$var_explained)
pca$var_explained[5]/mean(pca$var_explained)
pca$var_explained[6]/mean(pca$var_explained)
pca$var_explained[1:6]
# eigenvalues
get_eigenvalue(pca)
sum(get_eigenvalue(pca)$eigenvalue>1) # 34 autovalores mayores a 1 ..............

## ---PCA PESOS ----

# CP1
loadings_pca$PC1neg = 1
loadings_pca$PC1neg[loadings_pca$PC1<0] = 2

par(mfrow=c(1,2))
plot(x=sort(loadings_pca$PC1, decreasing =T),
     xlab = 'Variable (ordenadas según peso en CP1)',
     ylab = 'Peso de la variable en el componente',
     main = 'PCA - Primer componente principal (CP1)')
abline(h=0)
plot(sort(abs(loadings_pca$PC1), decreasing =T),
     xlab = 'Variable (ordenadas según valor absoluto del peso en CP1)',
     ylab = 'Valor absoluto del peso de la variable en el componente',
     col = loadings_pca$PC1neg,
     main = 'PCA - Primer componente principal')
legend("topright", 
       legend=c("Peso positivo", 
                "Peso negativo"),
       y.intersp = 1,
       col=c("black","red"), cex=0.9,pch=1 )
abline(h=0)

dev.off()


par(mfrow=c(1,2))
plot(x=sort(loadings_pca$PC1, decreasing =T),
     xlab = 'Variable (ordered by PC1 loadings)',
     ylab = 'Loading of the variable in the component',
     main = 'PCA - First Principal Component (PC1)')
abline(h=0)
plot(sort(abs(loadings_pca$PC1), decreasing =T),
     xlab = 'Variable (ordered by PC1 absolute value of loadings)',
     ylab = 'Absolute value of the loading of the variable in the component',
     col = loadings_pca$PC1neg,
     main = 'PCA - First Principal Component (PC1)')
legend("topright", 
       legend=c("Positive loading", 
                "Negative loading"),
       y.intersp = 1,
       col=c("black","red"), cex=0.9,pch=1 )
abline(h=0)

dev.off()

loadings_pca[loadings_pca$PC1 %in% sort(loadings_pca$PC1)[1:6],c('variable','PC1')]
loadings_pca[loadings_pca$PC1 %in% sort(loadings_pca$PC1, decreasing=T)[1:6],c('variable','PC1')]
loadings_pca[loadings_pca$PC1 %in% sort(abs(loadings_pca$PC1), decreasing=T)[1:10],c('variable','PC1')]
par(mfrow=c(1,1))
plot(sort(abs(loadings_pca$PC1), decreasing =T),
     xlab = 'ADECOCUR DECOCUR GDECOCUR RDECOCUR ESTADO',
     ylab = '',
     main = 'PCA - Primer componente principal')

#CP2

loadings_pca$PC2neg = 1
loadings_pca$PC2neg[loadings_pca$PC2<0] = 2


par(mfrow=c(1,2))
plot(x=sort(loadings_pca$PC2, decreasing =T),
     xlab = 'Variable (ordenadas según peso en CP2)',
     ylab = 'Peso de la variable en el componente',
     main = 'PCA - Segundo componente principal (CP2)')
abline(h=0)
plot(sort(abs(loadings_pca$PC2), decreasing =T),
     xlab = 'Variable (ordenadas según valor absoluto del peso en CP2)',
     ylab = 'Valor absoluto del peso de la variable en el componente',
     col = loadings_pca$PC2neg,
     main = 'PCA - Primer componente principal')
legend("topright", 
       legend=c("Peso positivo", 
                "Peso negativo"),
       y.intersp = 1,
       col=c("black","red"), cex=0.9,pch=1 )
abline(h=0)

dev.off()

par(mfrow=c(1,2))
plot(x=sort(loadings_pca$PC2, decreasing =T),
     xlab = 'Variable (ordered by PC2 loadings)',
     ylab = 'Loading of the variable in the component',
     main = 'PCA - Second Principal Component (PC2)')
abline(h=0)
plot(sort(abs(loadings_pca$PC2), decreasing =T),
     xlab = 'Variable (ordered by PC2 absolute value of loadings)',
     ylab = 'Absolute value of the loading of the variable in the component',
     col = loadings_pca$PC2neg,
     main = 'PCA - Second Principal Component (PC2)')
legend("topright", 
       legend=c("Positive loading", 
                "Negative loading"),
       y.intersp = 1,
       col=c("black","red"), cex=0.9,pch=1 )
abline(h=0)

dev.off()

loadings_pca[loadings_pca$PC2 %in% sort(loadings_pca$PC2)[1:6],c('variable','PC2')]
loadings_pca[loadings_pca$PC2 %in% sort(loadings_pca$PC2, decreasing=T)[1:6],c('variable','PC2')]
loadings_pca[loadings_pca$PC2 %in% sort(abs(loadings_pca$PC2), decreasing=T)[1:6],c('variable','PC2')]
par(mfrow=c(1,1))
plot(sort(abs(loadings_pca$PC1), decreasing =T),
     xlab = 'DECCFR  RDECCFR   GDECCFR  ADECCFR  IPCF ',
     ylab = '',
     main = 'PCA - Primer componente principal')


## --- PEV y loadings == 0 15 primeros componentes ----

# PEV
#par(mfrow=c(1,3))
par(mfrow=c(1,1))
plot(cumsum(pca$var_explained)[1:15]*100,
     main = 'Varianza explicada acumulanda en los primeros 15 CPs',
     xlab= 'Cantidad de componentes principales (CPs)',
     ylab = 'Porcentaje acumulado de varianza explicada (%)',
     ylim = c(0,100),
     type = 'b',
     pch=16)
lines(cumsum(spca_fit$pev)*100, type = 'b',col = 'blue'
      ,      pch=16)
lines(cumsum(spca_fit_lambda$pev)*100, type = 'b',col = 'dodgerblue',
      pch=16)

lines(spc_fit$prop.var.explained*100, type = 'b',col = 'violet',
      pch=16)
lines(spc_fit_orth$prop.var.explained*100, type = 'b',col = 'deeppink',
      pch=16)

  lines(spc_fit_esparso$prop.var.explained*100, type = 'b',col = 'yellowgreen',
      pch=16)
lines(spc_fit_orth_esparso$prop.var.explained*100, type = 'b',col = 'darkgreen',
      pch=16)
#lines(pos_spc_fit_orth$prop.var.explained*100, type = 'b', pch=20, col='gray44')
#lines(pos_spc_fit_orth_esparso$prop.var.explained*100, type = 'b', pch=20, col = 'grey')

abline(h=20,  col='grey')
#abline(h=25, col='grey')
abline(h=30,  col='grey')
abline(h=40,  col='grey')
#abline(h=0.5,  col='grey')
legend("topleft", 
       legend=c("PCA", 
                "SPCA varnum", "SPCA lambda", 
                "SPC", "SPC ortog.",
                "SPC esparso", "SPC ortog. esparso"#,
               # "SPC ortog. POSITIVO", "SPC ortog. - esparso POSITIVO"
                ),
       y.intersp = 1,
       col=c("black",
             "blue","dodgerblue",
             "violet", "deeppink",  
             "yellowgreen","darkgreen",
             "gray44", "grey"), cex=0.9,pch=16 )


dev.off()


# PEV
#par(mfrow=c(1,3))
par(mfrow=c(1,1))
plot(cumsum(pca$var_explained)[1:15]*100,
     main = 'Cumulative explained variance, first 15 PCs',
     xlab= 'Principal Component',
     ylab = 'Percentage of variance explained, cumulative (%)',
     ylim = c(0,100),
     type = 'b',
     pch=16)
lines(cumsum(spca_fit$pev)*100, type = 'b',col = 'blue',pch=16)
lines(cumsum(spca_fit_lambda$pev)*100, type = 'b',col = 'dodgerblue', pch=16)
lines(spc_fit$prop.var.explained*100, type = 'b',col = 'violet',pch=16)
lines(spc_fit_orth$prop.var.explained*100, type = 'b',col = 'deeppink', pch=16)
lines(spc_fit_esparso$prop.var.explained*100, type = 'b',col = 'yellowgreen',pch=16)
lines(spc_fit_orth_esparso$prop.var.explained*100, type = 'b',col = 'darkgreen',pch=16)
#lines(pos_spc_fit_orth$prop.var.explained*100, type = 'b', pch=20, col='gray44')
#lines(pos_spc_fit_orth_esparso$prop.var.explained*100, type = 'b', pch=20, col = 'grey')

abline(h=20,  col='grey')
#abline(h=25, col='grey')
abline(h=30,  col='grey')
abline(h=40,  col='grey')
#abline(h=0.5,  col='grey')
legend("topleft", 
       legend=c("PCA", 
                "SPCA varnum", "SPCA lambda", 
                "SPC", "SPC orthog.",
                "SPC sparse", "SPC orthog. sparse"#,
                # "SPC ortog. POSITIVO", "SPC ortog. - esparso POSITIVO"
       ),
       y.intersp = 1,
       col=c("black",
             "blue","dodgerblue",
             "violet", "deeppink",  
             "yellowgreen","darkgreen",
             "gray44", "grey"), cex=0.9,pch=16 )
dev.off()

## Loadings ==  0 

# Gráfico de cantidad de loadings cero en cada componente principal -----
# par(mar=c(5.1, 4.1, 4.1, 10.1), xpd=TRUE)
plot(apply(pca$rotation[,1:15], 2, FUN = function(x) sum(x==0)),
     main = 'Cantidad de variables con peso cero en los primeros 15 CPs',
     xlab= 'Componente principal (CP)',
     ylab = 'Cant. variables con peso cero',
     ylim = c(0,ncol(eph)),
     type = 'b', pch=16)
lines(apply(spca_fit$loadings, 2, FUN = function(x) sum(x==0)), type = 'b',col = 'blue', pch=16)
lines(apply(spca_fit_lambda$loadings, 2, FUN = function(x) sum(x==0)), type = 'b',col = 'dodgerblue', pch=16)
lines(apply(spc_fit$v, 2, FUN = function(x) sum(x==0)), type = 'b',col = 'violet', pch=16)
lines(apply(spc_fit_esparso$v, 2, FUN = function(x) sum(x==0)), type = 'b', col = 'yellowgreen' , pch=16)
lines(apply(spc_fit_orth$v, 2, FUN = function(x) sum(x==0)), type = 'b', col = 'deeppink', pch=16 )
lines(apply(spc_fit_orth_esparso$v, 2, FUN = function(x) sum(x==0)), type = 'b', col = 'darkgreen' , pch=16)
#lines(apply(pos_spc_fit_orth$v, 2, FUN = function(x) sum(x==0)), type = 'b', col = 'gray44', pch=20 )
#lines(apply(pos_spc_fit_orth_esparso$v, 2, FUN = function(x) sum(x==0)), type = 'b', col = 'grey', pch=20 )

# legend("topright", inset=c(-0.5,0),
#        legend=c("PCA", "SPCA - varnum", "SPCA - lambda", 
#                 "SPC", "SPC - esparso",
#                 "SPC ortog.", "SPC ortog.- esparso"),
#        y.intersp = 1,
#        col=c("black","blue","red","violet","green", "deeppink", "darkgreen"), cex=0.8, lty = 1)
# abline(v=2,lty=2,col='grey')
# abline(v=8,lty=2,col='grey')


dev.off()
plot(apply(pca$rotation[,1:15], 2, FUN = function(x) sum(x==0)),
     main = 'Number of zero loadings, first 15 PCs',
     xlab= 'Principal Component',
     ylab = 'Number of variables with zero loadings',
     ylim = c(0,ncol(eph)),
     type = 'b', pch=16)
lines(apply(spca_fit$loadings, 2, FUN = function(x) sum(x==0)), type = 'b',col = 'blue', pch=16)
lines(apply(spca_fit_lambda$loadings, 2, FUN = function(x) sum(x==0)), type = 'b',col = 'dodgerblue', pch=16)
lines(apply(spc_fit$v, 2, FUN = function(x) sum(x==0)), type = 'b',col = 'violet', pch=16)
lines(apply(spc_fit_esparso$v, 2, FUN = function(x) sum(x==0)), type = 'b', col = 'yellowgreen' , pch=16)
lines(apply(spc_fit_orth$v, 2, FUN = function(x) sum(x==0)), type = 'b', col = 'deeppink', pch=16 )
lines(apply(spc_fit_orth_esparso$v, 2, FUN = function(x) sum(x==0)), type = 'b', col = 'darkgreen' , pch=16)
dev.off()


## ESPARSITUD VERSUS VARIANZA
#par(mar=c(5, 4, 4, 2) + 0.1, xpd=FALSE)
plot(apply(pca$rotation[,1:4], 2, FUN = function(x) sum(x==0)),
     cumsum(pca$var_explained)[1:4]*100,
     main = 'Esparsitud versus varianza - primeros 4 CPs ',
     xlab= 'Cant. de variables con peso cero en el CP',
     ylab = 'Porcentaje acumulado de varianza explicada hasta el CP',
     ylim = c(0,40),
     xlim = c(0, ncol(eph)),
     type = 'b', pch = 16)
lines(apply(spca_fit$loadings[,1:4], 2, FUN = function(x) sum(x==0)),
      cumsum(spca_fit$pev)[1:4]*100, type = 'b',col = 'blue', pch = 16)
lines(apply(spca_fit_lambda$loadings[,1:4], 2, FUN = function(x) sum(x==0)), 
      cumsum(spca_fit_lambda$pev)[1:4]*100, type = 'b',col = 'dodgerblue', pch = 16)
lines(apply(spc_fit$v[,1:4], 2, FUN = function(x) sum(x==0)), 
      spc_fit$prop.var.explained[1:4]*100, type = 'b',col = 'violet', pch = 16)
lines(apply(spc_fit_esparso$v[,1:4], 2, FUN = function(x) sum(x==0)), 
      spc_fit_esparso$prop.var.explained[1:4]*100, type = 'b', col = 'yellowgreen' , pch = 16)
lines(apply(spc_fit_orth$v[,1:4]*100, 2, FUN = function(x) sum(x==0)), 
       spc_fit_orth$prop.var.explained[1:4]*100, type = 'b', col = 'deeppink', pch = 16 )
lines(apply(spc_fit_esparso$v[,1:4], 2, FUN = function(x) sum(x==0)), 
       spc_fit_orth_esparso$prop.var.explained[1:4]*100, type = 'b', col = 'darkgreen', pch = 16 )
# legend("topright", 
#        legend=c("PCA", "SPCA - varnum", "SPCA - lambda", 
#                 "SPC", "SPC - esparso",
#                 "SPC ortog.", "SPC ortog.- esparso"),
#        y.intersp = 1,
#        col=c("black","blue","red","violet","green", "deeppink", "darkgreen"), cex=0.8, lty = 1)


plot(apply(pca$rotation[,1:4], 2, FUN = function(x) sum(x==0)),
     cumsum(pca$var_explained)[1:4]*100,
     main = 'Sparsity versus Variance Explained - first 4 PCs ',
     xlab= 'Number of variables with zero loadings in the PC',
     ylab = 'Cumulative percentage of variance explained',
     ylim = c(0,40),
     xlim = c(0, ncol(eph)),
     type = 'b', pch = 16)

# CUANTAS VARIABLES TIENEN LOADING CERO EN LOS PRIMEROS K COMPONENTES PARA LA TABLA -----
sum(spc_fit_orth$v[,1]==0)
sum(rowSums(spc_fit_orth$v[,1:2])==0)
sum(rowSums(spc_fit_orth$v[,1:3])==0)
sum(spca_fit_lambda$loadings[,1]==0)
spca_fit_lambda$pev
sum(rowSums(spca_fit_lambda$loadings[,1:2])==0)
sum(rowSums(spca_fit_lambda$loadings[,1:4])==0)
sum(rowSums(spca_fit_lambda$loadings[,1:6])==0)
sum(rowSums(spca_fit_lambda$loadings[,1:12])==0)
sum(rowSums(spca_fit$loadings[,1:10])==0)
sum(spc_fit_orth_esparso$v[,1]==0)
spc_fit_esparso$prop.var.explained
sum(rowSums(spc_fit_orth_esparso$v[,1:2])==0)
sum(rowSums(spc_fit_orth_esparso$v[,1:3])==0)
sum(rowSums(spc_fit_orth_esparso$v[,1:4])==0)
sum(rowSums(spc_fit_orth_esparso$v[,1:6])==0)
sum(rowSums(spc_fit_orth_esparso$v[,1:12])==0)

## --- Lollipop GENERAL----
comparo_grupo =  merge(loadings_pca[c('variable','PC1', 'PC2', 'PC3','PC4', 'PC5')]%>%
                         pivot_longer(cols=contains('PC'),names_to = 'PC',values_to = 'loadings'),
                       loadings_spc_orth[c('variable','PC1', 'PC2', 'PC3','PC4', 'PC5')]%>%
                         pivot_longer(cols=contains('PC'),names_to = 'PC',values_to = 'loadings'),
                       by='variable', all=T, suffixes = c(".PCA",".SPC")) 
comparo_grupo = comparo_grupo[comparo_grupo$PC.PCA==comparo_grupo$PC.SPC,]
comparo_grupo = comparo_grupo%>%select(!'PC.SPC')
names(comparo_grupo)[names(comparo_grupo) == 'PC.PCA'] = 'PC'

comparo_grupo =  merge(comparo_grupo,
                       loadings_spc_orth_esparso[c('variable','PC1', 'PC2', 'PC3','PC4','PC5')]%>%
                         pivot_longer(cols=contains('PC'),names_to = 'PC',values_to = 'loadings.SPC_e'),
                       by='variable', all=T, suffixes = c("",".SPC_e"))
names(comparo_grupo)
comparo_grupo = comparo_grupo[comparo_grupo$PC==comparo_grupo$PC.SPC_e,]
comparo_grupo = comparo_grupo%>%select(!'PC.SPC_e')
names(comparo_grupo)

comparo_grupo =  merge(comparo_grupo,
                       loadings_spca[c('variable','PC1', 'PC2', 'PC3','PC4','PC5')]%>%
                         pivot_longer(cols=contains('PC'),names_to = 'PC',values_to = 'loadings.SPCA'),
                       by='variable', all=T, suffixes = c("",".SPCA"))
names(comparo_grupo)
comparo_grupo = comparo_grupo[comparo_grupo$PC==comparo_grupo$PC.SPCA,]
comparo_grupo = comparo_grupo%>%select(!'PC.SPCA')
names(comparo_grupo)

# GRAFICO

comparo_grupo %>%
  filter(PC=='PC1')%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.PCA,
                   col= '1. PCA'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.PCA, 
                 col='1. PCA'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPC,
                   col= '2. SPC ortog.'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPC, 
                 col='2. SPC ortog.'), alpha=0.7)+
  # geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
  #                  xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPCA,
  #                  col= '3. SPCA lambda'), alpha=0.7)+
  # geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPCA, 
  #                col='3. SPCA lambda'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPC_e,
                   col= '4. SPC ortog. esparso'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPC_e, 
                 col='4. SPC ortog. esparso'), alpha=0.7)+
  labs(x='Variable 
       (ordenadas por valor absoluto de pesos en CP1)',
       y = 'Peso',
       title='Primer componente principal')+
  scale_x_discrete(breaks=NULL) +
  scale_color_viridis(discrete=T)+
  ylim(c(-0.4,0.4))+
  # coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
ggsave(device ='png',filename='lollipopPC1.png', width = 5, height = 5)


comparo_grupo %>%
  filter(PC=='PC1')%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.PCA,
                   col= '1. PCA'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.PCA, 
                 col='1. PCA'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPC,
                   col= '2. SPC orthog.'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPC, 
                 col='2. SPC orthog.'), alpha=0.7)+
  # geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
  #                  xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPCA,
  #                  col= '3. SPCA lambda'), alpha=0.7)+
  # geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPCA, 
  #                col='3. SPCA lambda'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPC_e,
                   col= '4. SPC orthog. sparse'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPC_e, 
                 col='4. SPC orthog. sparse'), alpha=0.7)+
  labs(x='Variable 
       (ordered by absolute value of loadings in PC1)',
       y = 'Loading',
       title='First Principal Component (PC1)')+
  scale_x_discrete(breaks=NULL) +
  scale_color_viridis(discrete=T)+
  ylim(c(-0.4,0.4))+
  # coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
ggsave(device ='png',filename='lollipopPC1-eng.png', width = 5, height = 5)





comparo_grupo %>%
  filter(PC=='PC1')%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.PCA),
                   col= '1. PCA'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.PCA), 
                 col='1. PCA'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.SPC),
                   col= '2. SPC ortog.'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.SPC), 
                 col='2. SPC ortog.'), alpha=0.7)+
  # geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
  #                  xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.SPCA),
  #                  col= '3. SPCA lambda'), alpha=0.7)+
  # geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.SPCA), 
  #                col='3. SPCA lambda'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.SPC_e),
                   col= '4. SPC ortog. esparso'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.SPC_e), 
                 col='4. SPC ortog. esparso'), alpha=0.7)+
  labs(x='Variable 
       (ordenadas por valor absoluto de pesos en CP1)',
       y= 'Valor absoluto del peso',
       title='Primer componente principal')+
  scale_x_discrete(breaks=NULL) +
  scale_color_viridis(discrete=T)+
  ylim(c(-0.4,0.4))+
  # coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
ggsave(device ='png',filename='lollipopPC1abs.png', width = 5, height = 5)



comparo_grupo %>%
  filter(PC=='PC1')%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.PCA),
                   col= '1. PCA'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.PCA), 
                 col='1. PCA'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.SPC),
                   col= '2. SPC orthog.'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.SPC), 
                 col='2. SPC orthog.'), alpha=0.7)+
  # geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
  #                  xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.SPCA),
  #                  col= '3. SPCA lambda'), alpha=0.7)+
  # geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.SPCA), 
  #                col='3. SPCA lambda'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.SPC_e),
                   col= '4. SPC orthog. sparse'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.SPC_e), 
                 col='4. SPC orthog. sparse'), alpha=0.7)+
  labs(x='Variable 
       (ordered by absolute value of loadings in PC1)',
       y= 'Absolute value of loading',
       title='First Principal Component (PC1)')+
  scale_x_discrete(breaks=NULL) +
  scale_color_viridis(discrete=T)+
  ylim(c(-0.4,0.4))+
  # coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
ggsave(device ='png',filename='lollipopPC1abs-eng.png', width = 5, height = 5)



comparo_grupo %>%
  filter(PC=='PC2')%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.PCA,
                   col= '1. PCA'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.PCA, 
                 col='1. PCA'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPC,
                   col= '2. SPC ortog.'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPC, 
                 col='2. SPC ortog.'), alpha=0.7)+
  # geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
  #                  xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPCA,
  #                  col= '3. SPCA lambda'), alpha=0.7)+
  # geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPCA, 
  #                col='3. SPCA lambda'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPC_e,
                   col= '4. SPC ortog. esparso'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPC_e, 
                 col='4. SPC ortog. esparso'), alpha=0.7)+
  labs(x='Variable 
       (ordenadas por valor absoluto de pesos en CP2)',
       y = 'Peso',
       title='Segundo componente principal')+
  scale_x_discrete(breaks=NULL) +
  scale_color_viridis(discrete=T)+
  ylim(c(-0.4,0.4))+
  # coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
ggsave(device ='png',filename='lollipopPC2.png', width = 5, height = 5)




comparo_grupo %>%
  filter(PC=='PC2')%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.PCA,
                   col= '1. PCA'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.PCA, 
                 col='1. PCA'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPC,
                   col= '2. SPC orthog.'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPC, 
                 col='2. SPC orthog.'), alpha=0.7)+
  # geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
  #                  xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPCA,
  #                  col= '3. SPCA lambda'), alpha=0.7)+
  # geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPCA, 
  #                col='3. SPCA lambda'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPC_e,
                   col= '4. SPC orthog. sparse'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPC_e, 
                 col='4. SPC orthog. sparse'), alpha=0.7)+
  labs(x='Variable 
       (ordered by absolute value of loadings in PC2)',
       y = 'Loading',
       title='Second Principal Component (PC2)')+
  scale_x_discrete(breaks=NULL) +
  scale_color_viridis(discrete=T)+
  ylim(c(-0.4,0.4))+
  # coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
ggsave(device ='png',filename='lollipopPC2-eng.png', width = 5, height = 5)


comparo_grupo %>%
  filter(PC=='PC2')%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.PCA),
                   col= '1. PCA'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.PCA), 
                 col='1. PCA'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.SPC),
                   col= '2. SPC ortog.'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.SPC), 
                 col='2. SPC ortog.'), alpha=0.7)+
  # geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
  #                  xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.SPCA),
  #                  col= '3. SPCA lambda'), alpha=0.7)+
  # geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.SPCA), 
  #                col='3. SPCA lambda'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.SPC_e),
                   col= '4. SPC ortog. esparso'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.SPC_e), 
                 col='4. SPC ortog. esparso'), alpha=0.7)+
  labs(x='Variable 
       (ordenadas por valor absoluto de pesos en CP2)',
       y= 'Valor absoluto del peso',
       title='Segundo componente principal')+
  scale_x_discrete(breaks=NULL) +
  scale_color_viridis(discrete=T)+
  ylim(c(-0.4,0.4))+
  # coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
ggsave(device ='png',filename='lollipopPC2abs.png', width = 5, height = 5)



comparo_grupo %>%
  filter(PC=='PC3')%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.PCA,
                   col= '1. PCA'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.PCA, 
                 col='1. PCA'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPC,
                   col= '2. SPC ortog.'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPC, 
                 col='2. SPC ortog.'), alpha=0.7)+
  # geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
  #                  xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPCA,
  #                  col= '3. SPCA lambda'), alpha=0.7)+
  # geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPCA, 
  #                col='3. SPCA lambda'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPC_e,
                   col= '4. SPC ortog. esparso'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPC_e, 
                 col='4. SPC ortog. esparso'), alpha=0.7)+
  labs(x='Variable 
       (ordenadas por valor absoluto de pesos en CP3)',
       y = 'Peso',
       title='Tercer componente principal')+
  scale_x_discrete(breaks=NULL) +
  scale_color_viridis(discrete=T)+
  ylim(c(-0.4,0.4))+
  # coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()

# (XXXX MISMA DIREC) Data Frames de loadings y características de variables ---------------
nrow(pca$rotation)
nrow(spc_fit_orth$v)
nrow(spc_fit_orth_esparso$v)

loadings_pca = as_tibble(pca$rotation,rownames= 'variable')
loadings_spca = as_tibble(spca_fit_lambda$loadings)
loadings_spc_orth_esparso = as_tibble(spc_fit_orth_esparso$v)
loadings_spc_orth = as_tibble(spc_fit_orth$v)

colnames(loadings_pca)
colnames(loadings_spca)
colnames(loadings_spc_orth)
colnames(loadings_spc_orth_esparso)
colnames(loadings_spc_orth_esparso) = gsub("V","PC",colnames(loadings_spc_orth_esparso))
colnames(loadings_spc_orth) = gsub("V","PC",colnames(loadings_spc_orth))

# Invierto el sentido

loadings_spca$PC1 = - loadings_spca$PC1
loadings_spc_orth_esparso$PC1 = - loadings_spc_orth_esparso$PC1
loadings_spc_orth$PC1 = - loadings_spc_orth$PC1

loadings_pca$PC3 = - loadings_pca$PC3



loadings_spca$variable = spca_fit_lambda$vn
loadings_spc_orth_esparso$variable = spc_fit_orth_esparso$cnames
loadings_spc_orth$variable = spc_fit_orth$cnames




# CALCULO DE X (PC SCORES) -------------
# PCA
pca$x[,3] = - pca$x[,3] 

# SPC orth 
dim(eph_scaled)
dim(spc_fit_orth$v)
spc_fit_orth$x = eph_scaled %*% as.matrix(loadings_spc_orth[c('PC1','PC2','PC3')])
dim(spc_fit_orth$x)
plot(x = spc_fit_orth$x[,1], y= spc_fit_orth$x[,2],
     xlab = 'PC1', ylab='PC2', main='SPC ortog')
abline(h=0,v=0)

# SPC orth esparso
dim(eph_scaled)
dim(spc_fit_orth_esparso$v)
spc_fit_orth_esparso$x = eph_scaled %*%as.matrix(loadings_spc_orth_esparso[c('PC1','PC2','PC3')])
plot(x = spc_fit_orth_esparso$x[,1], y= spc_fit_orth_esparso$x[,2],
     xlab = 'PC1', ylab='PC2', main='SPC ortog. esparso')

# CARACTERÍSTICAS VARIABLES --------------
names(variables_usadas)
variables_usadas$Grupo = factor(variables_usadas$Grupo)
variables_usadas$Nivel = factor(variables_usadas$Nivel)
variables_usadas$Bienestar = factor(variables_usadas$Bienestar)

loadings_pca =  merge(loadings_pca, variables_usadas[c('Nombre', 'Grupo','Nivel','Bienestar')],
                      by.x='variable',by.y='Nombre',all.x = T  )
loadings_spca =  merge(loadings_spca, variables_usadas[c('Nombre', 'Grupo','Nivel','Bienestar')],
                       by.x='variable',by.y='Nombre',all.x = T  )
loadings_spc_orth =  merge(loadings_spc_orth, variables_usadas[c('Nombre', 'Grupo','Nivel','Bienestar')],
                           by.x='variable',by.y='Nombre',all.x = T  )


loadings_spc_orth_esparso =  merge(loadings_spc_orth_esparso, variables_usadas[c('Nombre', 'Grupo','Nivel','Bienestar')],
                                   by.x='variable',by.y='Nombre',all.x = T  )

# loadings_spca$PC2.resid = lm(PC2 ~ PC1, data = loadings_spca)$residuals






# (B) Dimensiones del bienetar ----
## --- Barras por grupo ----

# CP1 en PCA versus SPC

### Color : ordenamiento en bienestar ----
# loadings_pca %>% 
#   ggplot()+
#   geom_col(aes(x=reorder(variable,PC1),y=PC1,
#                 fill = Bienestar, color = NULL),
#            size=0, alpha=0.5)+
#   ylim(c(-0.5, 0.5))+
#   labs(x='', y='Pesos en el CP1', title = 'PCA')+
#   coord_flip()+
#   scale_color_viridis(discrete=T, direction=-1)+
#   scale_fill_viridis(discrete=T, direction=-1)+
#   theme_classic()
# ggsave(device ='png',filename='loadings_1er_PCA.png', width = 6, height = 12)


loadings_spc_orth %>% 
  filter(abs(PC1)>0)%>%
  ggplot()+
  geom_col(aes(x=reorder(variable,PC1),y=PC1,
               fill = Bienestar, color = NULL),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Pesos en el CP1', title = 'SPC ortog.')+
  coord_flip()+
  scale_color_viridis(discrete=T, direction=-1)+
  scale_fill_viridis(discrete=T, direction=-1)+
  theme_classic()
ggsave(device ='png',filename='loadings_1er_SPC.png', width = 6, height = 9)

loadings_spc_orth_esparso %>% 
  filter(abs(PC1)>0)%>%
  ggplot()+
  geom_col(aes(x=reorder(variable,PC1),y=PC1,
               fill = Bienestar, color = NULL),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Pesos en el CP1', title = 'SPC ortog. esparso')+
  coord_flip()+
  scale_color_viridis(discrete=T, direction=-1)+
  scale_fill_viridis(discrete=T, direction=-1)+
  theme_classic()
ggsave(device ='png',filename='loadings_1er_SPCe.png', width = 5.5, height = 3)

loadings_spc_orth_esparso %>% 
  filter(abs(PC2)>0)%>%
  ggplot()+
  geom_col(aes(x=reorder(variable,PC2),y=PC2,
               fill = Bienestar, color = NULL),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Pesos en el CP2', title = 'SPC ortog. esparso')+
  coord_flip()+
  scale_color_viridis(discrete=T, direction=1)+
  scale_fill_viridis(discrete=T, direction=1)+
  theme_classic()
ggsave(device ='png',filename='loadings_2do_SPCe.png', width = 6, height = 3)

loadings_spc_orth_esparso %>% 
  filter(abs(PC3)>0)%>%
  ggplot()+
  geom_col(aes(x=reorder(variable,PC3),y=PC3,
               fill = Bienestar, color = NULL),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Pesos en el CP3', title = 'SPC ortog. esparso')+
  coord_flip()+
  scale_color_viridis(discrete=T, direction=-1)+
  scale_fill_viridis(discrete=T, direction=-1)+
  theme_classic()
ggsave(device ='png',filename='loadings_3er_SPCe.png', width = 6, height = 3)

### Color: Grupo ----
loadings_pca %>% 
  ggplot()+
  geom_col(aes(x=reorder(variable,PC1),y=PC1,
               fill = Grupo, color = Grupo),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Loadings', title = 'PCA - CP1')+
  coord_flip()+
  theme_classic()

loadings_spc_orth %>% 
  filter(abs(PC1)>0)%>%
  ggplot()+
  geom_col(aes(x=reorder(variable,PC1),y=PC1,
               fill = Grupo, color = Grupo),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Loadings', title = 'SPC ortog. esparso - CP1 ')+
  coord_flip()+
  theme_classic()


# Mas CPs en esparso

## Color: grupo
loadings_spc_orth_esparso %>% 
  filter(abs(PC1)>0)%>%
  ggplot()+
  geom_col(aes(x=reorder(variable,PC1),y=PC1,
               fill = Grupo, color = Grupo),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Loadings', title = 'SPC ortog. esparso - CP1 ')+
  coord_flip()+
  theme_classic()

loadings_spc_orth_esparso %>% 
  filter(abs(PC2)>0)%>%
  ggplot()+
  geom_col(aes(x=reorder(variable,PC2),y=PC2,
               fill = Grupo, color = Grupo),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Loadings', title = 'SPC ortog. esparso - CP2')+
  coord_flip()+
  theme_classic()

loadings_spc_orth_esparso %>% 
  filter(abs(PC3)>0)%>%
  ggplot()+
  geom_col(aes(x=reorder(variable,PC3),y=PC3,
               fill = Grupo, color = Grupo),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Loadings', title = 'SPC ortog. esparso - CP3')+
  coord_flip()+
  theme_classic()



# MINIMO K --------------------
arrange(loadings_pca, desc(abs(PC1)))[1:15,c('variable', 'Grupo','PC1')]
arrange(loadings_spc_orth, desc(abs(PC1)))[1:100,c('variable', 'Grupo','PC1')]

arrange(loadings_spc_orth_esparso, desc(abs(PC1)))[1:15,c('variable', 'Grupo','PC1')]
arrange(loadings_spc_orth_esparso, desc(abs(PC2)))[1:20,c('variable', 'Grupo','PC2')]
arrange(loadings_spc_orth_esparso, desc(abs(PC3)))[1:20,c('variable', 'Grupo','PC3')]

arrange(loadings_spc_orth_esparso, desc(abs(PC4)))[1:15,c('variable', 'Grupo','PC4')]
arrange(loadings_spc_orth_esparso, desc(abs(PC5)))[1:25,c('variable', 'Grupo','PC5')]
arrange(loadings_spc_orth_esparso, desc(abs(PC6)))[1:20,c('variable', 'Grupo','PC6')]





# LOLLIPOP POR GRUPO -----------------
comparo_grupo =  merge(loadings_pca[c('Grupo','Nivel','variable','PC1', 'PC2', 'PC3','PC4', 'PC5')]%>%
                         pivot_longer(cols=contains('PC'),names_to = 'PC',values_to = 'loadings'),
                       loadings_spc_orth[c('variable','PC1', 'PC2', 'PC3','PC4', 'PC5')]%>%
                         pivot_longer(cols=contains('PC'),names_to = 'PC',values_to = 'loadings'),
                       by='variable', all=T, suffixes = c(".PCA",".SPC")) 
comparo_grupo = comparo_grupo[comparo_grupo$PC.PCA==comparo_grupo$PC.SPC,]
comparo_grupo = comparo_grupo%>%select(!'PC.SPC')
names(comparo_grupo)[names(comparo_grupo) == 'PC.PCA'] = 'PC'

comparo_grupo =  merge(comparo_grupo,
                       loadings_spc_orth_esparso[c('variable','PC1', 'PC2', 'PC3','PC4','PC5')]%>%
                         pivot_longer(cols=contains('PC'),names_to = 'PC',values_to = 'loadings.SPC_e'),
                       by='variable', all=T, suffixes = c("",".SPC_e"))
names(comparo_grupo)
comparo_grupo = comparo_grupo[comparo_grupo$PC==comparo_grupo$PC.SPC_e,]
comparo_grupo = comparo_grupo%>%select(!'PC.SPC_e')
names(comparo_grupo)

comparo_grupo =  merge(comparo_grupo,
                       loadings_spca[c('variable','PC1', 'PC2', 'PC3','PC4','PC5')]%>%
                         pivot_longer(cols=contains('PC'),names_to = 'PC',values_to = 'loadings.SPCA'),
                       by='variable', all=T, suffixes = c("",".SPCA"))
names(comparo_grupo)
comparo_grupo = comparo_grupo[comparo_grupo$PC==comparo_grupo$PC.SPCA,]
comparo_grupo = comparo_grupo%>%select(!'PC.SPCA')
names(comparo_grupo)

comparo_grupo %>%
  filter(abs(loadings.SPC)>0)%>%
  filter(PC %in% c('PC1')) %>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,as.numeric(Grupo)), y= 0, 
                   xend = reorder(variable,as.numeric(Grupo)), yend = loadings.SPC,
                   col= Grupo), size=1)+
  geom_point(aes(reorder(variable,as.numeric(Grupo)),loadings.SPC, 
                 col=  Grupo, shape=Nivel),size=2)+
  labs(x='Variable',
       y='Peso', title='SPC ortog.')+
  ylim(c(-0.3,0.3))+
  coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
ggsave(device ='png',filename='grupoSPCortog.png', width = 5, height = 8)

# comparo_grupo %>%
#   filter(abs(loadings.SPCA)>0)%>%
#   ggplot()+
#   geom_hline(yintercept=0)+
#   geom_segment(aes(x =  reorder(variable,as.numeric(Grupo)), y= 0, 
#                    xend = reorder(variable,as.numeric(Grupo)), yend = loadings.SPCA,
#                    col= Grupo), size=1)+
#   geom_point(aes(reorder(variable,as.numeric(Grupo)),loadings.SPCA, 
#                  col=  Grupo, shape=Nivel),size=2)+
#   labs(x='Variable',
#        y='Loadings', title='SPCA lambda')+
#   # ylim(c(-0.5,0.5))+
#   coord_flip()+
#   facet_grid(cols = vars(PC))+
#   theme_minimal()

comparo_grupo %>%
  filter(abs(loadings.SPC_e)>0)%>%
  filter(PC %in% c('PC1','PC2','PC3')) %>%
   ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,as.numeric(Grupo)), y= 0, 
                   xend = reorder(variable,as.numeric(Grupo)), yend = loadings.SPC_e,
                   col= Grupo), size=2)+
  geom_point(aes(reorder(variable,as.numeric(Grupo)),loadings.SPC_e, 
                 col=  Grupo, shape=Nivel),size=4)+
  labs(x='Variable',
       y='Peso', title='SPC ortog. esparso')+
  ylim(c(-0.5,0.5))+
  coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()

ggsave(device ='png',filename='grupoSPCortogesparso.png', width = 10, height = 8)

## --- Biplots ----------

# PCA
options(ggrepel.max.overlaps = Inf)
# fviz_pca_biplot(pca,label = 'var', repel = TRUE, labelsize = 2,
#                  alpha.ind = 0.05, select.var = list(contrib = 10))


loadings_pca %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_point(aes(x=PC1, y=PC2), alpha=0.02, data=as.data.frame(pca$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*100, yend=PC2*100, color=Grupo),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8,size=1)+
  geom_point(aes(x=PC1*100, y=PC2*100, color=Grupo), alpha=0.8, size=1)+
  #geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Grupo),size=2,segment.linetype=0)+
  labs(y=paste('CP2 (', round(pca$var_explained[2]*100,1), '%)', sep=''),
       x=paste('CP1 (', round(pca$var_explained[1]*100,1), '%)', sep=''),
       title = 'PCA (todas las variables)')+
  theme_minimal() 
ggsave(device ='png',filename='PCA grupo.png', width = 6, height = 5)


loadings_pca %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_point(aes(x=PC1, y=PC2), alpha=0.02, data=as.data.frame(pca$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*100, yend=PC2*100, color=Grupo),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8,size=1)+
  geom_point(aes(x=PC1*100, y=PC2*100, color=Grupo), alpha=0.8, size=1)+
  #geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Grupo),size=2,segment.linetype=0)+
  labs(y=paste('PC2 (', round(pca$var_explained[2]*100,1), '%)', sep=''),
       x=paste('PC1 (', round(pca$var_explained[1]*100,1), '%)', sep=''),
       title = 'PCA (all variables)')+
  theme_minimal() 
ggsave(device ='png',filename='PCA grupo eng.png', width = 6, height = 5)

loadings_pca %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_point(aes(x=PC1, y=PC2), alpha=0.01, data=as.data.frame(pca$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*100, yend=PC2*100, color=Bienestar),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8)+
  geom_point(aes(x=PC1*100, y=PC2*100, color=Bienestar), alpha=0.8, size=1)+
  #geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Bienestar),size=2,segment.linetype=0)+
  labs(y=paste('CP2 (', round(pca$var_explained[2]*100,1), '%)', sep=''),
       x=paste('CP1 (', round(pca$var_explained[1]*100,1), '%)', sep=''),
       title = 'PCA (todas las variables)')+
  scale_color_viridis(discrete = T, direction = -1)+
  theme_minimal() 
ggsave(device ='png',filename='PCA bienestar.png', width = 6, height = 5)

loadings_pca %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=PC1, y=PC2), alpha=0.01, data=as.data.frame(pca$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*100, yend=PC2*100, color=Nivel),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8)+
  geom_point(aes(x=PC1*100, y=PC2*100, color=Nivel), alpha=0.8, size=1)+
  #geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Nivel),size=2,segment.linetype=0)+
  labs(y=paste('CP2 (', round(pca$var_explained[2]*100,1), '%)', sep=''),
       x=paste('CP1 (', round(pca$var_explained[1]*100,1), '%)', sep=''),
       title = 'PCA (todas las variables)')+
#  scale_color_viridis_d(begin = 0.3)+
  scale_colour_hue(l = 30)+
  theme_minimal() 
ggsave(device ='png',filename='PCA nivel.png', width = 6, height = 5)


# ORTOG

loadings_spc_orth %>%
  filter(abs(PC1) > 0 | abs(PC2) > 0) %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=PC1, y=PC2), alpha=0.01, data=as.data.frame(spc_fit_orth$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*50, yend=PC2*50, color=Grupo),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8, size=1)+
  geom_point(aes(x=PC1*50, y=PC2*50, color=Grupo), alpha=0.8, size=1)+
  #geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Grupo),size=2,segment.linetype=0)+
  labs(y=paste('CP2 (', round((spc_fit_orth$prop.var.explained[2]-spc_fit_orth$prop.var.explained[1])*100,1), '%)', sep=''),
       x=paste('CP1 (', round(spc_fit_orth$prop.var.explained[1]*100,1), '%)', sep=''),
       title = 'SPC ortog.')+
#  scale_colour_hue(l = 55)+
  theme_minimal() 
ggsave(device ='png',filename='SPC grupo.png', width = 6, height = 5)

loadings_spc_orth %>%
  filter(abs(PC1) > 0 | abs(PC2) > 0) %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=PC1, y=PC2), alpha=0.01, data=as.data.frame(spc_fit_orth$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*50, yend=PC2*50, color=Bienestar),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8)+
  geom_point(aes(x=PC1*50, y=PC2*50, color=Bienestar), alpha=0.8, size=1)+
 # geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Bienestar),size=2,segment.linetype=0)+
  labs(y=paste('CP2 (', round((spc_fit_orth$prop.var.explained[2]-spc_fit_orth$prop.var.explained[1])*100,1), '%)', sep=''),
       x=paste('CP1 (', round(spc_fit_orth$prop.var.explained[1]*100,1), '%)', sep=''),
       title = 'SPC ortog.')+
  scale_color_viridis(discrete = T, direction=-1)+
  theme_minimal() 
ggsave(device ='png',filename='SPC bienestar.png', width = 6, height = 5)

loadings_spc_orth %>%
  filter(abs(PC1) > 0 | abs(PC2) > 0) %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=PC1, y=PC2), alpha=0.01, data=as.data.frame(spc_fit_orth$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*50, yend=PC2*50, color=Nivel),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8)+
  geom_point(aes(x=PC1*50, y=PC2*50, color=Nivel), alpha=0.8, size=1)+
  # geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Nivel),size=2,segment.linetype=0)+
  labs(y=paste('CP2 (', round((spc_fit_orth$prop.var.explained[2]-spc_fit_orth$prop.var.explained[1])*100,1), '%)', sep=''),
       x=paste('CP1 (', round(spc_fit_orth$prop.var.explained[1]*100,1), '%)', sep=''),
       title = 'SPC ortog.')+
  scale_colour_hue(l = 30)+
  theme_minimal() 
ggsave(device ='png',filename='SPC nivel.png', width = 6, height = 5)

#Esparso
loadings_spc_orth_esparso %>%
  filter(abs(PC1) > 0 | abs(PC2) > 0) %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=V1, y=V2), alpha=0.01, data=as.data.frame(spc_fit_orth_esparso$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*10, yend=PC2*10, color=Grupo),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8, size=2)+
  geom_point(aes(x=PC1*10, y=PC2*10, color=Grupo), alpha=0.8, size=1)+
  #geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Grupo),size=2,segment.linetype=0)+
  labs(y=paste('PC2 (', round((spc_fit_orth_esparso$prop.var.explained[2]-spc_fit_orth_esparso$prop.var.explained[1])*100,1), '%)', sep=''),
       x=paste('PC1 (', round(spc_fit_orth_esparso$prop.var.explained[1]*100,1), '%)', sep=''),
       title = 'SPC ortog. esparso')+
  theme_minimal() 

loadings_spc_orth_esparso %>%
  filter(abs(PC1) > 0 | abs(PC2) > 0) %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=V1, y=V2), alpha=0.01, data=as.data.frame(spc_fit_orth_esparso$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*10, yend=PC2*10, color=Bienestar),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.4,size=2)+
  geom_point(aes(x=PC1*10, y=PC2*10, color=Bienestar), alpha=0.4, size=1)+
  # geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Bienestar),size=2,segment.linetype=0)+
  labs(y=paste('PC2 (', round((spc_fit_orth_esparso$prop.var.explained[2]-spc_fit_orth_esparso$prop.var.explained[1])*100,1), '%)', sep=''),
       x=paste('PC1 (', round(spc_fit_orth_esparso$prop.var.explained[1]*100,1), '%)', sep=''),
       title = 'SPC ortog. esparso')+
  scale_color_viridis(discrete = T, direction = -1)+
  theme_minimal() 

loadings_spc_orth_esparso %>%
  filter(abs(PC1) > 0 | abs(PC2) > 0) %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=PC1, y=PC2), alpha=0.01, data=as.data.frame(spc_fit_orth_esparso$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*10, yend=PC2*10, color=Nivel),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8,size=2)+
  geom_point(aes(x=PC1*10, y=PC2*10, color=Nivel), alpha=0.8, size=1)+
  # geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Nivel),size=2,segment.linetype=0)+
  labs(y=paste('PC2 (', round((spc_fit_orth_esparso$prop.var.explained[2]-spc_fit_orth_esparso$prop.var.explained[1])*100,1), '%)', sep=''),
       x=paste('PC1 (', round(spc_fit_orth_esparso$prop.var.explained[1]*100,1), '%)', sep=''),
       title = 'SPC ortog. esparso')+
  scale_color_viridis_d()+
  theme_minimal() 


loadings_spca %>%
  filter(abs(PC1) > 0 | abs(PC2) > 0) %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=PC1, y=PC2), alpha=0.01, data=as.data.frame(spca_fit_lambda$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*10, yend=PC2*10, color=Nivel),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8)+
  geom_point(aes(x=PC1*10, y=PC2*10, color=Nivel), alpha=0.8)+
  # geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Nivel),size=2,segment.linetype=0)+
  labs(y=paste('PC2 (', round(spca_fit_lambda$pev[2]*100,1), '%)', sep=''),
       x=paste('PC1 (', round(spca_fit_lambda$pev[1]*100,1), '%)', sep=''),
       title = 'SPCA lambda')+
  scale_color_viridis_d()+
  theme_minimal() 


# loadings_spca %>%
#   filter(abs(PC1) > 0 | abs(PC2) > 0) %>%
#   ggplot() +
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept =0)+
#   geom_point(aes(x=PC1, y=PC2.resid), alpha=0.01, data=as.data.frame(spca_fit_lambda$x))+
#   geom_segment(aes(x=0, y=0, xend=PC1*10, yend=PC2.resid*10, color=Nivel),
#                arrow=arrow(length=unit(0.2,"cm")), alpha=0.8)+
#   geom_point(aes(x=PC1*10, y=PC2.resid*10, color=Nivel), alpha=0.8)+
#   # geom_text_repel(aes(x=PC1, y=PC2.resid, label=variable, color=Nivel),size=2,segment.linetype=0)+
#   labs(y=paste('Residuos PC2 (', round(spca_fit_lambda$pev[2]*100,1), '%)', sep=''),
#        x=paste('PC1 (', round(spca_fit_lambda$pev[1]*100,1), '%)', sep=''),
#        title = 'SPCA lambda')+
#   scale_color_viridis_d()+
#   theme_minimal() 
# # Notemos que los loadings dejaron de ser cero..... :(
# sum(loadings_spca$PC2==0)
# min(abs(loadings_spca$PC2)[abs(loadings_spca$PC2)>0])
# min(abs(loadings_spca$PC2.resid))
# # El mínimo es más grande que los loadings más pequeño, y mucho más grande que haciendo lo mismo sobre el ortog:
# min(abs(lm(PC2~PC1, data=as.data.frame(spc_fit_orth$x))$residuals))
