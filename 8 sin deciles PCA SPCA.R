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
                               col_types = c("skip", "text", "text", "text", "text", "text", "skip"))

sacar = colnames(eph)[!colnames(eph) %in% colnames(eph_3T2019)]
eph = eph[colnames(eph)!=sacar]
rm(sacar)

# deciles
deciles <- readRDS('deciles.rds')
eph = eph[!colnames(eph) %in% deciles]

# escalamos
eph_scaled = scale(eph)
dim(eph_scaled)
# Matriz de correlación
matriz_cor = cor(eph_scaled)


# Scores de categorías del optimal scaling---------
#catscores <- readRDS('catscores.rds')
#catscores$INTENSI_ord
# --------------------------------------
# DESCRIPTIVA --------------------------
# --------------------------------------
str(eph)
summary(eph)[,1:10]

# Matriz de distancias para ver si hay clusters
#gc()
#memory.limit(1024*400)
#fviz_dist(dist(eph), show_labels = FALSE) 

# Grafico matriz de correlación
heatmap(matriz_cor)
image(z=matriz_cor)


# -----------------------------------
# REDUZCO LA DIMENSION --------------
# -----------------------------------

#### --------- PCA ---------
set.seed(9)
pca = prcomp(eph, center = T, scale = T)

pca$sdev # desvíos de los componentes principales
pca$var_explained <- pca$sdev^2/sum(pca$sdev^2) # varianza explicada por cada componente
plot(pca$var_explained)

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

par(mfrow=c(1,3))
for (cp in 1:3){
  plot(lambdas_zerol[,cp], lambdas_pev[,cp],  ylim=c(0, 0.15), xlim = c(0,129),
       main =  paste('PC',cp), ylab = 'PEV', xlab = 'Cant. zero loadings')
  text(lambdas_zerol[,cp], lambdas_pev[,cp], lambdas, pos=3)
}
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
cv.out$bestsumabsv # 6.96502
cv.out$bestsumabsv1se # 4.402789
spc_fit <- SPC(eph_scaled, sumabsv=cv.out$bestsumabsv1se, center = T, 
               K=15, v = cv.out$v.init,
               cnames = names(eph))

## Esparsitud óptima a mano
sumabsvs = c(1.4, 2.4, 3, 3.4, 4, 4.4, 5, 5.4, 6, 6.4, 7.4)
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
  plot(sumabsv_zerol[,cp], sumabsv_pev[,cp], ylim = c(0,0.4),
       main =  paste('PC',cp), ylab = 'Varianza acumulada hasta ese componenete', xlab = 'Cant. zero loadings')
  text(sumabsv_zerol[,cp], sumabsv_pev[,cp], sumabsvs, pos=3)
}
rm(sumabsvs, sumabsv_pev, sumabsv_zerol)
par(mfrow=c(1,1))

set.seed(9)
spc_fit_esparso <- SPC(eph_scaled, sumabsv=3.4, center = T, 
                       K=15, v = cv.out$v.init,
                       cnames = names(eph))

# Ortogonalidad

set.seed(9)
cv.out <- SPC.cv(eph_scaled, sumabsvs = seq(1.2, sqrt(ncol(eph)), len = 30),
                 orth = TRUE)
cv.out$bestsumabsv  #6.96502
cv.out$bestsumabsv1se #4.402789
spc_fit_orth <- SPC(eph_scaled, sumabsv=cv.out$bestsumabsv1se, center = T, 
                    K=15, v = cv.out$v.init, orth = T,
                    cnames = names(eph))

## Esparsitud óptima a mano
sumabsvs = c(1.4, 2.4, 3, 3.4, 4, 4.4,5, 5.4, 6)
sumabsv_pev = matrix(ncol = 15, nrow= 9)
sumabsv_zerol = matrix(ncol = 15, nrow= 9)
set.seed(9)
for (i in 1:9){
  spc_fit_orth_esparso <- SPC(eph_scaled, sumabsv=sumabsvs[i], center = T, 
                              K=15, orth=T,
                              cnames = names(eph))
  sumabsv_pev[i,] = spc_fit_orth_esparso$prop.var.explained
  sumabsv_zerol[i,] = apply(spc_fit_orth_esparso$v, 2, FUN = function(x) sum(x==0))
  
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
spc_fit_orth_esparso <- SPC(eph_scaled, sumabsv=3.4, center = T, 
                            K=15, orth=T,
                            cnames = names(eph))






#### ---------  SAVE ---------
save(pca, spca_fit, spca_fit_lambda,
     spc_fit, spc_fit_esparso, 
     spc_fit_orth, spc_fit_orth_esparso,
     file='PCASPCASPC_sin10.RData')

# #### --------- SPC POSITIVOS ---------
# #set.seed(9)
# #cv.out <- SPC.cv(eph_scaled, sumabsvs = seq(1.2, sqrt(ncol(eph)), len = 30), orth=T, vpos=T)
# #cv.out$bestsumabsv1se #1.2
# #pos_spc_fit_orth <- SPC(eph_scaled, sumabsv=cv.out$bestsumabsv1se, center = T,
# #               K=15, orth = T, vpos=T,
# #               cnames = names(eph))
# 
# sumabsvs = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2)
# sumabsv_pev = matrix(ncol = 15, nrow= 7)
# sumabsv_zerol = matrix(ncol = 15, nrow= 7)
# set.seed(9)
# for (i in 1:7){
#   pos_spc_fit_orth_esparso <- SPC(eph_scaled, sumabsv=sumabsvs[i], center = T, 
#                                   K=15, orth=T, vpos=T,
#                                   cnames = names(eph))
#   sumabsv_pev[i,] = pos_spc_fit_orth_esparso$prop.var.explained
#   sumabsv_zerol[i,] = apply(pos_spc_fit_orth_esparso$v, 2, FUN = function(x) sum(x==0))
#   
# }
# par(mfrow=c(1,3))
# for (cp in 1:3){
#   plot(sumabsv_zerol[,cp], sumabsv_pev[,cp], ylim = c(0,0.4),
#        main =  paste('PC',cp), ylab = 'Varianza acumulada hasta ese componenete', xlab = 'Cant. zero loadings')
#   text(sumabsv_zerol[,cp], sumabsv_pev[,cp], sumabsvs, pos=3)
# }
# rm(sumabsvs, sumabsv_pev, sumabsv_zerol)
# par(mfrow=c(1,1))
# 
# 
# set.seed(9)
# pos_spc_fit_orth_esparso <- SPC(eph_scaled, sumabsv=3.2, center = T,
#                                 K=15, orth = T, vpos=T,
#                                 cnames = names(eph))
# set.seed(9)
# pos_spc_fit_orth <- SPC(eph_scaled, sumabsv=5.2, center = T,
#                         K=15, orth=T, vpos = T,
#                         cnames = names(eph))
# 
# 
# 
# 




#### ---------  LOAD ---------
load(file='PCASPCASPC_sin10.RData')



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

## --- PEV PCA ----
plot(cumsum(pca$var_explained),
     main = 'PCA - varianza explicada acumulando componentes principales',
     xlab= 'Cantidad de componentes principales',
     ylab = 'Suma acumulada de varianza explicada',
     ylim = c(0,1)) 

# eigenvalues
get_eigenvalue(pca)
sum(get_eigenvalue(pca)$eigenvalue>1) # 34 autovalores mayores a 1 ..............



## --- PEV y loadings == 0 15 primeros componentes ----

# PEV
#par(mfrow=c(1,3))
par(mfrow=c(1,1))
plot(cumsum(pca$var_explained)[1:15],
     main = 'Varianza explicada acumulando 15 CPs',
     xlab= 'Cantidad de componentes principales (CPs)',
     ylab = 'Suma acumulada % varianza explicada',
     ylim = c(0,1),
     type = 'b')
lines(cumsum(spca_fit$pev), type = 'b',col = 'blue')
lines(cumsum(spca_fit_lambda$pev), type = 'b',col = 'red')
lines(spc_fit$prop.var.explained, type = 'b',col = 'violet')
lines(spc_fit_esparso$prop.var.explained, type = 'b',col = 'green')
lines(spc_fit_orth$prop.var.explained, type = 'b',col = 'deeppink')
lines(spc_fit_orth_esparso$prop.var.explained, type = 'b',col = 'darkgreen')
lines(pos_spc_fit_orth$prop.var.explained, type = 'b', pch=20, col='gray44')
lines(pos_spc_fit_orth_esparso$prop.var.explained, type = 'b', pch=20, col = 'grey')


#abline(h=0.2,  col='grey')
#abline(h=0.25, col='grey')
#abline(h=0.3,  col='grey')
#abline(h=0.4,  col='grey')
#abline(h=0.5,  col='grey')
legend("topleft", 
       legend=c("PCA", 
                "SPCA - varnum", "SPCA - lambda", 
                "SPC", "SPC - esparso",
                "SPC ortog.", "SPC ortog.- esparso"#,
                #"SPC ortog. POSITIVO", "SPC ortog. - esparso POSITIVO"
                ),
       y.intersp = 1,
       col=c("black",
              "blue","red",
              "violet", "green", 
             "deeppink", "darkgreen"
             #,"gray44", "grey"
             ), cex=0.9,pch=16 )


## Loadings ==  0 

# CUANTAS VARIABLES TIENEN LOADING CERO EN LOS PRIMEROS K COMPONENTES PARA LA TABLA
sum(spc_fit_orth$v[,1]==0)
sum(rowSums(spc_fit_orth$v[,1:2])==0)
sum(rowSums(spc_fit_orth$v[,1:3])==0)
sum(rowSums(spc_fit_orth$v[,1:2])==0)
sum(spca_fit_lambda$loadings[,1]==0)
spca_fit_lambda$pev
sum(rowSums(spca_fit_lambda$loadings[,1:2])==0)
sum(rowSums(spca_fit_lambda$loadings[,1:4])==0)
sum(rowSums(spca_fit_lambda$loadings[,1:6])==0)
sum(rowSums(spca_fit_lambda$loadings[,1:12])==0)
sum(spc_fit_orth_esparso$v[,1]==0)
spc_fit_esparso$prop.var.explained
sum(rowSums(spc_fit_orth_esparso$v[,1:2])==0)
sum(rowSums(spc_fit_orth_esparso$v[,1:3])==0)
sum(rowSums(spc_fit_orth_esparso$v[,1:4])==0)
sum(rowSums(spc_fit_orth_esparso$v[,1:6])==0)
sum(rowSums(spc_fit_orth_esparso$v[,1:12])==0)

# Gráfico de cantidad de loadings cero en cada componente principal
# par(mar=c(5.1, 4.1, 4.1, 10.1), xpd=TRUE)
plot(apply(pca$rotation[,1:15], 2, FUN = function(x) sum(x==0)),
     main = 'Variables con loading cero - primeros 15 CPs',
     xlab= 'Componente principal (CP)',
     ylab = 'Cant. variables con loading cero',
     ylim = c(0,ncol(eph)),
     type = 'b')
lines(apply(spca_fit$loadings, 2, FUN = function(x) sum(x==0)), type = 'b',col = 'blue')
lines(apply(spca_fit_lambda$loadings, 2, FUN = function(x) sum(x==0)), type = 'b',col = 'red')
lines(apply(spc_fit$v, 2, FUN = function(x) sum(x==0)), type = 'b',col = 'violet')
lines(apply(spc_fit_esparso$v, 2, FUN = function(x) sum(x==0)), type = 'b', col = 'green' )
lines(apply(spc_fit_orth$v, 2, FUN = function(x) sum(x==0)), type = 'b', col = 'deeppink' )
lines(apply(spc_fit_orth_esparso$v, 2, FUN = function(x) sum(x==0)), type = 'b', col = 'darkgreen' )
lines(apply(pos_spc_fit_orth$v, 2, FUN = function(x) sum(x==0)), type = 'b', col = 'gray44', pch=20 )
lines(apply(pos_spc_fit_orth_esparso$v, 2, FUN = function(x) sum(x==0)), type = 'b', col = 'grey', pch=20 )

# legend("topright", inset=c(-0.5,0),
#        legend=c("PCA", "SPCA - varnum", "SPCA - lambda", 
#                 "SPC", "SPC - esparso",
#                 "SPC ortog.", "SPC ortog.- esparso"),
#        y.intersp = 1,
#        col=c("black","blue","red","violet","green", "deeppink", "darkgreen"), cex=0.8, lty = 1)
# abline(v=2,lty=2,col='grey')
# abline(v=8,lty=2,col='grey')


## ESPARSITUD VERSUS VARIANZA
#par(mar=c(5, 4, 4, 2) + 0.1, xpd=FALSE)
plot(apply(pca$rotation[,1:4], 2, FUN = function(x) sum(x==0)),
     cumsum(pca$var_explained)[1:4],
     main = 'Esparsitud vs. varianza - primeros 4 CPs ',
     xlab= 'Cantidad de variables con loading == 0',
     ylab = 'Suma acumulada % varianza explicada',
     ylim = c(0,0.5),
     xlim = c(0, ncol(eph)),
     type = 'b')
lines(apply(spca_fit$loadings[,1:4], 2, FUN = function(x) sum(x==0)),
      cumsum(spca_fit$pev)[1:4], type = 'b',col = 'blue')
lines(apply(spca_fit_lambda$loadings[,1:4], 2, FUN = function(x) sum(x==0)), 
      cumsum(spca_fit_lambda$pev)[1:4], type = 'b',col = 'red')
lines(apply(spc_fit$v[,1:4], 2, FUN = function(x) sum(x==0)), 
      spc_fit$prop.var.explained[1:4], type = 'b',col = 'violet')
lines(apply(spc_fit_esparso$v[,1:4], 2, FUN = function(x) sum(x==0)), 
      spc_fit_esparso$prop.var.explained[1:4], type = 'b', col = 'green' )
lines(apply(spc_fit_orth$v[,1:4], 2, FUN = function(x) sum(x==0)), 
      spc_fit_orth$prop.var.explained[1:4], type = 'b', col = 'deeppink' )
lines(apply(spc_fit_esparso$v[,1:4], 2, FUN = function(x) sum(x==0)), 
      spc_fit_orth_esparso$prop.var.explained[1:4], type = 'b', col = 'darkgreen' )
# legend("topright", 
#        legend=c("PCA", "SPCA - varnum", "SPCA - lambda", 
#                 "SPC", "SPC - esparso",
#                 "SPC ortog.", "SPC ortog.- esparso"),
#        y.intersp = 1,
#        col=c("black","blue","red","violet","green", "deeppink", "darkgreen"), cex=0.8, lty = 1)


## --- Loadings ordenados de mayor a menor para ver esparsitud ----

par(mfrow=c(1,3))
plot(sort(loadings_pca$PC1, decreasing =T),
     xlab = 'Variable (ordenadas por loading en CP1)',
     ylab = 'Loading',
     main = 'PCA - Primer componente principal')
abline(h=0)
plot(sort(loadings_pca$PC2, decreasing =T),
     xlab = 'Variable (ordenadas por loading en CP2)',
     ylab = 'Loading',
     main = 'PCA - Segundo componente principal',)
abline(h=0)
plot(sort(loadings_pca$PC3, decreasing =T),
     xlab = 'Variable (ordenadas por loading en CP3)',
     ylab = 'Loading',
     main = 'PCA - Tercer componente principal')
abline(h=0)


par(mfrow=c(1,3))
plot(sort(loadings_spc_orth$PC1, decreasing =T),
     xlab = 'Variable (ordenadas por loading en CP1)',
     ylab = 'Loading',
     main = 'spc_orh - Primer componente principal')
abline(h=0)
plot(sort(loadings_spc_orth$PC2, decreasing =T),
     xlab = 'Variable (ordenadas por loading en CP2)',
     ylab = 'Loading',
     main = 'spc_orh - Segundo componente principal',)
abline(h=0)
plot(sort(loadings_spc_orth$PC3, decreasing =T),
     xlab = 'Variable (ordenadas por loading en CP3)',
     ylab = 'Loading',
     main = 'spc_orh - Tercer componente principal')
abline(h=0)



# (B) Dimensiones del bienetar ----
## --- Barras por grupo ----

# CP1 en PCA versus SPC

### Color : ordenamiento en bienestar
loadings_pca %>% 
  ggplot()+
  geom_col(aes(x=reorder(variable,PC1),y=PC1,
               fill = Bienestar, color = NULL),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Loadings primer componente', title = 'PCA')+
  coord_flip()+
  scale_color_viridis(discrete=T, direction=-1)+
  scale_fill_viridis(discrete=T, direction=-1)+
  theme_classic()
#ggsave(device ='png',filename='loadings_1er_PCA.png', width = 6, height = 12)


loadings_spc_orth %>% 
  filter(abs(PC1)>0)%>%
  ggplot()+
  geom_col(aes(x=reorder(variable,PC1),y=PC1,
               fill = Bienestar, color = NULL),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Loadings primer componente', title = 'SPC ortog.')+
  coord_flip()+
  scale_color_viridis(discrete=T, direction=-1)+
  scale_fill_viridis(discrete=T, direction=-1)+
  theme_classic()
#ggsave(device ='png',filename='loadings_1er_SPC.png', width = 6, height = 12)

# loadings_spca %>% 
#   filter(abs(PC1)>0)%>%
#   ggplot()+
#   geom_col(aes(x=reorder(variable,PC1),y=PC1,
#                fill = Bienestar, color = NULL),
#            size=0, alpha=0.5)+
#   ylim(c(-0.5, 0.5))+
#   labs(x='', y='Loadings primer componente', title = 'SPCA lambda')+
#   coord_flip()+
#   scale_color_viridis(discrete=T, direction=-1)+
#   scale_fill_viridis(discrete=T, direction=-1)+
#   theme_classic()
# ggsave(device ='png',filename='loadings_1er_SPCA.png', width = 6, height = 10)

# loadings_spca %>% 
#   filter(abs(PC2)>0)%>%
#   ggplot()+
#   geom_col(aes(x=reorder(variable,PC2),y=PC2,
#                fill = Bienestar, color = NULL),
#            size=0, alpha=0.5)+
#   ylim(c(-0.5, 0.5))+
#   labs(x='', y='Loadings segundo componente', title = 'SPCA lambda')+
#   coord_flip()+
#   scale_color_viridis(discrete=T, direction=-1)+
#   scale_fill_viridis(discrete=T, direction=-1)+
#   theme_classic()
# 

loadings_spc_orth_esparso %>% 
  filter(abs(PC1)>0)%>%
  ggplot()+
  geom_col(aes(x=reorder(variable,PC1),y=PC1,
               fill = Bienestar, color = NULL),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Loadings primer componente', title = 'SPC ortog. esparso')+
  coord_flip()+
  scale_color_viridis(discrete=T, direction=-1)+
  scale_fill_viridis(discrete=T, direction=-1)+
  theme_classic()
#ggsave(device ='png',filename='loadings_1er_SPCe.png', width = 6, height = 4)

loadings_spc_orth_esparso %>% 
  filter(abs(PC2)>0)%>%
  ggplot()+
  geom_col(aes(x=reorder(variable,PC2),y=PC2,
               fill = Bienestar, color = NULL),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Loadings segundo componente', title = 'SPC ortog. esparso')+
  coord_flip()+
  scale_color_viridis(discrete=T, direction=1)+
  scale_fill_viridis(discrete=T, direction=1)+
  theme_classic()
#ggsave(device ='png',filename='loadings_2do_SPCe.png', width = 6, height = 4)

loadings_spc_orth_esparso %>% 
  filter(abs(PC3)>0)%>%
  ggplot()+
  geom_col(aes(x=reorder(variable,PC3),y=PC3,
               fill = Bienestar, color = NULL),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Loadings tercer componente', title = 'SPC ortog. esparso')+
  coord_flip()+
  scale_color_viridis(discrete=T, direction=-1)+
  scale_fill_viridis(discrete=T, direction=-1)+
  theme_classic()
#ggsave(device ='png',filename='loadings_3er_SPCe.png', width = 6, height = 4)

### Color: Grupo
loadings_pca %>% 
  ggplot()+
  geom_col(aes(x=reorder(variable,PC1),y=PC1,
               fill = Grupo, color = Grupo),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Loadings', title = 'PCA - CP1')+
  coord_flip()+
  theme_classic()
ggsave(device ='png',filename='loadings_1er_PCA.png', width = 6, height = 14)


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
ggsave(device ='png',filename='loadings_1er_SPC.png', width = 6, height = 14)


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
ggsave(device ='png',filename='loadings_1er_SPCesparso.png', width = 6, height = 5)

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
ggsave(device ='pdf',filename='loadings_2do_SPCesparso.png', width = 6, height = 5)

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
ggsave(device ='pdf',filename='loadings_3er_SPCesparso.png', width = 6, height = 5)


loadings_spc_orth_esparso %>% 
  filter(abs(PC4)>0)%>%
  ggplot()+
  geom_col(aes(x=reorder(variable,PC4),y=PC4,
               fill = Grupo, color = Grupo),
           size=0, alpha=0.5)+
  ylim(c(-0.5, 0.5))+
  labs(x='', y='Loadings', title = 'SPC ortog. esparso - CP3')+
  coord_flip()+
  theme_classic()
ggsave(device ='pdf',filename='loadings_4to_SPCesparso.png', width = 6, height = 5)


# Resumen
arrange(loadings_pca, desc(abs(PC1)))[1:15,c('variable', 'Grupo','PC1')]
arrange(loadings_spc_orth_esparso, desc(abs(PC1)))[1:15,c('variable', 'Grupo','PC1')]
arrange(loadings_pca, desc(abs(PC2)))[1:15,c('variable', 'Grupo','PC2')]
arrange(loadings_spc_orth_esparso, desc(abs(PC2)))[1:15,c('variable', 'Grupo','PC2')]





## --- Lollipop: PCA, SPCA y SPC por grupo y componente ----
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




#GENERAL ------------------

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
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPCA,
                   col= '3. SPCA lambda'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPCA, 
                 col='3. SPCA lambda'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPC_e,
                   col= '4. SPC ortog. esparso'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPC_e, 
                 col='4. SPC ortog. esparso'), alpha=0.7)+
  labs(x='Variable 
       (ordenadas por valor absoluto en el vector de loadings del PC1)',
       y='Loadings', main='Loadings de las variables en cada modelo')+
  scale_x_discrete(breaks=NULL) +
  scale_color_viridis(discrete=T)+
  #ylim(c(-0.5,0.5))+
  coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
#ggsave(device ='png',filename='lollipopPC1.png', width = 5, height = 6)

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
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.SPCA),
                   col= '3. SPCA lambda'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.SPCA), 
                 col='3. SPCA lambda'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.SPC_e),
                   col= '4. SPC ortog. esparso'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.SPC_e), 
                 col='4. SPC ortog. esparso'), alpha=0.7)+
  labs(x='Variable 
       (ordenadas por valor absoluto en el vector de loadings del PC1)',
       y='Loadings', main='Loadings de las variables en cada modelo')+
  scale_x_discrete(breaks=NULL) +
  scale_color_viridis(discrete=T)+
  #ylim(c(-0.5,0.5))+
  coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
#ggsave(device ='png',filename='lollipopPC1abs.png', width = 5, height = 6)

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
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPCA,
                   col= '3. SPCA lambda'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPCA, 
                 col='3. SPCA lambda'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPC_e,
                   col= '4. SPC ortog. esparso'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPC_e, 
                 col='4. SPC ortog. esparso'), alpha=0.7)+
  labs(x='Variable 
       (ordenadas por valor absoluto en el vector de loadings del PC2)',
       y='Loadings', main='Loadings de las variables en cada modelo')+
  scale_x_discrete(breaks=NULL) +
  scale_color_viridis(discrete=T)+
  ylim(c(-0.5,0.5))+
  coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
#ggsave(device ='png',filename='lollipopPC2.png', width = 5, height = 6)

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
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.SPCA),
                   col= '3. SPCA lambda'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.SPCA), 
                 col='3. SPCA lambda'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = abs(loadings.SPC_e),
                   col= '4. SPC ortog. esparso'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),abs(loadings.SPC_e), 
                 col='4. SPC ortog. esparso'), alpha=0.7)+
  labs(x='Variable 
       (ordenadas por valor absoluto en el vector de loadings del PC2)',
       y='Loadings', main='Loadings de las variables en cada modelo')+
  scale_x_discrete(breaks=NULL) +
  scale_color_viridis(discrete=T)+
  ylim(c(-0.5,0.5))+
  coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
#ggsave(device ='png',filename='lollipopPC2abs.png', width = 5, height = 6)



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
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPCA,
                   col= '3. SPCA lambda'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPCA, 
                 col='3. SPCA lambda'), alpha=0.7)+
  geom_segment(aes(x =  reorder(variable,abs(loadings.PCA)), y= 0, 
                   xend = reorder(variable,abs(loadings.PCA)), yend = loadings.SPC_e,
                   col= '4. SPC ortog. esparso'), alpha=0.7)+
  geom_point(aes(reorder(variable,abs(loadings.PCA)),loadings.SPC_e, 
                 col='4. SPC ortog. esparso'), alpha=0.7)+
  labs(x='Variable 
       (ordenadas por valor absoluto en el vector de loadings del PC1)',
       y='Loadings', main='Loadings de las variables en cada modelo')+
  scale_x_discrete(breaks=NULL) +
  scale_color_viridis(discrete=T)+
  ylim(c(-0.5,0.5))+
  coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()




# POR GRUPO -----------------
comparo_grupo %>%
  filter(abs(loadings.SPC)>0)%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,as.numeric(Grupo)), y= 0, 
                   xend = reorder(variable,as.numeric(Grupo)), yend = loadings.SPC,
                   col= Grupo), size=1)+
  geom_point(aes(reorder(variable,as.numeric(Grupo)),loadings.SPC, 
                 col=  Grupo, shape=Nivel),size=2)+
  labs(x='Variable',
       y='Loadings', title='SPC ortog')+
  ylim(c(-0.5,0.5))+
  coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()

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
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,as.numeric(Grupo)), y= 0, 
                   xend = reorder(variable,as.numeric(Grupo)), yend = loadings.SPC_e,
                   col= Grupo), size=2)+
  geom_point(aes(reorder(variable,as.numeric(Grupo)),loadings.SPC_e, 
                 col=  Grupo, shape=Nivel),size=4)+
  labs(x='Variable',
       y='Loadings', title='SPC ortog esparso')+
  ylim(c(-0.5,0.5))+
  coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()



## --- Biplots ----------

# PCA
options(ggrepel.max.overlaps = Inf)
fviz_pca_biplot(pca,label = 'var', repel = TRUE, labelsize = 2,
                alpha.ind = 0.05, select.var = list(contrib = 10))


loadings_pca %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=PC1, y=PC2), alpha=0.01, data=as.data.frame(pca$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*100, yend=PC2*100, color=Grupo),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8)+
  geom_point(aes(x=PC1*100, y=PC2*100, color=Grupo), alpha=0.8, size=1)+
  #geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Grupo),size=2,segment.linetype=0)+
  labs(y=paste('PC2 (', round(pca$var_explained[2]*100,1), '%)', sep=''),
       x=paste('PC1 (', round(pca$var_explained[1]*100,1), '%)', sep=''),
       title = 'PCA (todas las variables)')+
  theme_minimal() 

loadings_pca %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=PC1, y=PC2), alpha=0.01, data=as.data.frame(pca$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*100, yend=PC2*100, color=Bienestar),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8)+
  geom_point(aes(x=PC1*100, y=PC2*100, color=Bienestar), alpha=0.8, size=1)+
  #geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Bienestar),size=2,segment.linetype=0)+
  labs(y=paste('PC2 (', round(pca$var_explained[2]*100,1), '%)', sep=''),
       x=paste('PC1 (', round(pca$var_explained[1]*100,1), '%)', sep=''),
       title = 'PCA (todas las variables)')+
  scale_color_viridis(discrete = T, direction = -1)+
  theme_minimal() 


loadings_pca %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=PC1, y=PC2), alpha=0.01, data=as.data.frame(pca$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*100, yend=PC2*100, color=Nivel),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8)+
  geom_point(aes(x=PC1*100, y=PC2*100, color=Nivel), alpha=0.8, size=1)+
  #geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Nivel),size=2,segment.linetype=0)+
  labs(y=paste('PC2 (', round(pca$var_explained[2]*100,1), '%)', sep=''),
       x=paste('PC1 (', round(pca$var_explained[1]*100,1), '%)', sep=''),
       title = 'PCA (todas las variables)')+
  scale_color_viridis_d()+
  theme_minimal() 

# ORTOG

loadings_spc_orth %>%
  filter(abs(PC1) > 0 | abs(PC2) > 0) %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=V1, y=V2), alpha=0.01, data=as.data.frame(spc_fit_orth$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*50, yend=PC2*50, color=Grupo),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8)+
  geom_point(aes(x=PC1*50, y=PC2*50, color=Grupo), alpha=0.8, size=1)+
  #geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Grupo),size=2,segment.linetype=0)+
  labs(y=paste('PC2 (', round((spc_fit_orth$prop.var.explained[2]-spc_fit_orth$prop.var.explained[1])*100,1), '%)', sep=''),
       x=paste('PC1 (', round(spc_fit_orth$prop.var.explained[1]*100,1), '%)', sep=''),
       title = 'SPC ortog.')+
  theme_minimal() 

loadings_spc_orth %>%
  filter(abs(PC1) > 0 | abs(PC2) > 0) %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=V1, y=V2), alpha=0.01, data=as.data.frame(spc_fit_orth$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*50, yend=PC2*50, color=Bienestar),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8)+
  geom_point(aes(x=PC1*50, y=PC2*50, color=Bienestar), alpha=0.8, size=1)+
  # geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Bienestar),size=2,segment.linetype=0)+
  labs(y=paste('PC2 (', round((spc_fit_orth$prop.var.explained[2]-spc_fit_orth$prop.var.explained[1])*100,1), '%)', sep=''),
       x=paste('PC1 (', round(spc_fit_orth$prop.var.explained[1]*100,1), '%)', sep=''),
       title = 'SPC ortog.')+
  scale_color_viridis(discrete = T, direction=-1)+
  theme_minimal() 

loadings_spc_orth %>%
  filter(abs(PC1) > 0 | abs(PC2) > 0) %>%
  ggplot() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept =0)+
  geom_point(aes(x=V1, y=V2), alpha=0.01, data=as.data.frame(spc_fit_orth$x))+
  geom_segment(aes(x=0, y=0, xend=PC1*50, yend=PC2*50, color=Nivel),
               arrow=arrow(length=unit(0.2,"cm")), alpha=0.8)+
  geom_point(aes(x=PC1*50, y=PC2*50, color=Nivel), alpha=0.8, size=1)+
  # geom_text_repel(aes(x=PC1, y=PC2, label=variable, color=Nivel),size=2,segment.linetype=0)+
  labs(y=paste('PC2 (', round((spc_fit_orth$prop.var.explained[2]-spc_fit_orth$prop.var.explained[1])*100,1), '%)', sep=''),
       x=paste('PC1 (', round(spc_fit_orth$prop.var.explained[1]*100,1), '%)', sep=''),
       title = 'SPC ortog.')+
  scale_color_viridis_d()+
  theme_minimal() 

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

######## PRUEBA ##########
eph = readRDS('eph_scaled.rds')
eph_3T2019 = readRDS('eph_scaled_3T2019.rds')
variables_usadas <- read_excel("G:/Mi unidad/TESIS ECON/Sobre la EPH/variables usadas.xlsx", 
                               col_types = c("skip", "text", "text", "text", "text", "text", "skip"))

sacar = colnames(eph)[!colnames(eph) %in% colnames(eph_3T2019)]
eph = eph[colnames(eph)!=sacar]
rm(sacar)

# deciles
deciles <- readRDS('deciles.rds')
hist(eph[deciles][,1])
hist(eph[deciles][,2])
hist(eph[deciles][,3])
hist(eph['RDECOCUR'][,1])

# quedarme con un solo decil


deciles_placebo = deciles[deciles!="RDECOCUR"]
eph = eph[!colnames(eph) %in% deciles_placebo]
set.seed(9)
pca = prcomp(eph, center = T, scale = T)
loadings_pca = as_tibble(pca$rotation,rownames= 'variable')
loadings_pca =  merge(loadings_pca, variables_usadas[c('Nombre', 'Grupo','Nivel','Bienestar')],
                      by.x='variable',by.y='Nombre',all.x = T  )

names(loadings_pca)
loadings_pca %>%
 # filter(abs(PC1)>0)%>%
  ggplot()+
  geom_hline(yintercept=0)+
 geom_point(aes(reorder(variable,abs(PC1)),PC1, 
                 col=  Grupo, shape=Nivel),size=2)+
  labs(x='Variable',
       y='Loadings', title='SPC ortog')+
  ylim(c(-0.5,0.5))+
  coord_flip()+
 theme_minimal()


# resamplear random los valores del decil
set.seed(9)
eph$RDECOCUR = sample(eph$RDECOCUR,nrow(eph))
pca = prcomp(eph, center = T, scale = T)
loadings_pca = as_tibble(pca$rotation,rownames= 'variable')
loadings_pca =  merge(loadings_pca, variables_usadas[c('Nombre', 'Grupo','Nivel','Bienestar')],
                      by.x='variable',by.y='Nombre',all.x = T  )
loadings_pca %>%
  # filter(abs(PC1)>0)%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_point(aes(reorder(variable,abs(PC1)),PC1, 
                 col=  Grupo, shape=Nivel),size=2)+
  labs(x='Variable',
       y='Loadings', title='SPC ortog')+
  ylim(c(-0.5,0.5))+
  coord_flip()+
  theme_minimal()

hist(eph$RDECOCUR)
