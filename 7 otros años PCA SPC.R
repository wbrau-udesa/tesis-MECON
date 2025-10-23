library(tidyverse)
library(readxl)

library(ggrepel)
library(gridExtra)
library(viridis)
library(rgl)
library(scales)
library(scatterplot3d) 

library(elasticnet)
library(PMA)
library(factoextra)
library(FactoMineR)

library(depth)

# XXXXXXXXXXXXXXXXXXXXXXXXXXX Validación XXXXXXXXXXXXXXXXXXXXX
# -------------------------------------
# Cargar bases y otros  ------------------------
# -------------------------------------

# EPHs
eph = readRDS('eph_scaled.rds') 
eph_3T2019 = readRDS('eph_scaled_3T2019.rds')
sacar = colnames(eph)[!colnames(eph) %in% colnames(eph_3T2019)]
eph = eph[colnames(eph)!=sacar]
rm(sacar)

# escalamos
eph_scaled = scale(eph)
eph_scaled_3T2019 = scale(eph_3T2019)

# PCA-----
set.seed(9)
pca_3T2019 = prcomp(eph_3T2019, center = T, scale = T)

# SPC ORTH ESPARSO -----------
set.seed(9)
spc_fit_orth_esparso_3T2019 <- SPC(eph_scaled_3T2019, sumabsv=3.4, center = T, orth=T,
                                   K=15, cnames = names(eph_3T2019))
# SPC ORTH  -----------
set.seed(9)
#cv.out <- SPC.cv(eph_scaled_3T2019, sumabsvs = seq(1.2, sqrt(ncol(eph_scaled_3T2019)), len = 30),
#                 orth = TRUE)
#cv.out$bestsumabsv 
#cv.out$bestsumabsv1se # 7.10 NO HAGO CV PUES YA ESTO LO DEFINI PARA EL OTRO MODELO
spc_fit_orth_3T2019 <- SPC(eph_scaled, sumabsv=6.4, center = T, 
                           K=15, orth = T,
                           cnames = names(eph))

# SAVE -----
save(pca_3T2019, spc_fit_orth_3T2019, spc_fit_orth_esparso_3T2019, file ='3T2019.RData')

# LOAD -----
load('3T2019.RData')
load(file='PCASPCASPC.RData')
rm(spc_fit, spc_fit_esparso, spca_fit_lambda, spca_fit)


# DATAFRAMES DE LOADINGS -----

# 4T
loadings_pca = as_tibble(pca$rotation,rownames= 'variable')
loadings_spc_orth_esparso = as_tibble(spc_fit_orth_esparso$v)
loadings_spc_orth = as_tibble(spc_fit_orth$v)

colnames(loadings_spc_orth_esparso) = gsub("V","PC",colnames(loadings_spc_orth_esparso))
colnames(loadings_spc_orth) = gsub("V","PC",colnames(loadings_spc_orth))

loadings_spc_orth_esparso$variable = spc_fit_orth_esparso$cnames
loadings_spc_orth$variable = spc_fit_orth$cnames

# Invierto el sentido
loadings_spc_orth_esparso$PC1 = - loadings_spc_orth_esparso$PC1
loadings_spc_orth$PC1 = - loadings_spc_orth$PC1
loadings_pca$PC3 = - loadings_pca$PC3

# 3T 2019
loadings_spc_orth_esparso_3T2019 = as_tibble(spc_fit_orth_esparso_3T2019$v)
colnames(loadings_spc_orth_esparso_3T2019) = gsub("V","PC",colnames(loadings_spc_orth_esparso_3T2019))
loadings_spc_orth_esparso_3T2019$variable = spc_fit_orth_esparso_3T2019$cnames

loadings_spc_orth_3T2019 = as_tibble(spc_fit_orth_3T2019$v)
colnames(loadings_spc_orth_3T2019) = gsub("V","PC",colnames(loadings_spc_orth_3T2019))
loadings_spc_orth_3T2019$variable = spc_fit_orth_3T2019$cnames

# Invierto el sentido
loadings_spc_orth_esparso_3T2019$PC1 = - loadings_spc_orth_esparso_3T2019$PC1
loadings_spc_orth_3T2019$PC1 = - loadings_spc_orth_3T2019$PC1



# COMPARAR LOADINGS EN EL TIEMPO ----

# ESPARSO
names(loadings_spc_orth_esparso)
comparo_grupo =  merge(loadings_spc_orth_esparso[c("variable","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8")] %>% 
                         pivot_longer(cols=contains('PC'), names_to = 'PC',values_to = 'loadings'),
                       loadings_spc_orth_esparso_3T2019[c("variable","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8")]  %>% 
                         pivot_longer(cols=contains('PC'),names_to = 'PC',values_to = 'loadings'),
                       by='variable', all=T, suffixes = c(".4t2019",".3t2019")) 
comparo_grupo = comparo_grupo[comparo_grupo$PC.3t2019==comparo_grupo$PC.4t2019,]
comparo_grupo = comparo_grupo%>%select(!'PC.3t2019')
comparo_grupo = comparo_grupo %>%
  pivot_longer(cols=contains('loadings'), names_to = 'Modelo', values_to = 'loadings',
               names_prefix ="loadings.")
names(comparo_grupo)[names(comparo_grupo)=="PC.4t2019"] = 'PC'

comparo_grupo %>%
  filter(PC%in%c('PC1', 'PC2', 'PC3'))%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,abs(loadings)), y= 0, 
                   xend = reorder(variable,abs(loadings)), yend = loadings,
                   col=Modelo), alpha=0.5)+
  geom_point(aes(reorder(variable,abs(loadings)),loadings, col=Modelo), alpha=0.5)+
  labs(x='Variables (ordenadas por valor absoluto de pesos en primeros tres CPs)',
       y='Pesos', title = 'SPC ortog. esparso')+
  scale_x_discrete(breaks = NULL)+
  coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
ggsave(device ='png',filename='tiempoSPCortogesp.png', width = 6, height = 4)


comparo_grupo %>%
  filter(PC%in%c('PC1', 'PC2', 'PC3'))%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,abs(loadings)), y= 0, 
                   xend = reorder(variable,abs(loadings)), yend = loadings,
                   col=Modelo), alpha=0.5)+
  geom_point(aes(reorder(variable,abs(loadings)),loadings, col=Modelo), alpha=0.5)+
  labs(x='Variables (ordered by absolute value of loadings in the first 3 PCs)',
       y='Loadings', title = 'SPC orthog. sparse')+
  scale_x_discrete(breaks = NULL)+
  coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
ggsave(device ='png',filename='tiempoSPCortogesp-ENG.png', width = 6, height = 4)


# ORTH
names(loadings_spc_orth)
comparo_grupo =  merge(loadings_spc_orth[c("variable","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8")] %>% 
                         pivot_longer(cols=contains('PC'), names_to = 'PC',values_to = 'loadings'),
                       loadings_spc_orth_3T2019[c("variable","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8")]  %>% 
                         pivot_longer(cols=contains('PC'),names_to = 'PC',values_to = 'loadings'),
                       by='variable', all=T, suffixes = c(".4t2019",".3t2019")) 
comparo_grupo = comparo_grupo[comparo_grupo$PC.3t2019==comparo_grupo$PC.4t2019,]
comparo_grupo = comparo_grupo%>%select(!'PC.3t2019')
comparo_grupo = comparo_grupo %>%
  pivot_longer(cols=contains('loadings'), names_to = 'Modelo', values_to = 'loadings',
               names_prefix ="loadings.")
names(comparo_grupo)[names(comparo_grupo)=="PC.4t2019"] = 'PC'

comparo_grupo %>%
  filter(PC%in%c('PC1','PC2'))%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,abs(loadings)), y= 0, 
                   xend = reorder(variable,abs(loadings)), yend = loadings,
                   col=Modelo), alpha=0.5)+
  geom_point(aes(reorder(variable,abs(loadings)),loadings, col=Modelo), alpha=0.5)+
  labs(x='Variable (ordenadas por valor absoluto de pesos en primeros dos CPs)',
       y='Pesos', title = 'SPC ortog.')+
  scale_x_discrete(breaks = NULL)+
  coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
ggsave(device ='png',filename='tiempoSPCortog.png', width = 5, height = 4)

comparo_grupo %>%
  filter(PC%in%c('PC1','PC2'))%>%
  ggplot()+
  geom_hline(yintercept=0)+
  geom_segment(aes(x =  reorder(variable,abs(loadings)), y= 0, 
                   xend = reorder(variable,abs(loadings)), yend = loadings,
                   col=Modelo), alpha=0.5)+
  geom_point(aes(reorder(variable,abs(loadings)),loadings, col=Modelo), alpha=0.5)+
  labs(x='Variable ( of loadings in the first PCs)',
       y='Loadings', title = 'SPC orthog.')+
  scale_x_discrete(breaks = NULL)+
  coord_flip()+
  facet_grid(cols = vars(PC))+
  theme_minimal()
ggsave(device ='png',filename='tiempoSPCortog-ENG.png', width = 5, height = 4)




# ÍNDICE UNIDIMENSIONAL ----
# SPC orth 
dim(eph_scaled)
dim(spc_fit_orth$v)
dim(spc_fit_orth_3T2019$v)
sum(abs(spc_fit_orth$v[,1]))
sum(abs(spc_fit_orth_3T2019$v[,1]))

real =  eph_scaled %*% as.matrix(loadings_spc_orth[c('PC1','PC2','PC3')]) # valores reales para 4T 2019
predicho = eph_scaled %*% as.matrix(loadings_spc_orth_3T2019[c('PC1','PC2','PC3')]) # loadings del 3T

predicho_real = cbind(predicho[,1],real[,1])
colnames(predicho_real) = c('predicho','real')
rm(predicho, real)

predicho_real = as.data.frame(predicho_real)


# Superposición histogramas
predicho_real%>%
  ggplot()+
  geom_histogram(aes(predicho, fill='Predicho'), alpha=0.3)+
  geom_histogram(aes(real, fill='Real'), alpha=0.3)+
  theme_classic()+
  labs(x='Valores PC1', y='Cant. individuos.',
       title = 'SPC ortog.')
ggsave(device ='png',filename='histortog.png', width = 5, height = 3)


predicho_real%>%
  ggplot()+
  geom_histogram(aes(predicho, fill='Predicho'), alpha=0.3)+
  geom_histogram(aes(real, fill='Real'), alpha=0.3)+
  theme_classic()+
  labs(x='PC1 values', y='Number of people',
       title = 'SPC orthog.')
ggsave(device ='png',filename='histortog-eng.png', width = 5, height = 3)


ks.test(predicho_real$predicho, predicho_real$real)


predicho_real$mse = (predicho_real[,1] - predicho_real[,2])**2
format(mean(predicho_real$mse), scientific=F)
mean(predicho_real$mse>0)
plot(predicho_real$real, predicho_real$mse)

# MSE PCA
real_pca = eph_scaled %*% pca$rotation[,1] # valores reales para 4T 2019
predicho_pca = eph_scaled %*% (pca_3T2019$rotation[,1]*(-1))
mean((predicho_pca - real_pca)**2)
predicho_real%>%
  ggplot()+
  geom_histogram(aes(predicho_pca, fill='Predicho'), alpha=0.3)+
  geom_histogram(aes(real_pca, fill='Real'), alpha=0.3)+
  theme_classic()+
  labs(x='Valores PC1', y='Cant. individuos',
       title = 'PCA')
ggsave(device ='png',filename='histpca.png', width = 5, height = 3)

# Quantiles
sum(abs(loadings_spc_orth$PC1))
sum(abs(loadings_spc_orth_3T2019$PC1))
min(predicho_real$real)

# Cuartiles
predicho_real$q = 4
predicho_real$q[predicho_real$real < quantile(predicho_real$real)[4]] = 3
predicho_real$q[predicho_real$real < quantile(predicho_real$real)[3]] = 2
predicho_real$q[predicho_real$real < quantile(predicho_real$real)[2]] = 1

predicho_real$qpred = 4
predicho_real$qpred[predicho_real$predicho < quantile(predicho_real$predicho)[4]] = 3
predicho_real$qpred[predicho_real$predicho < quantile(predicho_real$predicho)[3]] = 2
predicho_real$qpred[predicho_real$predicho < quantile(predicho_real$predicho)[2]] = 1

unique(predicho_real$qpred)
unique(predicho_real$q)

predicho_real$mseq = (predicho_real$q - predicho_real$qpred)**2
mean(predicho_real$mseq>0)*100
mean(predicho_real$mseq)

plot(predicho_real$real, predicho_real$predicho,
     col= 1*(predicho_real$mseq>0)+1 ,
     xlab='Reales', ylab='Predichos',main='Cuartiles')

# Deciles
deciles = quantile(predicho_real$real, probs = seq(0.1, 1, by=0.1))
decilesp = quantile(predicho_real$predicho, probs = seq(0.1, 1, by=0.1))
predicho_real$d = 10
predicho_real$dpred = 10
for (i in 9:1){
  predicho_real$d[predicho_real$real < deciles[i]] = i
  predicho_real$dpred[predicho_real$predicho < decilesp[i]] = i
}
unique(predicho_real$d)
unique(predicho_real$dpred)

predicho_real$msed = (predicho_real$d - predicho_real$dpred)**2
mean(predicho_real$msed>0)*100
mean(predicho_real$msed)
unique(predicho_real[predicho_real$msed>0, c('d','dpred')]$d)
rm(deciles, decilesp)

plot(predicho_real$real, predicho_real$predicho,
     col= 1*(predicho_real$msed>0)+1,
     xlab='Reales', ylab='Predichos',main='Deciles')

# Percentiles
percentiles= quantile(predicho_real$real, probs = seq(0.01, 1, by=0.01))
percentilesp = quantile(predicho_real$predicho, probs = seq(0.01, 1, by=0.01))
predicho_real$p = 100
predicho_real$ppred = 100
for (i in 99:1){
  predicho_real$p[predicho_real$real < percentiles[i]] = i
  predicho_real$ppred[predicho_real$predicho < percentilesp[i]] = i
}
unique(predicho_real$p)
unique(predicho_real$ppred)

predicho_real$msep = (predicho_real$p - predicho_real$ppred)**2
mean(predicho_real$msep>0)
mean(predicho_real$msep)
min(predicho_real$p - predicho_real$ppred)
max(predicho_real$p - predicho_real$ppred)





# ÍNDICE MULTIDIMENSIONAL ----

real =  eph_scaled %*% as.matrix(loadings_spc_orth_esparso[c('PC1','PC2','PC3')]) # valores reales para 4T 2019
predicho = eph_scaled %*% as.matrix(loadings_spc_orth_esparso_3T2019[c('PC1','PC2','PC3')]) # loadings del 3T
#pairs(real)
#pairs(predicho)



### Histogramas
ggplot()+
  geom_histogram(aes(predicho[,1], fill='Predicho'), alpha=0.3)+
  geom_histogram(aes(real[,1], fill='Real'), alpha=0.3)+
  theme_classic()+
  labs(x='Valores CP1', y='Cant. individuos')
ggsave(device ='png',filename='hist1.png', width = 4, height = 3)

ggplot()+
  geom_histogram(aes(predicho[,2], fill='Predicho'), alpha=0.3)+
  geom_histogram(aes(real[,2], fill='Real'), alpha=0.3)+
  theme_classic()+
  labs(x='Valores CP2', y='Cant. individuos')
ggsave(device ='png',filename='hist2.png', width = 4, height = 3)


ggplot()+
  geom_histogram(aes(predicho[,3], fill='Predicho'), alpha=0.3)+
  geom_histogram(aes(real[,3], fill='Real'), alpha=0.3)+
  theme_classic()+
  labs(x='Valores CP3', y='Cant.individuos')
ggsave(device ='png',filename='hist3.png', width = 4, height = 3)

ks.test(predicho[,1],real[,1])
ks.test(predicho[,2],real[,2])
ks.test(predicho[,3],real[,3])


# MSE
mean((abs(predicho)-abs(real))**2)



# Elimino duplicates
mean(duplicated(real))
mean(duplicated(predicho))
real = as.data.frame(real)
predicho = as.data.frame(predicho)
real$dup = duplicated(real)
nrow(real)-sum(real$dup)
nrow(real)
predicho$dup = duplicated(predicho)
mean(real$dup)
mean(predicho$dup)
real$id = rownames(real)
predicho$id = rownames(predicho)
predicho=predicho[!real$dup,]# Me quedo solo con los no duplicados en real
real = real[!real$dup,]
mean(real$id %in% predicho$id)
mean(predicho$id %in% real$id)
names(real)



tukeymed.r = med(real[,c(1,2,3)], approx = T)$median # tukey Median
spatialmed.r = med(real[,c(1,2,3)], method = 'Spatial')$median 
tukeymed.p = med(predicho[,c(1,2,3)], approx = T)$median
spatialmed.p = med(predicho[,c(1,2,3)], method = 'Spatial',maxit = 1)$median  

## Tukey depth -----
prof.r = apply(real[,c(1,2,3)],1, FUN = function(x) depth(x, real[,c(1,2,3)], approx = T))
saveRDS(prof.r, file='profr.rds')

prof.p = apply(predicho[,c(1,2,3)],1, FUN = function(x) depth(x, predicho[,c(1,2,3)], approx = T))
saveRDS(prof.p, file='profp.rds')

## Load Tukey depth -----
prof.r <- readRDS('profr.rds')
prof.p <- readRDS('profp.rds')


## Spatial/Tukey Median + Mahalanobis distance ----
mahalanobis.r <- mahalanobis(
  real[,c(1,2,3)],
  spatialmed.r,
  cov(real[,c(1,2,3)])
)

mahalanobis.p <- mahalanobis(
  predicho[,c(1,2,3)],
  spatialmed.p,
  cov(predicho[,c(1,2,3)])
)

## Medidas de error ----
# profundidades
profundidades = as.data.frame(cbind(prof.r, prof.p))
rm(prof.p, prof.r)
names(profundidades)

mean((profundidades$prof.p - profundidades$prof.r)**2) # MSE prof
mean((profundidades$prof.p - profundidades$prof.r)**2>0) # obviamente las distancias no son las mismas 

profundidades$qprof = 4
profundidades$qprof[profundidades$prof.r < quantile(profundidades$prof.r)[4]] = 3
profundidades$qprof[profundidades$prof.r < quantile(profundidades$prof.r)[3]] = 2
profundidades$qprof[profundidades$prof.r < quantile(profundidades$prof.r)[2]] = 1
profundidades$qprofpred = 4
profundidades$qprofpred[profundidades$prof.p < quantile(profundidades$prof.p)[4]] = 3
profundidades$qprofpred[profundidades$prof.p < quantile(profundidades$prof.p)[3]] = 2
profundidades$qprofpred[profundidades$prof.p < quantile(profundidades$prof.p)[2]] = 1
profundidades$mseprof = (profundidades$qprof - profundidades$qprofpred)**2
mean(profundidades$mseprof>0)
mean(profundidades$mseprof)
max(profundidades$qprof-profundidades$qprofpred)
profundidades$misclas = 0
profundidades$misclas[profundidades$qprof!= profundidades$qprofpred] = 1
mean(profundidades$misclas)


mahalanobis = as.data.frame(cbind(mahalanobis.r, mahalanobis.p))
rm(mahalanobis.p, mahalanobis.r)
names(mahalanobis)

mean((mahalanobis$mahalanobis.p - mahalanobis$mahalanobis.r)**2) # MSE mahalanobis
mean((mahalanobis$mahalanobis.p - mahalanobis$mahalanobis.r)**2>0) # obviamente las distancias no son las mismas 

mahalanobis$qprof = 4
mahalanobis$qprof[mahalanobis$mahalanobis.r < quantile(mahalanobis$mahalanobis.r)[4]] = 3
mahalanobis$qprof[mahalanobis$mahalanobis.r < quantile(mahalanobis$mahalanobis.r)[3]] = 2
mahalanobis$qprof[mahalanobis$mahalanobis.r < quantile(mahalanobis$mahalanobis.r)[2]] = 1
mahalanobis$qprofpred = 4
mahalanobis$qprofpred[mahalanobis$mahalanobis.p < quantile(mahalanobis$mahalanobis.p)[4]] = 3
mahalanobis$qprofpred[mahalanobis$mahalanobis.p < quantile(mahalanobis$mahalanobis.p)[3]] = 2
mahalanobis$qprofpred[mahalanobis$mahalanobis.p < quantile(mahalanobis$mahalanobis.p)[2]] = 1
mahalanobis$mseprof = (mahalanobis$qprof - mahalanobis$qprofpred)**2
mean(mahalanobis$mseprof>0)
mean(mahalanobis$mseprof)
max(mahalanobis$qprof-mahalanobis$qprofpred)
mahalanobis$misclas = 0
mahalanobis$misclas[mahalanobis$qprof!= mahalanobis$qprofpred] = 1
mean(mahalanobis$misclas)

names(profundidades)
names(mahalanobis)

mean(mahalanobis$misclas + profundidades$misclas ==2)*100


## PLOT ----

myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}



set.seed(9)
submuestra = sample(nrow(real),1000)
cols <- myColorRamp(c("blue", "red"), profundidades$prof.r[submuestra])
scatterplot3d(x = real[submuestra,1], y = real[submuestra,2], z = real[submuestra,3], 
              color = cols, pch=20,
              xlab='PC1', ylab='PC2', zlab='PC3', main = 'Tukey depth')

cols <- myColorRamp(c("red", "blue"), mahalanobis$mahalanobis.r[submuestra])
scatterplot3d(x = real[submuestra,1], y = real[submuestra,2], z = real[submuestra,3], 
              color = cols, pch=20,
              xlab='PC1', ylab='PC2', zlab='PC3', main = 'Mahalanobis')



scatterplot3d(x = real[submuestra,1], y = real[submuestra,2],
              z = real[submuestra,3], 
              color = profundidades$qprof[submuestra], pch=20,
              xlab='CP 1', ylab='CP 2', zlab='CP 3',
              main = 'Cuantiles profundidad de Tukey', angle = 145)
scatterplot3d(x = real[submuestra,1], y = real[submuestra,2],
              z = real[submuestra,3], 
              color = mahalanobis$qprof[submuestra]*(-1)+5, pch=20,
              xlab='CP 1', ylab='CP 2', zlab='CP 3',
              main = 'Cuantiles distancia de Mahalanobis', angle = 145)


# plot3d(x = real[,1], y = real[,2], z = real[,3], col = cols, pch=20,
#        xlab='PC1', ylab='PC2', zlab='PC3',aspect=T)
# 

#### k-MEANS -------------
real =  eph_scaled %*% as.matrix(loadings_spc_orth_esparso[c('PC1','PC2','PC3')]) # valores reales para 4T 2019
predicho = eph_scaled %*% as.matrix(loadings_spc_orth_esparso_3T2019[c('PC1','PC2','PC3')]) # loadings del 3T
set.seed(11)
k_means.r = kmeans(real[,c(1,2,3)], 3)
k_means.p = kmeans(predicho[,c(1,2,3)], 3)
k_means.p$cluster = k_means.p$cluster*(-1)+4
k_means.p$cluster[k_means.p$cluster==3] = 4
k_means.p$cluster[k_means.p$cluster==2] = 3
k_means.p$cluster[k_means.p$cluster==4] = 2
table(k_means.r$cluster, k_means.p$cluster)

set.seed(9)
submuestra = sample(nrow(real),1000)
scatterplot3d(x = real[submuestra,1], y = real[submuestra,2],
              z = real[submuestra,3], 
              color = k_means.r$cluster[submuestra], pch=20,
              xlab='CP 1', ylab='CP 2', zlab='CP 3',
              main = 'Clusters k-medias', angle = 145)
scatterplot3d(x = real[submuestra,1], y = real[submuestra,2],
              z = real[submuestra,3], 
              color = k_means.p$cluster[submuestra], pch=20,
              xlab='PC1', ylab='PC2', zlab='PC3',
              main = 'k-means predicho')

mat.conf <- apply(as.matrix.noquote(table(k_means.r$cluster, k_means.p$cluster)),2,as.numeric)
(mat.conf[2,1] + mat.conf[1,2] + mat.conf[2,3] + mat.conf[3,2] + mat.conf[3,1] + mat.conf[1,3]) / sum(mat.conf) * 100
