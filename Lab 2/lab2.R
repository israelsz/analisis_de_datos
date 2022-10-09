if(!require(cluster)){
  install.packages("cluster",dependencies = TRUE)
  require(cluster)
}

if(!require(factoextra)){
  install.packages("factoextra",dependencies = TRUE)
  require(factoextra)
}

if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}


if(!require(mice)){
  install.packages("mice",dependencies = TRUE)
  require(mice)
}

if(!require(VIM)){
  install.packages("VIM",dependencies = TRUE)
  require(VIM)
}

# Se carga el archivo de datos CSV
datos <- read.csv(file.choose(new = FALSE), header = FALSE, sep=",")

#Se agregan los nombres de header
colnames(datos) <- c("age", "sex", "on_thyroxine", "query_on_thyroxine",
                     "on_antithyroid_medication", "sick", "pregnant",
                     "thyroid_surgery", "I131_treatment", "query_hypothyroid",
                     "query_hyperthyroid", "lithium", "goitre", "tumor",
                     "hypopituitary", "psych", "TSH_measured", "TSH",
                     "T3_measured", "T3", "TT4_measured", "TT4",
                     "T4U_measured", "T4U", "FTI_measured", "FTI",
                     "TBG_measured", "TBG", "referral_source", "classification")

#Se elimina el id del paciente de la columna de classificación
aux<- c()
for(i in datos$classification){
  aux <- c(aux, strsplit(i, ".", fixed = TRUE)[[1]][1])
}
# Se agrega la clasificación como una columna aparte
datos$classification <- aux

# Se reemplazas los ? por NA's
datos <- datos %>% dplyr::na_if("?")

############################################################################
#                   Pre-procesamiento de datos
############################################################################

# Al hacer un análisis exploratorio inicial de los datos es posible observar 
# que se presentan datos que no tienen mediciones para las hormonas relevantes 
# al problema, no presentan información respecto a su edad o sexo. 
# Estas variables incompletas serán eliminadas debido a no aportar la 
#información completa que podría afectar en el análisis posterior.


# Se convierten los datos continuos a numericos
datos$age <- as.numeric(datos$age)
datos$TSH <- as.numeric(datos$TSH)
datos$TT4 <- as.numeric(datos$TT4)
datos$T4U <- as.numeric(datos$T4U)
datos$FTI <- as.numeric(datos$FTI)
datos$T3 <- as.numeric(datos$T3)


datosFiltrados <- datos %>% select(-c("TSH_measured", 
                             "T3_measured", 
                             "TT4_measured", 
                             "T4U_measured",
                             "FTI_measured", 
                             "TBG_measured",
                             "TBG",
                             "referral_source"))

# Grafico de data faltante
aggr_plot <- aggr(datosFiltrados, 
                  col=c('cadetblue1','red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels=names(datosFiltrados), 
                  cex.axis=.90,
                  ylab=c("Histograma de data faltante","Patrón"),
                  gap = 0.5,
                  oma = c(10,5,5,2),
                  cex.numbers = 0.70)

# Por regla, las columnas con 5% menos de data faltante, se borran
# sus observaciones con NA, las columnas con mas de 5% de data faltante se
# recomienda su imputación

#Se eliminan las observaciones que tienen menos de 5% de los datos
# en este caso, corresponden a las variables sex y age
datosFiltrados <- datosFiltrados[!is.na(datosFiltrados$age), ]
datosFiltrados <- datosFiltrados[!is.na(datosFiltrados$sex), ]

# Ademas se decide el limitar la edad a 100, debido a la observación de un dato
# atipico con edad 455
datosFiltrados <- datosFiltrados[(datosFiltrados$age<100),]

# Imputación de datos:

# Se hace imputación de los datos usando predictive mean matching

datosNA <- datosFiltrados[ ,c(1,17:21)]
datosNA <- mice(datosNA, 
            method = "pmm",
            m = 5, 
            seed = 754)

# Se guardan los datos
datosNA <- complete(datosNA)

# Se incorporan las columnas inputadas al dataset original
datosFiltrados$age <- datosNA$age
datosFiltrados$TSH <- datosNA$TSH
datosFiltrados$T3 <- datosNA$T3
datosFiltrados$TT4 <- datosNA$TT4
datosFiltrados$T4U <- datosNA$T4U
datosFiltrados$FTI <- datosNA$FTI

# Se almacena la clasificacion
clasificacion <- datosFiltrados$classification
datosFiltrados$classification <- NULL
# Se escalan los datos

datosClusterScaled <- datosFiltrados
datosClusterScaled$age <- scale(datosClusterScaled$age)[,1]
datosClusterScaled$TSH <- scale(datosClusterScaled$TSH)[,1]
datosClusterScaled$TT4 <- scale(datosClusterScaled$TT4)[,1]
datosClusterScaled$T3 <- scale(datosClusterScaled$T3)[,1]
datosClusterScaled$T4U <- scale(datosClusterScaled$T4U)[,1]
datosClusterScaled$FTI <- scale(datosClusterScaled$FTI)[,1]

##########################################################
# Datasets utilizados
##########################################################

# One hot encoding
datosClusterOHE <- datosClusterScaled

# Se convierten las variables no numericas a 1,0
datosClusterOHE[datosClusterOHE == "t" | 
                        datosClusterOHE == "M"] <- 1
datosClusterOHE[datosClusterOHE == "f" | 
                        datosClusterOHE == "F"] <- 0

# Se convierten las variables a numericas
datosClusterOHE <- datosClusterOHE %>%
  mutate_if(is.character,as.numeric)


# Mixto
datosClusterMix <- datosClusterScaled
# Se convierten las variables a factores
datosClusterMix <- datosClusterMix %>%
  mutate_if(is.character,as.factor)

# Solo numéricas
datosClusterNumeric <- datosClusterScaled %>% select(c(1,17:21))


##############################################################################
#                     Clustering
##############################################################################

##########################
# Metodo con K-prototype
#########################

####################
# Obtencion K optimo
####################
# Metodo del Codo
wss<-vector()
for (i in 2:15){
set.seed(155)
wss[i] <- sum(kproto(datosClusterScaled, i, verbose = FALSE)$withinss)
}

gProtoCodo<- plot(1:15, wss, type="b", xlab="Numero de Clusters",
     ylab="Within groups sum of squares",
     main="K optimo con el método del Codo",
     pch=20, cex=2)

# El metodo del codo indica que optimo seria k = 5

# Metodo de la silueta
siluetaProto <- vector()
for(i in 2:15){
  set.seed(86)
  kpres <- kproto(datosClusterScaled, k = i,na.rm=FALSE, verbose = FALSE)
  valor_sil<-validation_kproto(method = "silhouette", object=kpres)
  siluetaProto[i] <- valor_sil
}
gProtoSil <- plot(1:15, siluetaProto, type = "b", ylab = "Silhouette", 
     xlab = "Numero de Clusters", 
     main = "K optimo con método de la Silueta")
# El metodo de la silueta arroja un optimo de 3

####################
# Generación de Clusters
####################
# Cluster con k = 3
set.seed(86)
protoClusterk3 <- kproto(datosClusterScaled, 3, verbose = FALSE, nstart = 25)


# Cluster con k = 5
set.seed(86)
protoClusterk5 <- kproto(datosClusterScaled, 5, verbose = FALSE, nstart = 25)

############################
# Resultados e interpretación
############################

summary(protoClusterk3)
summary(protoClusterk5)

table(protoClusterk3$cluster, datosFiltrados$classification)
table(protoClusterk4$cluster, datosFiltrados$classification)

#clprofiles(protoClusterk3, datosClusterScaled) #graficos
#clprofiles(protoClusterk5, datosClusterScaled) #graficos


##########################
# Metodo con K-mediodes PAM K=3
#########################

# Calculo de la distancia de gower
set.seed(4) 

distanciaMediod <- daisy(datosClusterScaled, metric = "gower")

#Cluster con k = 3
mediod_clusterk3 <- pam(distanciaMediod, diss = TRUE, k = 3)

#Grafico
#tsne_c <- Rtsne(distanciaMediod, is_distance = TRUE)
#graficoPAMk3 <- ggplot(data.frame(tsne_c$Y), 
#                       aes(x = X1, y = X2)) + 
#        labs(x = "Dim1", y = "Dim2", title = "Cluster PAM k=3") + 
#        geom_point(color = factor(mediod_clusterk3$clustering))

#graficoPAMk3


tablon <- table(mediod_clusterk3$clustering, datosClusterScaled$classification)


#################
# Jerarquico
##################
res.agnes <- agnes(x = datosClusterScaled[,c(17,21)], #data frame
                   stand = FALSE, #Standardize the Data 
                   metric = "euclidiean", # Metric for Distance
                   method = "ward") #Linkage Method 
 

fviz_dend(res.agnes, cex = 0.6, k = 3, type = "circular", rect = TRUE)

#############
# K means
############
set.seed(123) 
km.res <- kmeans(datosFiltrados[c(1,17:21)], 
                 centers = 3, iter.max = 250, nstart =25) 

# grafico
fviz_cluster(km.res, data = datosClusterScaled[c(1,17:21)], 
             stand = FALSE, geom = c("point", "text"),
             repel = TRUE, ellipse.type = "confidence", ellipse.level = 0.95,
             main = "Swiss Cluster Plot", ggtheme = theme_classic())

table(km.res$clustering, datosClusterScaled$classification)

############
# Kamila
############
datosCategoricos <- datosClusterScaled[,c(2:16,22)]
datosNumericos <- datosClusterScaled[,c(1,17:21)]
clusterKamila <- kamila(datosNumericos, datosCategoricos, 3, 30)


#################
# Jerarquico 2
##################
# Dissimilarity matrix
d <- dist(datosClusterScaled[,c(17,21)], method = "euclidean")
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

#Formar cluster
clust <- cutree(hc1, k = 3)
fviz_cluster(list(data = datosClusterScaled[,c(17,21)], cluster = clust))  ## from ‘factoextra’ package 


pltree(hc1, hang=-1, cex = 0.6)
rect.hclust(hc1, k = 3, border = 2:10)

#OTRO CLUSTER
#Ward’s method gets us the highest agglomerative coefficient. Let us look at its dendogram.
hc3 <- agnes(datosClusterScaled[,c(17,21)], method = "ward")

# Dendograma con clases
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
rect.hclust(hc3, k = 3, border = 2:10) 

clust3 <- cutree(hc3, k = 3)
# Grafico bonito
fviz_cluster(list(data = datosClusterScaled[,c(17,21)], cluster = clust3))  ## from ‘factoextra’ package 

datosClusterScaled <- mutate(datosClusterScaled, cluster = clust3)
count(datosClusterScaled, cluster)

## Otro cluster mas
# Finding distance matrix
distance_mat <- dist(datosClusterScaled[,c(17,21)], method = 'euclidean')
#distance_mat

# Fitting Hierarchical clustering Model
# to training dataset
set.seed(240)  # Setting seed
Hierar_cl <- hclust(distance_mat, method = "average")
#Hierar_cl

# Plotting dendrogram
plot(Hierar_cl)

# Choosing no. of clusters
# Cutting tree by height
abline(h = 110, col = "green")

# Cutting tree by no. of clusters
fit <- cutree(Hierar_cl, k = 3 )
fit

table(fit)
rect.hclust(Hierar_cl, k = 3, border = "green")
