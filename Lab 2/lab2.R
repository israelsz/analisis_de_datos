if(!require(cluster)){
  install.packages("cluster",dependencies = TRUE)
  require(cluster)
}

if(!require(factoextra)){
  install.packages("factoextra",dependencies = TRUE)
  require(factoextra)
}

if(!require(FactoMineR)){
  install.packages("FactoMineR",dependencies = TRUE)
  require(FactoMineR)
}

if(!require(klaR)){
  install.packages("klaR",dependencies = TRUE)
  require(klaR)
}

if(!require(kamila)){
  install.packages("kamila",dependencies = TRUE)
  require(kamila)
}

if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}

if(!require(clustMixType)){
  install.packages("clustMixType",dependencies = TRUE)
  require(clustMixType)
}

if(!require(Rtsne)){
  install.packages("Rtsne",dependencies = TRUE)
  require(Rtsne)
}


# Se carga el archivo de datos CSV
datos <- read.csv(file.choose(new = FALSE), header = FALSE, sep=",")

#Se agregan los nombres de header
colnames(datos) <- c("age", "sex", "on thyroxine", "query on thyroxine",
                     "on antithyroid medication", "sick", "pregnant",
                     "thyroid surgery", "I131 treatment", "query hypothyroid",
                     "query hyperthyroid", "lithium", "goitre", "tumor",
                     "hypopituitary", "psych", "TSH measured", "TSH",
                     "T3 measured", "T3", "TT4 measured", "TT4",
                     "T4U measured", "T4U", "FTI measured", "FTI",
                     "TBG measured", "TBG", "referral source", "classification")

#Se elimina el id del paciente de la columna de classificación
aux<- c()
for(i in datos$classification){
  aux <- c(aux, strsplit(i, ".", fixed = TRUE)[[1]][1])
}
# Se agrega la clasificación como una columna aparte
datos$classification <- aux


############################################################################
#                   Pre-procesamiento de datos
############################################################################

# Al hacer un análisis exploratorio inicial de los datos es posible observar 
# que se presentan datos que no tienen mediciones para las hormonas relevantes 
# al problema, no presentan información respecto a su edad o sexo. 
# Estas variables incompletas serán eliminadas debido a no aportar la 
#información completa que podría afectar en el análisis posterior.


# Se filtran los datos, seleccionando las columnas en las cuales se encuentren
# la edad, sexo y las distintas mediciones registradas
datosFiltrados <- datos[(datos$age!="?" & datos$sex != "?" & 
                           datos[["TSH measured"]] =="t" & 
                           datos[["T3 measured"]] == "t" & 
                           datos[["TT4 measured"]] == "t" & 
                           datos[["T4U measured"]] == "t" & 
                           datos[["FTI measured"]] == "t"),]

# Se convierten los datos continuos a numericos
datosFiltrados$age <- as.numeric(datosFiltrados$age)
datosFiltrados$TSH <- as.numeric(datosFiltrados$TSH)
datosFiltrados$TT4 <- as.numeric(datosFiltrados$TT4)
datosFiltrados$T4U <- as.numeric(datosFiltrados$T4U)
datosFiltrados$FTI <- as.numeric(datosFiltrados$FTI)
datosFiltrados$T3 <- as.numeric(datosFiltrados$T3)

# Criterios de filtración de datos atípicos:

# Al observar una muestra con edad de 455, 
# se decide filtrar por edades menores a 100
datosFiltrados <- datosFiltrados[(datosFiltrados$age<100),]

# Debido a que solo se tiene una muestra bajo la clase de hipotiroidismo 
# secundario, se decide eliminarla debido a que no se podrá obtener
# información de ella.
datosFiltrados <- datosFiltrados %>% 
  filter(classification != "secondary hypothyroid")

datosFiltrados$classification <- as.factor(datosFiltrados$classification)

# Se convierten todas variables no numericas a factores
datosFiltrados <- datosFiltrados %>% mutate_if(is.character,as.factor)


# Change box plot colors by groups
p<-ggplot(datosFiltrados, aes(x=classification, y=TSH, fill=classification)) +
  geom_boxplot()
p

# Finalmente se decide por eliminar las columnas “measured” que indican si fue
# tomada la medición de la hormona que indica, debido a que con el filtro actual
# de datos todas corresponden al valor de true, por lo que esas columnas no
# aportan información, además se elimina la columna de TBG measured y TBG 
# debido a que para ninguna muestra fue medida, por lo que
# tampoco aportan información.

datosFiltrados <- datosFiltrados %>% select(-c("TSH measured", 
                                               "T3 measured", 
                                               "TT4 measured", 
                                               "T4U measured",
                                               "FTI measured", 
                                               "TBG measured",
                                               "TBG",
                                               "referral source"))

#datosCluster <- datosFiltrados
#datosCluster <- datosFiltrados %>% select(-c("classification"))
datosClusterScaled <- datosFiltrados
datosClusterScaled$age <- scale(datosClusterScaled$age)[,1]
datosClusterScaled$TSH <- scale(datosClusterScaled$TSH)[,1]
datosClusterScaled$TT4 <- scale(datosClusterScaled$TT4)[,1]
datosClusterScaled$T3 <- scale(datosClusterScaled$T3)[,1]
datosClusterScaled$T4U <- scale(datosClusterScaled$T4U)[,1]
datosClusterScaled$FTI <- scale(datosClusterScaled$FTI)[,1]

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
distanciaMediod <- daisy(datosClusterScaled, metric = "gower")

#Cluster con k = 3
mediod_clusterk3 <- pam(distanciaMediod, diss = TRUE, k = 3)

#Grafico
tsne_c <- Rtsne(distanciaMediod, is_distance = TRUE)
graficoPAMk3 <- ggplot(data.frame(tsne_c$Y), 
                       aes(x = X1, y = X2)) + 
        labs(x = "Dim1", y = "Dim2", title = "Cluster PAM k=3") + 
        geom_point(color = factor(mediod_clusterk3$clustering))

graficoPAMk3


table(mediod_clusterk3$clustering, datosClusterScaled$classification)


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
