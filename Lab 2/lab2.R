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

#################################
# Numericas

###########
# K means #
###########

# Cálculo de los k óptimos

#Metodo del codo
resCodo <- fviz_nbclust(datosClusterNumeric, kmeans, method='wss')
plot(resCodo)
kCodo <- 5
# K óptimo = 5

#Método de la Silueta
resSil <- fviz_nbclust(datosClusterNumeric, kmeans, method = "silhouette")
plot(resSil)
kSilueta <- 2
#K óptimo = 2


# Obtención de clustering

# K del Método del codo
set.seed(123) 
kmCodo <- kmeans(datosClusterNumeric, 
                 centers = kCodo, iter.max = 250, nstart =25) 
# Gráfico
fviz_cluster(kmCodo, data = datosClusterNumeric, 
             repel = TRUE,
             main = "Clustering de datos numéricos con k = 5", 
             ggtheme = theme_classic())
# Tabla
table(kmCodo$cluster, clasificacion)


# K del Método de la silueta
set.seed(123) 
kmSilueta <- kmeans(datosClusterNumeric, 
                 centers = kSilueta, iter.max = 250, nstart =25) 
# Gráfico
fviz_cluster(kmSilueta, data = datosClusterNumeric, 
             repel = TRUE,
             main = "Clustering con datos numéricos k = 2", 
             ggtheme = theme_classic())
# Tabla
table(kmSilueta$cluster, clasificacion)


################################################
# One hot encoding

###########
# K means #
###########

# Cálculo de los k óptimos

#Metodo del codo
resCodo <- fviz_nbclust(datosClusterOHE, kmeans, method='wss')
plot(resCodo)
kCodo <- 5
# K óptimo = 5

#Método de la Silueta
resSil <- fviz_nbclust(datosClusterOHE, kmeans, method = "silhouette")
plot(resSil)
kSilueta <- 2
#K óptimo = 2


# Obtención de clustering

# K del Método del codo
set.seed(123) 
kmCodo <- kmeans(datosClusterOHE, 
                 centers = kCodo, iter.max = 250, nstart =25) 
# Gráfico
fviz_cluster(kmCodo, data = datosClusterOHE, 
             repel = TRUE,
             main = "Clustering de datos OHE con k = 5", 
             ggtheme = theme_classic())
# Tabla
table(kmCodo$cluster, clasificacion)


# K del Método de la silueta
set.seed(123) 
kmSilueta <- kmeans(datosClusterOHE, 
                    centers = kSilueta, iter.max = 250, nstart =25) 
# Gráfico
fviz_cluster(kmSilueta, data = datosClusterOHE, 
             repel = TRUE,
             main = "Clustering de datos OHE con k = 2", 
             ggtheme = theme_classic())
# Tabla
table(kmSilueta$cluster, clasificacion)

############################################
# Mixto

##############
# K-mediodes #
##############
# Cálculo de matriz de distancia
distanciaMediod <- daisy(datosClusterMix, metric = "gower")
distanciaMediodMatrix <- as.matrix(distanciaMediod)

# Cálculo de los k óptimos

# Método del codo
codoPlot <- fviz_nbclust(distanciaMediodMatrix, pam, method = "wss")
plot(codoPlot)
kCodo <- 4
# K óptimo = 4

# Método de la silueta
siluetaPlot <- fviz_nbclust(distanciaMediodMatrix, pam, method = "silhouette")
plot(siluetaPlot)
kSilueta <- 6

# Obtención de clustering

# K del Método del codo
set.seed(123) 
pamCodo <- pam(distanciaMediodMatrix, k = kCodo)
#Gráfico
graficoClusterCodo <- fviz_cluster(pamCodo,
             ellipse.type = "norm", 
             show.clust.cent = TRUE,star.plot = TRUE)
plot(graficoClusterCodo)
#Tabla
table(pamCodo$clustering, clasificacion)


# K del Método de la silueta
set.seed(123) 
pamSilueta <- pam(distanciaMediodMatrix, k = kSilueta)
#Gráfico
graficoClusterSilueta <-fviz_cluster(pamSilueta, 
             ellipse.type = "norm", 
             show.clust.cent = TRUE,star.plot = TRUE)
plot(graficoClusterSilueta)
#Tabla
table(pamSilueta$clustering, clasificacion)



