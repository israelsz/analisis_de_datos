if(!require(lattice)){
  install.packages("lattice",dependencies = TRUE)
  require(lattice)
}

if(!require(ggplot2)){
  install.packages("ggplot2",dependencies = TRUE)
  require(ggplot2)
}

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}

if(!require(caret)){
  install.packages("caret",dependencies = TRUE)
  require(caret)
}

if(!require(leaps)){
  install.packages("leaps",dependencies = TRUE)
  require(leaps)
}

if(!require(rocc)){
  install.packages("rocc",dependencies = TRUE)
  require(rocc)
}

if(!require(ggcorrplot)){
  install.packages("ggcorrplot",dependencies = TRUE)
  require(ggcorrplot)
}

if(!require(DescTools)){
  install.packages("DescTools",dependencies = TRUE)
  require(DescTools)
}

if(!require(corrplot)){
  install.packages("corrplot",dependencies = TRUE)
  require(corrplot)
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
datos$classification <- aux

# Se filtran los datos, seleccionando las columnas en las cuales se encuentren
# la edad, sexo y las distintas mediciones registradas
datosFiltrados <- datos[(datos$age!="?" & datos$sex != "?" & 
                           datos[["TSH measured"]] =="t" & 
                           datos[["T3 measured"]] == "t" & 
                           datos[["TT4 measured"]] == "t" & 
                           datos[["T4U measured"]] == "t" & 
                           datos[["FTI measured"]] == "t"),]

# Se convierten los datos a numericos
datosFiltrados$age <- as.numeric(datosFiltrados$age)
datosFiltrados$TSH <- as.numeric(datosFiltrados$TSH)
datosFiltrados$TT4 <- as.numeric(datosFiltrados$TT4)
datosFiltrados$T4U <- as.numeric(datosFiltrados$T4U)
datosFiltrados$FTI <- as.numeric(datosFiltrados$FTI)
datosFiltrados$T3 <- as.numeric(datosFiltrados$T3)


# Se filtra ademas la edad, debido a una muestra que excede los 100 años de edad
datosFiltrados <- datosFiltrados[(datosFiltrados$age<100),]


# Se convierten todas variables no numericas a factores
#datosFiltrados <- datosFiltrados %>% mutate_if(is.character,as.factor)

datosFiltrados <- datosFiltrados %>% 
  filter(classification != "secondary hypothyroid")


# Graficos de puntos por clase:
# TSH vs Clase
graphTSH <- ggplot(datosFiltrados, 
                   aes(x=classification, y=TSH, fill = classification)) +
  geom_jitter(position= position_jitter(0.3), size=3, alpha=0.8, shape=21) + 
  theme(legend.position="none")

print(graphTSH)

# T3 vs Clase
graphT3 <- ggplot(datosFiltrados, 
                  aes(x=classification, y=T3, fill = classification)) + 
  geom_jitter(position= position_jitter(0.3), size=3, alpha=0.8, shape=21) +
  theme(legend.position="none")

print(graphT3)

# TT4 vs Clase
graphTT4 <- ggplot(datosFiltrados, 
                   aes(x=classification, y=TT4, fill = classification)) + 
  geom_jitter(position= position_jitter(0.3), size=3, alpha=0.8, shape=21) + 
  theme(legend.position="none")

print(graphTT4)

# T4U vs Clase
graphT4U <- ggplot(datosFiltrados, 
                   aes(x=classification, y=T4U, fill = classification)) + 
  geom_jitter(position= position_jitter(0.3), size=3, alpha=0.8, shape=21) +
  theme(legend.position="none")

print(graphT4U)

# FTI vs Clase
graphFTI <- ggplot(datosFiltrados,
                   aes(x=classification, y=FTI, fill = classification)) + 
  geom_jitter(position= position_jitter(0.3), size=3, alpha=0.8, shape=21) + 
  theme(legend.position="none")

print(graphFTI)

# Age vs Clase
graphAge <- ggplot(datosFiltrados, 
                   aes(x=classification, y=age, fill = classification)) + 
  geom_jitter(position= position_jitter(0.3), size=3, alpha=0.8, shape=21) + 
  theme(legend.position="none")

print(graphAge)


############################################################################
# Modelo de regresión logistica negativo v/s hipotiroidismo primario
############################################################################

# Creación de dataframe que contiene solo los datos de pacientes clasificados
# negativos y con hipotiroidismo primario
datosRlog <- datosFiltrados %>% 
  filter(classification == "negative" | classification == "primary hypothyroid")


# Descartar columnas inútiles
datosRlog <- datosRlog %>% select(-c("TSH measured", "T3 measured", 
                                     "TT4 measured", "T4U measured",
                                     "FTI measured", "TBG measured",
                                     "TBG"))

# Se convierten todas variables no numericas a factores
datosRlog <- datosRlog %>% mutate_if(is.character,as.factor)

set.seed(99)
# Se crea el conjunto de entrenamiento y el conjunto de prueba
trainIndex <- createDataPartition(datosRlog$classification, p = 0.7, 
                                  list = FALSE)
datosEntrenamiento <- datosRlog[trainIndex,]
datosPrueba  <- datosRlog[-trainIndex,]

set.seed(99)
# Se en entrenara el modelo con el metodo de validación de validación cruzada
# de 10 pliegues, con 3 repeticiones.
controlB <- trainControl(method="repeatedcv", number=10, repeats=3)
# Se entrena el modelo, usando la curva ROC como factor de optimización

modelo <- train(classification ~ ., 
               data=datosEntrenamiento, 
               method="rocc", 
               preProcess="scale", 
               trControl=controlB)

# Se estima la importancia de cada variable
importancia <- varImp(modelo, scale=FALSE)
# Printear el resumen de la importancia de cada variable
print(importancia)
# Graficar el resumen anterior de importancia por variable
plot(importancia)
# Imprime los predictores finales del mejor modelo
predictors(modelo)
# Resumen del modelo
print(modelo)
# Se puede ver gráficamente como varía la precisión según curva ROC de acuerdo
# a las variables
print(ggplot(modelo))
# Evaluar calidad predictiva del modelo con el conjunto de prueba.
predicciones <- predict(modelo, datosPrueba)
print(confusionMatrix(predicciones, datosPrueba$classification))


#########################################################################
# Modelo de regresión logistica negativo v/s hipotirodisimo compensado
#########################################################################

# Creación de dataframe que contiene solo los datos de pacientes clasificados
# negativos y con hipotiroidismo compensado
datosRlogcompensado <- datosFiltrados %>% 
  filter(classification == "negative" | 
           classification == "compensated hypothyroid")

# Descartar columnas inútiles
datosRlogcompensado <- datosRlogcompensado %>% select(-c("TSH measured", 
                                                         "T3 measured", 
                                     "TT4 measured", "T4U measured",
                                     "FTI measured", "TBG measured",
                                     "TBG"))

# Se convierten todas variables no numericas a factores
datosRlogcompensado <- datosRlogcompensado %>% mutate_if(is.character,as.factor)

set.seed(99)
# Se crea el conjunto de entrenamiento y el conjunto de prueba
trainIndexB <- createDataPartition(datosRlogcompensado$classification, p = 0.7, 
                                  list = FALSE)

datosEntrenamientoB <- datosRlogcompensado[trainIndexB,]
datosPruebaB  <- datosRlogcompensado[-trainIndexB,]

set.seed(99)
# Se entrenara el modelo con el metodo de validación de validación cruzada
# de 10 pliegues, con 3 repeticiones.
controlB <- trainControl(method="repeatedcv", number=10, repeats=3)
# Se entrena el modelo, usando la curva ROC como factor de optimización
modeloB <- train(classification ~ ., 
                data=datosEntrenamientoB, 
                method="rocc", 
                preProcess="scale", 
                trControl=controlB)

# Se estima la importancia de cada variable
importanciaB <- varImp(modeloB, scale=FALSE)
# Printear el resumen de la importancia de cada variable
print(importanciaB)
# Graficar el resumen anterior de importancia por variable
plot(importanciaB)
# Imprime los predictores finales del mejor modelo
predictors(modeloB)
# Resumen del modelo
print(modeloB)
# Se puede ver gráficamente como varía la precisión según curva ROC de acuerdo
# a las variables
print(ggplot(modeloB))

# Evaluar calidad predictiva del modelo con el conjunto de prueba.
prediccionesB <- predict(modeloB, datosPruebaB)
print(confusionMatrix(prediccionesB, datosPruebaB$classification))

########################################################################
# Modelo de regresión logistica todas las clases 
# (Multinominal Logistic Regression)
########################################################################

# Creación de dataframe que contiene solo los datos de pacientes clasificados
# negativos y con hipotiroidismo compensado
datosModelo <- datosFiltrados %>% 
  filter(classification == "primary hypothyroid" | 
           classification == "compensated hypothyroid" |
           classification == "negative")

# Descartar columnas inútiles
datosModelo <- datosModelo %>% select(-c("TSH measured",
                                         "T3 measured", 
                                         "TT4 measured",
                                         "T4U measured",
                                         "FTI measured",
                                         "TBG measured",
                                         "TBG"))

# Se convierten todas variables no numericas a factores
datosModelo <- datosModelo %>% 
  mutate_if(is.character,as.factor)

set.seed(99)
# Se crea el conjunto de entrenamiento y el conjunto de prueba
trainIndexD <- createDataPartition(datosModelo$classification, 
                                   p = 0.7, 
                                   list = FALSE)

datosEntrenamientoD <- datosModelo[trainIndexD,]
datosPruebaD  <- datosModelo[-trainIndexD,]

set.seed(99)
# Se entrenara el modelo con el metodo de validación de validación cruzada
# de 5 pliegues, con 10 repeticiones.
controlD <- trainControl(method="repeatedcv", number=5, repeats=10)
# Se entrena el modelo, usando la curva ROC como factor de optimización

modeloD <- train(classification ~ ., 
                 data=datosEntrenamientoD, 
                 method="multinom", 
                 preProcess="scale",
                 trace= FALSE,
                 trControl=controlD)

# Se estima la importancia de cada variable
importanciaD <- varImp(modeloD, scale=FALSE)
# Printear el resumen de la importancia de cada variable
print(importanciaD)
# Graficar el resumen anterior de importancia por variable
plot(importanciaD)
# Imprime los predictores finales del mejor modelo
predictors(modeloD)
# Resumen del modelo
print(modeloD)
# Se puede ver gráficamente como varía la precisión según curva ROC de acuerdo
# a las variables
print(ggplot(modeloD))

# Evaluar calidad predictiva del modelo con el conjunto de prueba.
prediccionesD <- predict(modeloD, datosPruebaD)
print(confusionMatrix(prediccionesD, datosPruebaD$classification))

###############################
# Matriz de correlacion  ##
###########################

#library(ggcorrplot)

# Descartar columnas inútiles

datosCor <- datosFiltrados

datosCor <- datosCor %>% select(-c("TSH measured",
                                   "T3 measured", 
                                   "TT4 measured",
                                   "T4U measured",
                                   "FTI measured",
                                   "TBG measured",
                                   "TBG"))

# Se convierten todas variables no numericas a factores
datosCor <- datosCor %>% 
  mutate_if(is.character,as.factor)


model.matrix(~0+., data=datosCor) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)


############################## asd
####################################
datosCor <- datosFiltrados

datosCor <- datosCor %>% select(-c("TSH measured", 
                                   "T3 measured", 
                                   "TT4 measured",
                                   "T4U measured",
                                   "FTI measured",
                                   "TBG measured",
                                   "TBG"))
# Se convierten todas variables no numericas a factores
datosCor <- datosCor %>% 
  mutate_if(is.character,as.factor)



corrplot(DescTools::PairApply(datosCor, DescTools::CramerV), 
         method="number",
         type="upper", 
         diag=F)








