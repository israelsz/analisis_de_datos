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

# Creación de dataframe que contiene solo los datos de pacientes clasificados
# negativos y con hipotiroidismo primario
datosRlog <- datosFiltrados %>% 
  filter(classification == "negative" | classification == "primary hypothyroid")

################################################################################
# Regresión logística múltiple
################################################################################

# Descartar columnas inútiles
datosRlog <- datosRlog %>% select(-c("TSH measured", "T3 measured", 
                                     "TT4 measured", "T4U measured",
                                     "FTI measured", "TBG measured",
                                     "TBG"))

# Se convierten todas variables no numericas a factores
datosRlog <- datosRlog %>% mutate_if(is.character,as.factor)

# Separar variable de respuesta de los predictores.
#datosRlog[["classification"]] <- factor(datosRlog[["classification"]])

#datosRlog[["sex"]] <- factor(datosRlog[["sex"]])
#datosRlog[["on thyroxine"]] <- factor(datosRlog[["on thyroxine"]])
#datosRlog[["query on thyroxine"]] <- factor(datosRlog[["query on thyroxine"]])
#datosRlog[["on antithyroid medication"]] <- factor(datosRlog[["on antithyroid medication"]])
#datosRlog[["sick"]] <- factor(datosRlog[["sick"]])
#datosRlog[["pregnant"]] <- factor(datosRlog[["pregnant"]])
#datosRlog[["thyroid surgery"]] <- factor(datosRlog[["thyroid surgery"]])
#datosRlog[["I131 treatment"]] <- factor(datosRlog[["I131 treatment"]])
#datosRlog[["query hypothyroid"]] <- factor(datosRlog[["query hypothyroid"]])
#datosRlog[["query hyperthyroid"]] <- factor(datosRlog[["query hyperthyroid"]])
#datosRlog[["lithium"]] <- factor(datosRlog[["lithium"]])
#datosRlog[["goitre"]] <- factor(datosRlog[["goitre"]])
#datosRlog[["tumor"]] <- factor(datosRlog[["tumor"]])
#datosRlog[["hypopituitary"]] <- factor(datosRlog[["hypopituitary"]])
#datosRlog[["psych"]] <- factor(datosRlog[["psych"]])
#datosRlog[["referral source"]] <- factor(datosRlog[["referral source"]])

# Se crea el conjunto de entrenamiento y el conjunto de prueba
trainIndex <- createDataPartition(datosRlog$classification, p = 0.8, 
                                  list = FALSE)
datosEntrenamiento <- datosRlog[trainIndex,]
datosPrueba  <- datosRlog[-trainIndex,]

#########################################################################
# Modelo de regresión logistica negativo v/s hipotiroidismo primario
#########################################################################
set.seed(99)
# Se en entrenara el modelo con el metodo de validación de validación cruzada
# de 10 pliegues, con 3 repeticiones.
controlB <- trainControl(method="repeatedcv", number=10, repeats=3)
# Se entrena el modelo, usando la curva ROC como factor de optimización
set.seed(99)
modelo <- train(classification ~ ., 
               data=datosEntrenamiento, 
               method="rocc", 
               preProcess="scale", 
               trControl=controlB)
set.seed(99)
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

set.seed(99)
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

# Se crea el conjunto de entrenamiento y el conjunto de prueba
trainIndexB <- createDataPartition(datosRlogcompensado$classification, p = 0.8, 
                                  list = FALSE)

datosEntrenamientoB <- datosRlogcompensado[trainIndexB,]
datosPruebaB  <- datosRlogcompensado[-trainIndexB,]

set.seed(99)
# Se entrenara el modelo con el metodo de validación de validación cruzada
# de 10 pliegues, con 3 repeticiones.
controlB <- trainControl(method="repeatedcv", number=10, repeats=3)
# Se entrena el modelo, usando la curva ROC como factor de optimización
set.seed(99)
modeloB <- train(classification ~ ., 
                data=datosEntrenamientoB, 
                method="rocc", 
                preProcess="scale", 
                trControl=controlB)
set.seed(99)
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

set.seed(99)
# Evaluar calidad predictiva del modelo con el conjunto de prueba.
prediccionesB <- predict(modeloB, datosPruebaB)
print(confusionMatrix(prediccionesB, datosPruebaB$classification))

#########################################################################
# Modelo de regresión logistica hipo primario v/s hipotirodisimo compensado
#########################################################################


# Creación de dataframe que contiene solo los datos de pacientes clasificados
# negativos y con hipotiroidismo compensado
datosRlogHipoCompensado <- datosFiltrados %>% 
  filter(classification == "primary hypothyroid" | 
           classification == "compensated hypothyroid")

# Descartar columnas inútiles
datosRlogHipoCompensado <- datosRlogHipoCompensado %>% select(-c("TSH measured", 
                                                         "T3 measured", 
                                                         "TT4 measured",
                                                         "T4U measured",
                                                         "FTI measured",
                                                         "TBG measured",
                                                         "TBG"))

# En este caso especifico se descartaran las siguientes columnas por solo
# tener el mismo valor, lo que no aporta para la predicción
datosRlogHipoCompensado <- datosRlogHipoCompensado %>% select(-c("pregnant", 
                                                                 "goitre",
                                                                 "hypopituitary"))


# Se convierten todas variables no numericas a factores
datosRlogHipoCompensado <- datosRlogHipoCompensado %>% 
  mutate_if(is.character,as.factor)

# Se crea el conjunto de entrenamiento y el conjunto de prueba
trainIndexC <- createDataPartition(datosRlogHipoCompensado$classification, 
                                   p = 0.8, 
                                   list = FALSE)

datosEntrenamientoC <- datosRlogHipoCompensado[trainIndexC,]
datosPruebaC  <- datosRlogHipoCompensado[-trainIndexC,]

set.seed(99)
# Se entrenara el modelo con el metodo de validación de validación cruzada
# de 10 pliegues, con 3 repeticiones.
controlC <- trainControl(method="repeatedcv", number=10, repeats=3)
# Se entrena el modelo, usando la curva ROC como factor de optimización
set.seed(99)
modeloC <- train(classification ~ ., 
                 data=datosEntrenamientoC, 
                 method="rocc", 
                 preProcess="scale", 
                 trControl=controlC)
set.seed(99)
# Se estima la importancia de cada variable
importanciaC <- varImp(modeloC, scale=FALSE)
# Printear el resumen de la importancia de cada variable
print(importanciaC)
# Graficar el resumen anterior de importancia por variable
plot(importanciaC)
# Imprime los predictores finales del mejor modelo
predictors(modeloC)
# Resumen del modelo
print(modeloC)
# Se puede ver gráficamente como varía la precisión según curva ROC de acuerdo
# a las variables
print(ggplot(modeloC))

set.seed(99)
# Evaluar calidad predictiva del modelo con el conjunto de prueba.
prediccionesC <- predict(modeloC, datosPruebaC)
print(confusionMatrix(prediccionesC, datosPruebaC$classification))



#########################################################################
# Modelo de regresión logistica todas las clases
#########################################################################


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

# Se crea el conjunto de entrenamiento y el conjunto de prueba
trainIndexD <- createDataPartition(datosModelo$classification, 
                                   p = 0.8, 
                                   list = FALSE)

datosEntrenamientoD <- datosModelo[trainIndexD,]
datosPruebaD  <- datosModelo[-trainIndexD,]

set.seed(99)
# Se entrenara el modelo con el metodo de validación de validación cruzada
# de 10 pliegues, con 3 repeticiones.
controlD <- trainControl(method="repeatedcv", number=10, repeats=3)
# Se entrena el modelo, usando la curva ROC como factor de optimización
set.seed(99)
modeloD <- train(classification ~ ., 
                 data=datosEntrenamientoD, 
                 method="rocc", 
                 preProcess="scale", 
                 trControl=controlD)
set.seed(99)
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

set.seed(99)
# Evaluar calidad predictiva del modelo con el conjunto de prueba.
prediccionesD <- predict(modeloD, datosPruebaD)
print(confusionMatrix(prediccionesD, datosPruebaD$classification))

###################################
# Intento de prueba numero dos -> robado de internet
###############################
library(nnet)
# Fit the model
model <- nnet::multinom(classification ~., data = datosEntrenamientoD)
# Summarize the model
summary(model)
# Make predictions
predicted.classes <- model %>% predict(datosPruebaD)
head(predicted.classes)
# Model accuracy
mean(predicted.classes == datosPruebaD$classification)
# Mostrar matriz de confusión
print(confusionMatrix(predicted.classes, datosPruebaD$classification))

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




