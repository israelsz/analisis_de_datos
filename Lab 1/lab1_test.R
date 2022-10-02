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

if(!require(WRS2)){
  install.packages("WRS2",dependencies = TRUE)
  require(WRS2)
}


if(!require(PairedData)){
  install.packages('PairedData',dependencies = TRUE)
  require(PairedData)
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

#######################################################################
#                  Graficos de puntos por clase
#######################################################################

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

# Evaluar calidad predictiva del modelo con el conjunto de prueba.
prediccionesD <- predict(modeloD, datosPruebaD)
print(confusionMatrix(prediccionesD, datosPruebaD$classification))

############################################
## Matriz de correlacion con V de Cramer  ##
############################################

datosCor <- datosFiltrados

# Se eliminan las columnas que no aportan información
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

# Se hace el gráfico de la matriz de correlación, usando V de cramer
corrplot(DescTools::PairApply(datosCor, DescTools::CramerV), 
         method="number",
         type="upper", 
         diag=F)

#############################################################################
# Pruebas de hipótesis
##########################################################################

# Hipotiroidismo primario
datosPrimary <- datosFiltrados %>% 
  filter(classification == "primary hypothyroid")
# Hipotiroidismo compensado
datosCompensated <- datosFiltrados %>% 
  filter(classification == "compensated hypothyroid")
# Negativos
datosNegative <-datosFiltrados %>% 
  filter(classification == "negative")
datosSecondary <-datosFiltrados %>% 
  filter(classification == "secondary hypothyroid")

compararMedias<- function(variable){
  variableDeseada <- c(datosPrimary[[variable]], 
                       datosCompensated[[variable]],
                       datosNegative[[variable]])
  clasificacion <- c(rep("Primary", length(datosPrimary[[variable]])), 
                     rep("Compensated", length(datosCompensated[[variable]])), 
                     rep("Negative", length(datosNegative[[variable]])))
  datos <- data.frame(variableDeseada, clasificacion)
  # Fijar nivel de significación.
  alfa <- 0.05
  
  # Comparar los diferentes algoritmos usando medias truncadas.
  cat("Comparación entre grupos usando medias truncadas\n\n")
  gamma <- 0.2
  set.seed(666)
  medias_truncadas <- t1way(variableDeseada ~ clasificacion, 
                            data = datos, tr = gamma,
                            alpha = alfa)
  print(medias_truncadas)
  if(medias_truncadas$p.value < alfa) {
    cat("\nProcedimiento post-hoc\n\n")
    set.seed(666)
    post_hoc <- lincon(variableDeseada ~ clasificacion, 
                       data = datos, tr = gamma,
                       alpha = alfa)
    print(post_hoc)
  }
}
mediasVariable <- function(variable){
  mediaPrimary<-mean(datosPrimary[[variable]])
  mediaCompensated<-mean(datosCompensated[[variable]])
  mediaNegative<-mean(datosNegative[[variable]])
  cat("Primary:",mediaPrimary,
      "Compensated:",mediaCompensated,
      "Negative: ",mediaNegative)
}

# Age

# Hipotesis nula: no existe diferencia en la edad promedio
# entre los pacientes con hipotiroidismo primario, 
# compensado y negativo.  
# Hipotesis alternativa: existe diferencia en la edad promedio
# entre los pacientes con hipotiroidismo primario, 
# compensado y negativo. 
shapiro.test(datosPrimary$age)
shapiro.test(datosCompensated$age)
shapiro.test(datosNegative$age)
# Al no cumplirse una distribución cercana a la normal
# de los datos, tamaños de muestra distintos, se utiliza un método
# robusto para comparar las medias.
compararMedias("age")
# Al tener un p = 0.9127, se puede concluir que no existe diferencia
# en la edad promedio entre los pacientes con hipotiroidismo primario, 
# compensado y negativo.  


# TSH
# Hipotesis nula: no existe diferencia en el nivel promedio de TSH
# entre los pacientes con hipotiroidismo primario, 
# compensado y negativo.  
# Hipotesis alternativa: existe diferencia en el nivel promedio de TSH
# entre los pacientes con hipotiroidismo primario, 
# compensado y negativo.  
shapiro.test(datosPrimary$TSH)
shapiro.test(datosCompensated$TSH)
shapiro.test(datosNegative$TSH)
# Al no cumplirse una distribución cercana a la normal
# de los datos, tamaños de muestra distintos, se utiliza un método
# robusto para comparar las medias.
compararMedias("TSH")
# Al tener un valor p = 0, se puede concluir que existe diferencia
# en el nivel de TSH promedio en al menos un grupo de pacientes con 
# hipotiroidismo primario, compensado y negativo.
# Por medio de la prueba post-hoc se puede concluir que existe
# diferencia en los niveles de TSH promedio entre los pacientes de
# hipotiroidismo primario, compensado y negativos.
mediasVariable("TSH")

#PRIMARIO VS COMPENSADO

# Hipotesis nula:
# Los niveles de TSH en promedio son iguales entre los
# pacientes de hipotiroidismo primario y compensado.

# Hipotesis alternativa:
# Los niveles de TSH promedio de los pacientes 
# de hipotiroidismo primario son mayores a los de pacientes
# de hipotiroidismo compensado.
yuen.t.test(datosPrimary$TSH,
            datosCompensated$TSH,
            tr=0.2,
            alternative="greater",
            paired = FALSE, mu = 0, conf.level=0.95)
# Al obtener un valor p = 4.837e-07, se puede concluir
# con un 95 % de certeza que los pacientes de hipotiroidismo
# primario tienen en promedio mayores niveles de TSH que los
# pacientes con hiporitoidismo compensado.

#PRIMARIO VS NEGATIVO

# Hipotesis nula:
# Los niveles de TSH en promedio son iguales entre los
# pacientes de hipotiroidismo primario y negativos.

# Hipotesis alternativa:
# Los niveles de TSH promedio de los pacientes 
# de hipotiroidismo primario son mayores a los de pacientes
# negativos.
yuen.t.test(datosPrimary$TSH,
            datosNegative$TSH,
            tr=0.2,
            alternative="greater",
            paired = FALSE, mu = 0, conf.level=0.95)
# Al obtener un valor p = 2.397e-08, se puede concluir
# con un 95 % de certeza que los pacientes de hipotiroidismo
# primario tienen en promedio mayores niveles de TSH que los
# pacientes negativos.

# COMPENSADO VS NEGATIVO

# Hipotesis nula:
# Los niveles de TSH en promedio son iguales entre los
# pacientes de hipotiroidismo compensado y negativos.

# Hipotesis alternativa:
# Los niveles de TSH promedio de los pacientes 
# de hipotiroidismo compensado son mayores a los de pacientes
# negativos.
yuen.t.test(datosCompensated$TSH,
            datosNegative$TSH,
            tr=0.2,
            alternative="greater",
            paired = FALSE, mu = 0, conf.level=0.95)
# Al obtener un valor p < 2.2e-16, se puede concluir
# con un 95 % de certeza que los pacientes de hipotiroidismo
# compensado tienen en promedio mayores niveles de TSH que los
# pacientes negativos.

#FTI
# Hipotesis nula: no existe diferencia en el nivel promedio de FTI
# entre los pacientes con hipotiroidismo primario, 
# compensado y negativo.  
# Hipotesis alternativa: existe diferencia en el nivel promedio de FTI
# entre los pacientes con hipotiroidismo primario, 
# compensado y negativo.  

shapiro.test(datosPrimary$FTI)
shapiro.test(datosCompensated$FTI)
shapiro.test(datosNegative$FTI)
#Al no tener distribuciones normales y tamaños 
#distintos entre cada uno de los grupos
#se aplica una prueba de una vía
#de multiples grupos independientes.
compararMedias("FTI")
# Al tener un valor p = 0, se puede concluir que existe diferencia
# en el nivel de FTI promedio en al menos
# un grupo de pacientes con  hipotiroidismo primario, compensado y negativo.
# Por medio de la prueba post-hoc se puede concluir que existe
# diferencia en los niveles de FTI promedio
# entre los pacientes de hipotiroidismo primario, compensado y negativos.
mediasVariable("FTI")
#PRIMARIO VS COMPENSADO

# Hipotesis nula:
# Los niveles de FTI en promedio son iguales entre los
# pacientes de hipotiroidismo primario y compensado.

# Hipotesis alternativa:
# Los niveles de FTI promedio de los pacientes 
# de hipotiroidismo primario son menores a los de pacientes
# de hipotiroidismo compensado.
yuen.t.test(datosPrimary$FTI,
            datosCompensated$FTI,
            tr=0.2,
            alternative="less",
            paired = FALSE, mu = 0, conf.level=0.95)
# Al obtener un valor p = 7.513e-16, se puede concluir
# con un 95 % de certeza que los pacientes de hipotiroidismo
# primario tienen en promedio menores niveles de FTI que los
# pacientes con hiporitoidismo compensado.

#PRIMARIO VS NEGATIVO

# Hipotesis nula:
# Los niveles de FTI en promedio son iguales entre los
# pacientes de hipotiroidismo primario y negativos.

# Hipotesis alternativa:
# Los niveles de FTI promedio de los pacientes 
# de hipotiroidismo primario son menores a los de pacientes
# negativos.
yuen.t.test(datosPrimary$FTI,
            datosNegative$FTI,
            tr=0.2,
            alternative="less",
            paired = FALSE, mu = 0, conf.level=0.95)
# Al obtener un valor p < 2.2e-16, se puede concluir
# con un 95 % de certeza que los pacientes de hipotiroidismo
# primario tienen en promedio menores niveles de FTI que los
# pacientes negativos.

# COMPENSADO VS NEGATIVO

# Hipotesis nula:
# Los niveles de FTI en promedio son iguales entre los
# pacientes de hipotiroidismo compensado y negativos.

# Hipotesis alternativa:
# Los niveles de FTI promedio de los pacientes 
# de hipotiroidismo compensado son menores a los de pacientes
# negativos.
yuen.t.test(datosCompensated$FTI,
            datosNegative$FTI,
            tr=0.2,
            alternative="less",
            paired = FALSE, mu = 0, conf.level=0.95)
# Al obtener un valor p = 1.298e-10, se puede concluir
# con un 95 % de certeza que los pacientes de hipotiroidismo
# compensado tienen en promedio menores niveles de FTI que los
# pacientes negativos.


# Mujeres embarazadas

datosMujeres <- datosFiltrados %>% filter(sex == "F")
datosMujeresEmbarazadas <- datosMujeres %>% filter(pregnant == "t")
datosMujeresNoEmbarazadas <- datosMujeres %>% filter(pregnant == "f")
# Hipotesis nula:
# Los niveles de T4U en promedio son iguales entre las
# pacientes embarazadas y no embarazadas.

# Hipotesis alternativa:
# Los niveles de T4U en promedio son mayores en las
# pacientes embarazadas con respecto a las no embarazadas.

yuen.t.test(datosMujeresEmbarazadas$T4U,
            datosMujeresNoEmbarazadas$T4U,
            tr=0.2,
            alternative="greater",
            paired = FALSE, mu = 0, conf.level=0.95)
# Al obtener un valor p = 1.272e-11, se puede concluir
# con un 95 % de certeza que los pacientes de hipotiroidismo
# compensado tienen en promedio menores niveles de FTI que los
# pacientes negativos.