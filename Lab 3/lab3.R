if(!require(arulesViz)){
  install.packages("arulesViz",dependencies = TRUE)
  require(arulesViz)
}

if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}

if(!require(mice)){
  install.packages("mice",dependencies = TRUE)
  require(mice)
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

# Grafico de data faltante (Descomentar para ver)
#aggr_plot <- aggr(datosFiltrados, 
#                  col=c('cadetblue1','red'),
#                  numbers=TRUE,
#                  sortVars=TRUE,
#                  labels=names(datosFiltrados), 
#                  cex.axis=.90,
#                  ylab=c("Histograma de data faltante","Patrón"),
#                  gap = 0.5,
#                  oma = c(10,5,5,2),
#                  cex.numbers = 0.70)

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
datosNA <- NULL

# Se unen las clases positivas en una sola
datosFiltrados$classification[datosFiltrados$classification == "compensated hypothyroid"] <- "positive"
datosFiltrados$classification[datosFiltrados$classification == "secondary hypothyroid"] <- "positive"
datosFiltrados$classification[datosFiltrados$classification == "primary hypothyroid"] <- "positive"

# Se convierten todas variables no numericas a factores
datosFiltrados <- datosFiltrados %>% mutate_if(is.character,as.factor)

# Discretización de variables:

#Se fijan los rangos de edad
edad_valores <- c(0, 11, 26, 59, Inf)
edad_nombres <- c("Infancia","Juventud","Adultez","Vejez")

# Se fijan los rangos para cada hormona
rango_nombres<- c("Baja", "Normal", "Alta")

tsh_valores <- c(-Inf, 0.36, 4.7, Inf)
t3_valores <- c(-Inf, 1.1, 2.7, Inf)
tt4_valores <- c(-Inf, 59, 150, Inf)
t4u_valores <- c(-Inf, 0.73, 1.08, Inf)
fti_valores <- c(-Inf, 87, 132, Inf)

# Se reemplazan los valores por su clasificación según su rango
datosFiltrados$age <- cut(datosFiltrados$age, edad_valores, edad_nombres)
datosFiltrados$TSH <- cut(datosFiltrados$TSH, tsh_valores, rango_nombres)
datosFiltrados$T3 <- cut(datosFiltrados$T3, t3_valores, rango_nombres)
datosFiltrados$TT4 <- cut(datosFiltrados$TT4, tt4_valores, rango_nombres)
datosFiltrados$T4U <- cut(datosFiltrados$T4U, t4u_valores, rango_nombres)
datosFiltrados$FTI <- cut(datosFiltrados$FTI, fti_valores, rango_nombres)

# Se elimina la columna hypopituitary, debido a que no tiene varianza
# practicamente
datosFiltrados$hypopituitary <- NULL

###############################################################################
#                    Reglas para clase positiva
###############################################################################

reglas_positivas = apriori(
  data = datosFiltrados, 
  parameter=list(support = 0.01, confidence = 0.8, minlen = 3, maxlen = 17, 
                 target="rules"),
  appearance=list(rhs = c("classification=positive"))
)

# Se filtran solo reglas con lift mayor a uno
subReglas_positivas <-reglas_positivas[quality(reglas_positivas)$lift > 1]


# Se filtran las diez mejores reglas, ordenadas por su valor de lift
reglas_positivas_por_lift <- head(subReglas_positivas, n = 10, by = "lift")
# Muestra las reglas
inspect(reglas_positivas_por_lift)

# Se filtran las diez mejores reglas, ordenadas por su valor de confianza
reglas_positivas_por_confianza <- head(subReglas_positivas, n = 5,
                                       by = "confidence")
inspect(reglas_positivas_por_confianza)

###############################################################################
#                    Reglas para clase negativa
###############################################################################

lista <- c("classification=negative")

reglas_negativas = apriori(
  data = datosFiltrados, 
  parameter=list(support = 0.3, confidence = 0.8, minlen = 3, maxlen = 17, 
                 target="rules"),
  appearance=list(rhs = lista)
)

# Se filtran solo reglas con lift mayor a uno
subReglas_negativas <-reglas_negativas[quality(reglas_negativas)$lift > 1]

# Se filtran las diez mejores reglas, ordenadas por su valor de lift
reglas_negativas_por_lift <- head(subReglas_negativas, n = 10, by = "lift")
inspect(reglas_negativas_por_lift)

# Se filtran las diez mejores reglas, ordenadas por su valor de confianza
reglas_negativas_por_confianza <- head(subReglas_negativas, n = 10,
                                       by = "confidence")
inspect(reglas_negativas_por_confianza)

###############################################################################
#                    Reglas para Variable
###############################################################################

reglas_TSH = apriori(
  data = datosFiltrados[,c(1:20)], 
  parameter=list(support = 0.01, confidence = 0.5, minlen = 3, maxlen = 17, 
                 target="rules"),
  appearance=list(rhs = c("TSH=Alta"))
)

# Se filtran solo reglas con lift mayor a uno
subReglas_TSH <-reglas_TSH[quality(reglas_TSH)$lift > 1]


# Se filtran las diez mejores reglas, ordenadas por su valor de lift
reglas_TSH_por_lift <- head(subReglas_TSH, n = 10, by = "lift")
# Muestra las reglas
inspect(reglas_TSH_por_lift)

# Se filtran las diez mejores reglas, ordenadas por su valor de confianza
reglas_TSH_por_confianza <- head(subReglas_TSH, n = 5,
                                       by = "confidence")
inspect(reglas_TSH_por_confianza)


