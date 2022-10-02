if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}

if(!require(cluster)){
  install.packages("cluster",dependencies = TRUE)
  require(cluster)
}

if(!require(factoextra)){
  install.packages("factoextra",dependencies = TRUE)
  require(factoextra)
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


# Se convierten todas variables no numericas a factores
datosFiltrados <- datosFiltrados %>% mutate_if(is.character,as.factor)

# Finalmente se decide por eliminar las columnas “measured” que indican si fue
# tomada la medición de la hormona que indica, debido a que con el filtro actual
# de datos todas corresponden al valor de true, por lo que esas columnas no
# aportan información, además se elimina la columna de TBG measured y TBG 
# debido a que para ninguna muestra fue medida, por lo que
# tampoco aportan información.

datosFiltrados <- datosFiltrados %>% select(-c("TSH measured", 
                                                         "T3 measured", 
                                                         "TT4 measured", "T4U measured",
                                                         "FTI measured", "TBG measured",
                                                         "TBG"))




##############################################################################
#                     Cluster
##############################################################################



