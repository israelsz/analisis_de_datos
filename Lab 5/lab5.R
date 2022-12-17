if (!require(MTS)){
  install.packages("MTS", dependencies = TRUE)
  require(MTS)
}

if (!require(signal)){
  install.packages("signal", dependencies = TRUE)
  require(signal)
}

if (!require(TSA)){
  install.packages("TSA", dependencies = TRUE)
  require(TSA)
}

if (!require(oce)){
  install.packages("oce", dependencies = TRUE)
  require(oce)
}

if (!require(ggplot2)){
  install.packages("ggplot2", dependencies = TRUE)
  require(ggplot2)
}

if (!require(descomponer)){
  install.packages("descomponer", dependencies = TRUE)
  require(descomponer)
}


#Lectura archivo normocapnia
nomCols <- c("PAM", "CO2", "VFSC")
normocapnia <- read.delim(file.choose(new = FALSE), 
                          header = FALSE, 
                          sep="\t", 
                          col.names = nomCols)

#Lectura archivo hipercapnia
hipercapnia <- read.delim(file.choose(new = FALSE), 
                          header = FALSE, 
                          sep="\t", 
                          col.names = nomCols)




# Desde los datos discretos se generan las series temporales a 5HZ o período de 1/5 = 0.2 [s]
normocapnia_tiempo_PAM<-ts(data = normocapnia$PAM,  frequency = 5)
normocapnia_tiempo_VFSC<-ts(data = normocapnia$VFSC,  frequency = 5)
normocapnia_tiempo_CO2<-ts(data = normocapnia$CO2,  frequency = 5)


hipercapnia_tiempo_PAM<<-ts(data = hipercapnia$PAM,  frequency = 5)
hipercapnia_tiempo_VFSC<-ts(data = hipercapnia$VFSC,  frequency = 5)
hipercapnia_tiempo_CO2<-ts(data = hipercapnia$CO2,  frequency = 5)



#Se grafican las señales en el dominio del tiempo 
plot.ts(normocapnia_tiempo_PAM, main="Normocapnia: PAM vs Tiempo", xlab="Tiempo", ylab="PAM")
plot.ts(normocapnia_tiempo_VFSC, main="Normocapnia: VFSC vs Tiempo", xlab="Tiempo", ylab="VFSC")
#plot.ts(normocapnia_tiempo_CO2, main="Normocapnia: CO2 vs Tiempo", xlab="Tiempo", ylab="CO2")



plot.ts(hipercapnia_tiempo_PAM, main="Hipercapnia: PAM vs Tiempo", xlab="Tiempo", ylab="PAM")
plot.ts(hipercapnia_tiempo_VFSC, main="Hipercapnia: VFSC vs Tiempo", xlab="Tiempo", ylab="VFSC")
#plot.ts(hipercapnia_tiempo_CO2, main="Hipercapnia: CO2 vs Tiempo", xlab="Tiempo", ylab="CO2")

# Graficar al mismo tiempo Pam y VFSC para hipercapnia

ts.plot(hipercapnia_tiempo_PAM, hipercapnia_tiempo_VFSC, col=1:2, main = "VFSC y PAM sujeto con hipercapnia")
legend("topleft", legend = c("PAM","VFSC"), col = 1:2, lty = 1)


# Transformada Pwelch
pwelch(normocapnia_tiempo_PAM, nfft=256, fs=5)
pwelch(normocapnia_tiempo_VFSC, lwd=1, nfft = 256)

pwelch(hipercapnia_tiempo_PAM, nfft=256, fs=5)
pwelch(hipercapnia_tiempo_VFSC, lwd=1, nfft = 256)

# Correlación cruzada
# Rxy
normocapnia_correlacion_cruzada<-ccf(normocapnia_tiempo_PAM,
                                     normocapnia_tiempo_VFSC, 
                                     lag.max= 1000, pl="True") 

hipercapnia_correlacion_cruzada<-ccf(hipercapnia_tiempo_PAM,
                                     hipercapnia_tiempo_VFSC, 
                                     lag.max= 1000, pl="True") 

normocapnia_correlacion_cruzada_lag2k<-ccf(normocapnia_tiempo_PAM,
                                     normocapnia_tiempo_VFSC, 
                                     lag.max= 2000, pl="True") 

hipercapnia_correlacion_cruzada_lag2k<-ccf(hipercapnia_tiempo_PAM,
                                     hipercapnia_tiempo_VFSC, 
                                     lag.max= 2000, pl="True") 

# Transformada Pwelch con periodogramas
# wRxy
normocapnia_pwelch<-pwelch(normocapnia_correlacion_cruzada$acf, nfft=256, fs=5, plot=TRUE )  
hipercapnia_pwelch<-pwelch(hipercapnia_correlacion_cruzada$acf, nfft=256, fs=5, plot=TRUE )  

#Autocorrelacion
#Rx
normocapnia_auto_PAM<-acf(normocapnia_tiempo_PAM,
                          lag.max = length(normocapnia_tiempo_PAM)) 

hipecapnia_auto_PAM<-acf(hipercapnia_tiempo_PAM,
                         lag.max = length(hipercapnia_tiempo_PAM))


# Trasformada de Pwelch con autocorrelacion
#wx
normocapnia_auto_welch_PAM<-pwelch(normocapnia_auto_PAM$acf,
                                   nfft=256, fs=5, plot=TRUE)

hipercapnia_auto_welch_PAM<-pwelch(hipecapnia_auto_PAM$acf, 
                                   nfft=256, fs=5, plot=TRUE)



# Función de transferencia PAM
normocapnia_fun_transferencia<-normocapnia_pwelch$spec/normocapnia_auto_welch_PAM$spec 
plot(normocapnia_fun_transferencia, type="b")

hipercapnia_fun_transferencia<-hipercapnia_pwelch$spec/hipercapnia_auto_welch_PAM$spec 
plot(hipercapnia_fun_transferencia, type="b")



#Autocorrelación de vfsc para normo e hipercapnia
normocapnia_auto_VFSC<-acf(normocapnia_tiempo_VFSC,
                           lag.max = length(normocapnia_tiempo_VFSC)) 

hipecapnia_auto_VFSC<-acf(hipercapnia_tiempo_VFSC,
                          lag.max = length(hipercapnia_tiempo_VFSC))


# Pwelch vfsc                    
normocapnia_auto_welch_VFSC<-pwelch(normocapnia_auto_VFSC$acf,
                                    nfft=256, fs=5, plot=TRUE)

hipercapnia_auto_welch_VFSC<-pwelch(hipecapnia_auto_VFSC$acf, 
                                    nfft=256, fs=5, plot=TRUE)


# Funcion de transferencia para VFSC
normocapnia_fun_transferencia <- normocapnia_pwelch$spec/normocapnia_auto_welch_VFSC$spec 
plot(normocapnia_fun_transferencia, type="b")

hipercapnia_fun_transferencia<-hipercapnia_pwelch$spec/hipercapnia_auto_welch_VFSC$spec 
plot(hipercapnia_fun_transferencia, type="b")

# Aplicación de impulso unitario para VFSC
t <- 35
fin <- t/0.2
drop <- 76
step <- rep(1,fin)
step[drop:fin] <- rep(0,fin-drop+1)
fw <- butter(2,0.3)
step_f <- filter(fw$b, fw$a, step)

normo_tfm_t <- ifft(normocapnia_fun_transferencia)
hiper_tfm_t <- ifft(hipercapnia_fun_transferencia)
normo_Yx <- conv(step_f, normo_tfm_t)
plot(abs(normo_Yx))

hiper_Yx <- conv(step_f, hiper_tfm_t)
plot(abs(hiper_Yx))








