#################Simulación MonteCarlo#################
###############Teorema del Límite Central##############

library(ggplot2)
library(tidyverse)

#################Generación de poblaciones#################

  población1 <- rgamma(1000000, 500, 1)
  mean(población1)
  var(población1)

  población2 <- rbeta(1000000, 0.5,0.85)
  mean(población2)
  var(población2)

  población3 <-rchisq(1000000, 5)
  mean(población3)
  var(población3)

  población4 <- rt(1000000, 5)
  mean(población4)
  var(población4)
  
  
###############################Parte 1###############################

  #################Generación de n muestras población1#################
    #####n=5
    muestras_1.5 <- replicate(n=5, sample(población1, size = 1000))
    medias_1.5 <- apply(muestras_1.5, MARGIN = 2, FUN = mean)
    x_1.5 <- mean(medias_1.5)
    s_1.5 <- var(medias_1.5)
  
    #####n=10
    muestras_1.10 <- replicate(n=10, sample(población1, size = 1000))
    medias_1.10 <- apply(muestras_1.10, MARGIN = 2, FUN = mean)
    x_1.10 <- mean(medias_1.10)
    s_1.10 <- var(medias_1.10)
  
    #####n=20
    muestras_1.20 <-replicate(n=20, sample(población1, size = 1000))
    medias_1.20 <- apply(muestras_1.20, MARGIN = 2, FUN = mean)
    x_1.20 <- mean(medias_1.20)
    s_1.20 <- var(medias_1.20)
  
    #####n=30
    muestras_1.30 <-replicate(n=30, sample(población1, size = 1000))
    medias_1.30 <- apply(muestras_1.30, MARGIN = 2, FUN = mean)
    x_1.30 <- mean(medias_1.30)
    s_1.30 <- var(medias_1.30)
  
  #################Histogramas medias de las muestras
  
    par(mfrow =c(2,2), mar=c(4,4,4,4))
    hist(medias_1.5, main=NULL, xlab = "(a)", ylab = "Frecuencia")
    hist(medias_1.10, main = NULL, xlab = "(b)", ylab = "Frecuencia")
    hist(medias_1.20, main = NULL, xlab = "(c)", ylab = "Frecuencia")
    hist(medias_1.30, main = NULL, xlab = "(d)", ylab = "Frecuencia")
  
  
  #################Generación de n muestras población2#################
    #####n=5
    muestras_2.5 <- replicate(n=5, sample(población2, size = 1000))
    medias_2.5 <- apply(muestras_2.5, MARGIN = 2, FUN = mean)
    x_2.5 <- mean(medias_2.5)
    s_2.5 <- var(medias_2.5)
  
    #####n=10
    muestras_2.10 <- replicate(n=10, sample(población2, size = 1000))
    medias_2.10 <- apply(muestras_2.10, MARGIN = 2, FUN = mean)
    x_2.10 <- mean(medias_2.10)
    s_2.10 <- var(medias_2.10)
  
    #####n=20
    muestras_2.20 <-replicate(n=20, sample(población2, size = 1000))
    medias_2.20 <- apply(muestras_2.20, MARGIN = 2, FUN = mean)
    x_2.20 <- mean(medias_2.20)
    s_2.20 <- var(medias_2.20)
  
    #####n=30
    muestras_2.30 <-replicate(n=30, sample(población2, size = 1000))
    medias_2.30 <- apply(muestras_2.30, MARGIN = 2, FUN = mean)
    x_2.30 <- mean(medias_2.30)
    s_2.30 <- var(medias_2.30)
  
  #################Histogramas medias de las muestras
  
    par(mfrow =c(2,2), mar=c(4,4,4,4))
    hist(medias_2.5, main=NULL, xlab = "(a)", ylab = "Frecuencia")
    hist(medias_2.10, main = NULL, xlab = "(b)", ylab = "Frecuencia")
    hist(medias_2.20, main = NULL, xlab = "(c)", ylab = "Frecuencia")
    hist(medias_2.30, main = NULL, xlab = "(d)", ylab = "Frecuencia")
  
  
  #################Generación de n muestras población3#################
    #####n=5
    muestras_3.5 <- replicate(n=5, sample(población3, size = 1000))
    medias_3.5 <- apply(muestras_3.5, MARGIN = 2, FUN = mean)
    x_3.5 <- mean(medias_3.5)
    s_3.5 <- var(medias_3.5)
  
    #####n=10
    muestras_3.10 <- replicate(n=10, sample(población3, size = 1000))
    medias_3.10 <- apply(muestras_3.10, MARGIN = 2, FUN = mean)
    x_3.10 <- mean(medias_3.10)
    s_3.10 <- var(medias_3.10)
  
    #####n=20
    muestras_3.20 <-replicate(n=20, sample(población3, size = 1000))
    medias_3.20 <- apply(muestras_3.20, MARGIN = 2, FUN = mean)
    x_3.20 <- mean(medias_3.20)
    s_3.20 <- var(medias_3.20)
  
    #####n=3
    muestras_3.30 <-replicate(n=30, sample(población3, size = 1000))
    medias_3.30 <- apply(muestras_3.30, MARGIN = 2, FUN = mean)
    x_3.30 <- mean(medias_3.30)
    s_3.30 <- var(medias_3.30)
  
  #################Histogramas medias de las muestras
  
    par(mfrow =c(2,2), mar=c(4,4,4,4))
    hist(medias_3.5, main=NULL, xlab = "(a)", ylab = "Frecuencia")
    hist(medias_3.10, main = NULL, xlab = "(b)", ylab = "Frecuencia")
    hist(medias_3.20, main = NULL, xlab = "(c)", ylab = "Frecuencia")
    hist(medias_3.30, main = NULL, xlab = "(d)", ylab = "Frecuencia")
  
  
  #################Generación de n muestras población4#################  
    #####n=5
    muestras_4.5 <- replicate(n=5, sample(población4, size = 100))
    medias_4.5 <- apply(muestras_4.5, MARGIN = 2, FUN = mean)
    x_4.5 <- mean(medias_4.5)
    s_4.5 <- var(medias_4.5)
  
    #####n=10
    muestras_4.10 <- replicate(n=10, sample(población4, size = 100))
    medias_4.10 <- apply(muestras_4.10, MARGIN = 2, FUN = mean)
    x_4.10 <- mean(medias_4.10)
    s_4.10 <- var(medias_4.10)
  
    #####n=20
    muestras_4.20 <-replicate(n=20, sample(población4, size = 100))
    medias_4.20 <- apply(muestras_4.20, MARGIN = 2, FUN = mean)
    x_4.20 <- mean(medias_4.20)
    s_4.20 <- var(medias_4.20)
  
    #####n=30
    muestras_4.30 <-replicate(n=30, sample(población4, size = 100))
    medias_4.30 <- apply(muestras_4.30, MARGIN = 2, FUN = mean)
    x_4.30 <- mean(medias_4.30)
    s_4.30 <- var(medias_4.30)
  
  #################Histogramas medias de las muestras
  
    par(mfrow =c(2,2), mar=c(4,4,4,4))
    hist(medias_4.5, main=NULL, xlab = "(a)", ylab = "Frecuencia")
    hist(medias_4.10, main = NULL, xlab = "(b)", ylab = "Frecuencia")
    hist(medias_4.20, main = NULL, xlab = "(c)", ylab = "Frecuencia")
    hist(medias_4.30, main = NULL, xlab = "(d)", ylab = "Frecuencia")
  
  
  #################Transformación N(0,1) de medias muestrales#################
    #####Población1
    z_1.30 <- (medias_1.30 - x_1.30)/((s_1.30)/1000)^(1/2)

    #####Población2
    z_2.30 <- (medias_2.30 - x_2.30)/((s_2.30)/1000)^(1/2)  

    #####Población3
    z_3.30 <- (medias_3.30 - x_3.30)/((s_3.30)/1000)^(1/2)
  
    #####Población4
    z_4.30 <- (medias_4.30 - x_4.30)/((s_4.30)/100)^(1/2)  
  
  #####Histograma medias normalizadas
  
    par(mfrow =c(2,2), mar=c(4,4,4,4))
    hist(z_1.30,  main = NULL, xlab = "(d)", ylab = "Frecuencia")
    hist(z_2.30,  main = NULL, xlab = "(d)", ylab = "Frecuencia")
    hist(z_3.30,  main = NULL, xlab = "(d)", ylab = "Frecuencia")
    hist(z_4.30,  main = NULL, xlab = "(d)", ylab = "Frecuencia")  



###############################Parte 2###############################

  #################Generación muestras tamaño n población1#################
    #####Tamaño mil
    muestras_1.mil <- replicate(n=30, sample(población1, size = 1000))
    medias_1.mil <- apply(muestras_1.mil, MARGIN = 2, FUN = mean)
    x_1.mil <- mean(medias_1.mil)
    s_1.mil <- var(medias_1.mil)

    #####Tamaño 5mil
    muestras_1.5mil <- replicate(n=30, sample(población1, size = 5000))
    medias_1.5mil <- apply(muestras_1.5mil, MARGIN = 2, FUN = mean)
    x_1.5mil <- mean(medias_1.5mil)
    s_1.5mil <- var(medias_1.5mil)
  
    #####Tamaño 10mil
    muestras_1.10mil <- replicate(n=30, sample(población1, size = 10000))
    medias_1.10mil <- apply(muestras_1.10mil, MARGIN = 2, FUN = mean)
    x_1.10mil <- mean(medias_1.10mil)
    s_1.10mil <- var(medias_1.10mil)
  
    #####Tamaño 100mil
    muestras_1.100mil <- replicate(n=30, sample(población1, size = 100000))
    medias_1.100mil <- apply(muestras_1.100mil, MARGIN = 2, FUN = mean)
    x_1.100mil <- mean(medias_1.100mil)
    s_1.100mil <- var(medias_1.100mil)
  
  #################Histogramas medias de las muestras
  
    par(mfrow =c(2,2), mar=c(4,4,4,4))
    hist(medias_1.mil, main= NULL, xlab = "(a)", ylab = "Frecuencia")
    hist(medias_1.5mil, main = NULL, xlab = "(b)", ylab = "Frecuencia")
    hist(medias_1.10mil, main = NULL, xlab = "(c)", ylab = "Frecuencia")
    hist(medias_1.100mil, main = NULL, xlab = "(d)", ylab = "Frecuencia")
  
  
  
  #################Generación muestras tamaño n población2#################
    #####Tamaño mil
    muestras_2.mil <- replicate(n=30, sample(población2, size = 1000))
    medias_2.mil <- apply(muestras_2.mil, MARGIN = 2, FUN = mean)
    x_2.mil <- mean(medias_2.mil)
    s_2.mil <- var(medias_2.mil)
  
    #####Tamaño 5mil
    muestras_2.5mil <- replicate(n=30, sample(población2, size = 5000))
    medias_2.5mil <- apply(muestras_2.5mil, MARGIN = 2, FUN = mean)
    x_2.5mil <- mean(medias_2.5mil)
    s_2.5mil <- var(medias_2.5mil)
  
    #####Tamaño 10mil
    muestras_2.10mil <- replicate(n=30, sample(población2, size = 10000))
    medias_2.10mil <- apply(muestras_2.10mil, MARGIN = 2, FUN = mean)
    x_2.10mil <- mean(medias_2.10mil)
    s_2.10mil <- var(medias_2.10mil)
  
    #####Tamaño 100mil
    muestras_2.100mil <- replicate(n=30, sample(población2, size = 100000))
    medias_2.100mil <- apply(muestras_2.100mil, MARGIN = 2, FUN = mean)
    x_2.100mil <- mean(medias_2.100mil)
    s_2.100mil <- var(medias_2.100mil)
  
  #################Histogramas medias de las muestras
    
    par(mfrow =c(2,2), mar=c(4,4,4,4))
    hist(medias_2.mil, main= NULL, xlab = "(a)", ylab = "Frecuencia")
    hist(medias_2.5mil, main = NULL, xlab = "(b)", ylab = "Frecuencia")
    hist(medias_2.10mil, main = NULL, xlab = "(c)", ylab = "Frecuencia")
    hist(medias_2.100mil, main = NULL, xlab = "(d)", ylab = "Frecuencia")
  
  
  #################Generación muestras tamaño n población3#################
    #####Tamaño mil
    muestras_3.mil <- replicate(n=30, sample(población3, size = 1000))
    medias_3.mil <- apply(muestras_3.mil, MARGIN = 2, FUN = mean)
    x_3.mil <- mean(medias_3.mil)
    s_3.mil <- var(medias_3.mil)
  
    #####Tamaño 5mil
    muestras_3.5mil <- replicate(n=30, sample(población3, size = 5000))
    medias_3.5mil <- apply(muestras_3.5mil, MARGIN = 2, FUN = mean)
    x_3.5mil <- mean(medias_3.5mil)
    s_3.5mil <- var(medias_3.5mil)
  
    #####Tamaño 10mil
    muestras_3.10mil <- replicate(n=30, sample(población3, size = 10000))
    medias_3.10mil <- apply(muestras_3.10mil, MARGIN = 2, FUN = mean)
    x_3.10mil <- mean(medias_3.10mil)
    s_3.10mil <- var(medias_3.10mil)
  
    #####Tamaño 100mil  
    muestras_3.100mil <- replicate(n=30, sample(población3, size = 100000))
    medias_3.100mil <- apply(muestras_3.100mil, MARGIN = 2, FUN = mean)
    x_3.100mil <- mean(medias_3.100mil)
    s_3.100mil <- var(medias_3.100mil)
  
  #################Histogramas medias de las muestras
  
    par(mfrow =c(2,2), mar=c(4,4,4,4))
    hist(medias_3.mil, main= NULL, xlab = "(a)", ylab = "Frecuencia")
    hist(medias_3.5mil, main = NULL, xlab = "(b)", ylab = "Frecuencia")
    hist(medias_3.10mil, main = NULL, xlab = "(c)", ylab = "Frecuencia")
    hist(medias_3.100mil, main = NULL, xlab = "(d)", ylab = "Frecuencia")
  
  #################Generación muestras tamaño n población4#################
    #####Tamaño mil
    muestras_4.mil <- replicate(n=30, sample(población4, size = 100))
    medias_4.mil <- apply(muestras_4.mil, MARGIN = 2, FUN = mean)
    x_4.mil <- mean(medias_4.mil)
    s_4.mil <- var(medias_4.mil)
  
    #####Tamaño 5mil
    muestras_4.5mil <- replicate(n=30, sample(población4, size = 500))
    medias_4.5mil <- apply(muestras_4.5mil, MARGIN = 2, FUN = mean)
    x_4.5mil <- mean(medias_4.5mil)
    s_4.5mil <- var(medias_4.5mil)
  
    #####Tamaño 10mil
    muestras_4.10mil <- replicate(n=30, sample(población4, size = 1000))
    medias_4.10mil <- apply(muestras_4.10mil, MARGIN = 2, FUN = mean)
    x_4.10mil <- mean(medias_4.10mil)
    s_4.10mil <- var(medias_4.10mil)
  
    #####Tamaño 100mil
    muestras_4.100mil <- replicate(n=30, sample(población4, size = 1500))
    medias_4.100mil <- apply(muestras_4.100mil, MARGIN = 2, FUN = mean)
    x_4.100mil <- mean(medias_4.100mil)
    s_4.100mil <- var(medias_4.100mil)
  
  #################Histogramas medias de las muestras
  
    par(mfrow =c(2,2), mar=c(4,4,4,4))
    hist(medias_4.mil, main= NULL, xlab = "(a)", ylab = "Frecuencia")
    hist(medias_4.5mil, main = NULL, xlab = "(b)", ylab = "Frecuencia")
    hist(medias_4.10mil, main = NULL, xlab = "(c)", ylab = "Frecuencia")
    hist(medias_4.100mil, main = NULL, xlab = "(d)", ylab = "Frecuencia")
  
  
  #################Transformación N(0,1) de medias muestrales#################
    #####Población1
    z_1.100mil <- (medias_1.100mil - x_1.100mil)/((s_1.100mil)/100000)^(1/2)
  
    #####Población2
    z_2.100mil <- (medias_2.100mil - x_2.100mil)/((s_2.100mil)/100000)^(1/2)  
  
    #####Población3
    z_3.100mil <- (medias_3.100mil - x_3.100mil)/((s_3.100mil)/100000)^(1/2)
  
    #####Población4
    z_4.100mil <- (medias_4.100mil - x_4.100mil)/((s_4.100mil)/1500)^(1/2)
  
  #####Histograma medias normalizadas
  
    par(mfrow =c(2,2), mar=c(4,4,4,4))
    hist(z_1.100mil,  main = NULL, xlab = "(a)", ylab = "Frecuencia")
    hist(z_2.100mil,  main = NULL, xlab = "(b)", ylab = "Frecuencia")
    hist(z_3.100mil,  main = NULL, xlab = "(c)", ylab = "Frecuencia")
    hist(z_4.100mil,  main = NULL, xlab = "(d)", ylab = "Frecuencia")
  
  