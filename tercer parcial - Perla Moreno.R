#Tercer parcial
#Problemas extras
#Perla Moreno

#Población 1 gamma con media=78
  P1 <- rgamma(10000, 78, 1) 
  mean(P1)
  var(P1)

#Población 2 binomial con 1000 repeticiones y probabilidad de éxito 1/3
  P2 <- rbinom(1000, 1000, 1/3) 
  mean(P2)
  var(P2)

#Ejemplificación ley de grandes números
  
  c <- 1:1000
  i <- 1
  repeat {
    muestra <- sample(P1, size = i)
    c[i] <- mean(muestra)
    i <- i+1
    if(i == 1001){ break}
  }

  c2 <- 1:1000
  plot(x=c2, y = c, xlab= "N", ylab= "medias", main = "Medias de las muestras tamaño N")


#ejemplificación Teorema del Límite Central

  a <- 1:150
  j <- 1
  repeat{
    samples <- sample(P2, size = 150) #Tomo una muestra tamaño 150
    a[j] <- mean(samples)             #obtiene la media de esa muestra
    j <- j+1                          # y se guarda en a[i]
    if(j == 151){break}
  }
  
  hist(a, xlab = "medias", ylab = "muestras", main = "Histograma de medias" )
  var(a)
  sigmaTLC <- var(P2)/150
  sigmaTLC
