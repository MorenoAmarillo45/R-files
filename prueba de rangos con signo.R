n <- 100

rechazos <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

mu_T <- n*(n+1)/4
sigma_T <- sqrt(n*(n+1)*(2*n+1)/24)
Z_alpha <- qnorm(1-0.025, mean= 0, sd=1)
población <- rnorm(100, mean = 37, sd=1)
i <- 1

for(k in seq(35.5,38.5,by=0.1)){
  N <- 0
  repeat{
    muestra <- sample(población, size = 100, replace = TRUE, prob = NULL)
    diferencias <- c(muestra - k)
    jerarquias_diferencias <- rank(abs(diferencias[diferencias != 0]))
    tabla <- data.frame(muestra = muestra, signo = sign(diferencias),
                    diferencia = abs(diferencias), jerarquias = jerarquias_diferencias)

    sumaPositivos <- sum(tabla[tabla$signo == 1,"jerarquias"])
    sumaNegativos <- sum(tabla[ tabla$signo == -1,"jerarquias"])
    
    W <- min(sumaPositivos, sumaNegativos)
    Z_T <- (W - mu_T)/sigma_T
    Z_T
    if(Z_T > Z_alpha | Z_T < -1*Z_alpha){
      rechazos[i] <- rechazos[i] + 1
    }
    
    N <- N + 1
    if(N == 1001){
      i <- i+1
      break
    }
  }
}

rechazos
  
plot(x=seq(35.5,38.5,by=0.1), y=rechazos, xlab= "medias", type="o", 
    col="deepskyblue4")


rechazos2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

población2 <- rexp(100,rate=1/5)
i <- 1

for(k in seq(4,6,by=0.1)){
  N <- 0
  repeat{
    k=4
    muestra <- sample(población2, size = 100, replace = TRUE, prob = NULL)
    diferencias <- c(muestra - k)
    jerarquias_diferencias <- rank(abs(diferencias[diferencias != 0]))
    tabla <- data.frame(muestra = muestra, signo = sign(diferencias),
                        diferencia = abs(diferencias), jerarquias = jerarquias_diferencias)
    
    sumaPositivos <- sum(tabla[tabla$signo == 1,"jerarquias"])
    sumaNegativos <- sum(tabla[ tabla$signo == -1,"jerarquias"])
    
    W <- min(sumaPositivos, sumaNegativos)
    Z_T <- (W - mu_T)/sigma_T
    Z_T
    if(Z_T > Z_alpha | Z_T < -1*Z_alpha){
      rechazos2[i] <- rechazos2[i] + 1
    }
    
    N <- N + 1
    if(N == 1001){
      i <- i+1
      break
    }
  }
}

rechazos2

plot(x=seq(4,6,by=0.1), y=rechazos2, xlab= "medias", type="o", 
     col="deepskyblue4")












