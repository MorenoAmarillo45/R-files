library(ggplot2)

# Hipótesis Nula: mu = 16
# Hipóteis Alterna: mu =/ 16


#### SIMULACIÓN ####
a <- seq(6,26, by = 0.5)
length(a)

población <- rnorm(100, mean = 16, sd = 16)
#P(|Z|<Z_alpha/2)=alpha región donde NO se rechaza -z<Z<z
#P(|Z|> Z_alpha/2)=alpha 
#P(Z>z)=alpaha/2 + P(Z<-z)=alpha/2
Z_score <- qnorm(0.05/2)
Power <- 1:41

cnts <- 1
for (j in seq(6,26, by = 0.5)) {
  contador <- 0
  cnt <- 1
  
  repeat{
    muestra <- sample(población, size = 100, replace= TRUE, prob= NULL)
    Z <- (mean(muestra) - j)/(16/sqrt(100))
    
    if(Z<Z_score | Z>-Z_score){
      contador <- contador + 1 
      }
    
    cnt <- cnt + 1 
    
    if(cnt> 10000){
      break
      }
  } 
  Power[cnts] <- contador/10000
  cnts <- cnts + 1
}


plot(x = seq(6,26, by = 0.5), y = Power, 
     main = "Función Potencia por simulación",
        xlab= "Valores posibles de la media", 
        ylab = "Probabilidad de error tipo I", 
        type="o", xaxt = "n", col="deepskyblue4")
axis(1, at = seq(6,26, by = 1), cex.axis=1)

Power

#### TEÓRICO ####

lz_score <- qnorm(0.05/2, mean = 16, sd = 16, lower.tail=TRUE)
rz_score <- qnorm(0.05/2, mean =16, sd = 16, lower.tail=FALSE)

iz_score <- qnorm(0.05/2, mean = 0, sd = 1, lower.tail=TRUE)
dz_score <- qnorm(0.05/2, mean = 0, sd = 1, lower.tail=FALSE)

puntos <- 1:41
length(puntos)
length(seq(-100,100, by=5))

cnts <- 1
for (i in seq(-100,100, by=5)) {
  P1 <- pnorm(lz_score - 16/sqrt(100), mean = i, sd = 16, lower.tail=TRUE)
  P2 <- pnorm(rz_score - 16/sqrt(100), mean = i, sd = 16, lower.tail=FALSE)
  puntos[cnts] = P1 + P2
  cnts <- cnts + 1
}

plot(x = seq(-100,100, by = 5), y = puntos, 
     main = "Función Potencia",
     xlab= "Valores posibles de la media", 
     ylab = "Probabilidad de error tipo I", 
     type="o", xaxt = "n", col="deepskyblue4")
axis(1, at = seq(-100,100, by=5), cex.axis=1) 
puntos

