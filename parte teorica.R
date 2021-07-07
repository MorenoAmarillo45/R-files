library(ggplot2)
# n=100
# N(mean = 16, sd = 16) 
# H0: media = 16 
# z_score <- qnorm(1-0.05/2)
# P(Rechazar H0 / media = 16 )
# P( |x_ - media / sd/sqrt(n)| > z_score )

# P(x_ - media / sd/sqrt(n) < -z_score) + P(x_ - media / sd/sqrt(n) > z_score)
# lo anterior es igual 0.05 porque así lo "acomodamos" 
# por lo que el primer punto de la grafica sería en (x=16, 0.05)
# recordemos que alpha = 0.05 es la probabilidad de error tipo 1 

# supongamos que la media real no es 16 pero 16.1
# 
# P( |x_ - media / sd/sqrt(n)| > z_score / mu = 16.1)
# P( x_ - mu=16.1 / sd/sqrt(n) < -z_score - mu=16.1/ sd/sqrt(n) )
 # falta la otra probabilidad, "el otro lado", los sumo y eso sería el
 # segundo punto de la gráfica

lz_score <- qnorm(0.05/2, mean = 16, sd = 16, lower.tail=TRUE)
z_score
rz_score <- qnorm(0.05/2, mean =16, sd = 16, lower.tail=FALSE)

puntos <- 1:41
length(puntos)
length(seq(-100,100, by=5))
cnts <- 1
for (i in seq(-100,100, by=5)) {
  P1 <- pnorm(lz_score, mean = i, sd = 16, lower.tail=TRUE)
  P2 <-  pnorm(rz_score, mean = i, sd = 16, lower.tail=FALSE)
  puntos[cnts] = P1 + P2
  cnts <- cnts + 1
}


puntos
plot(puntos)

plot(x = seq(-100,100, by = 5), y = puntos, 
     main = "Función Potencia",
     xlab= "Valores posibles de la media", 
     ylab = "Probabilidad del error tipo I", 
     type="o", xaxt = "n", col="deepskyblue4")
axis(1, at = seq(-100,100, by=10), cex.axis=1) 





