estatura <- c(121,123,108,118,111,109,114,103,110,115)
peso <- c(25,22,19,24,19,18,20,15,20,21)

plot(x=estatura, y=peso)

#De la gráfica pareciera que un ajuste lineal describe la tendencia
#de los datos, entonces definimos 

# peso = beta*estatura + gamma 

#donde 
beta <- cov(estatura, peso)/var(estatura)
beta
gamma <- mean(peso) -beta*mean(estatura)
gamma

#por lo que el ajuste queda como 

ajuste_lineal <- beta*estatura + gamma
lines(x=estatura,y=ajuste_lineal)


modelo <- lm(peso ~ poly(estatura,1,raw = T))
summary(modelo)

qt(0.025,8,lower.tail = F)




