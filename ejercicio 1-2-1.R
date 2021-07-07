setwd("C:/Users/HP/Documents/datos 1")
base_de_datos <- read.csv("datos 1.csv")

plot(x=base_de_datos$x, y=base_de_datos$y)

hola <- function(x, x1, x2,y1,y2){
  variable1 <- y1*((x-x2)/(x1-x2)) + y2*((x-x1)/(x2-x1))
  return(variable1)
}

#vector del 1 al 7
predY <- 1:dim(base_de_datos)[1]

for(i in 1:dim(base_de_datos)[1]){
  predY[i] <- hola(base_de_datos$x[i], base_de_datos$x[3], base_de_datos$x[6],
                   base_de_datos$y[3], base_de_datos$y[6])
}

predY
base_de_datos$y

lines(x=base_de_datos$x, y=predY,col='red')

errorRelativo <- (base_de_datos$y - predY)/predY
errorRelativo


plot(x=base_de_datos$x, y=errorRelativo, xlab="x", ylab="y")

intento1 <- log(base_de_datos$x)
intento2 <- log(base_de_datos$y)
intento2
plot (x=base_de_datos$x, y=intento2)
plot(x=intento1, y=intento2)