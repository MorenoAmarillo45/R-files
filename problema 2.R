setwd("C:/Users/HP/Documents/datos 1")
datos <- read.csv("datos 2.csv")

plot(x=datos$x, y=datos$y)

hola <- function(x, x1, x2,y1,y2){
  variable1 <- y1*((x-x2)/(x1-x2)) + y2*((x-x1)/(x2-x1))
  return(variable1)
}

nuevosdatos <- 1:dim(datos)[1]
nuevosdatos

for(i in 1:dim(datos)[1]){
nuevosdatos[i] <- hola(datos$x[i], datos$x[2], datos$x[5],
                       datos$y[2], datos$y[5])
}

lines(x=datos$x, y=nuevosdatos, col='red')


logaritmo1 <- log(datos$x)
logaritmo2 <- log(datos$y)

lines(x=datos$x,y=logaritmo2)
plot(x=logaritmo1, y=logaritmo2)





