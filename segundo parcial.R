install.packages('e1071')
library(e1071)
library(ggplot2)
setwd('C:/Users/HP/Documents/examen modelado datos')

# Segundo Parcial Modelado Matemático
# Perla Moreno Pascual (: 

### Funciones ####

GetLinearBy2PointsMB <- function(x1,x2,y1,y2) {
  m<- y1/(x1-x2) + y2/(x2-x1)
  b<- -(y1*x2)/(x1-x2) - (y2*x1)/(x2-x1)
  return(c(m,b))
}

linearFun <- function(x,m,b){
  linear <- m*x + b
  return(linear)
}

ajusteLinealOptimo <- function(datX, datY){
  a <- cov(datX,datY)/var(datX)
  b <- mean(datY) - a*mean(datX)
  return(list(m=a,b=b))
}

r_xy <- function(datX,datY){
  c <- cov(datX,datY)/sqrt(cov(datX,datX)*cov(datY,datY))
  return(c)
}
  
#### Problema 1 ####

  DATOSP1 <- read.csv("E2P1.csv", as.is = TRUE)
  X1 <- DATOSP1$x
  Y1 <- DATOSP1$y
  plot(x=X1, y=Y1, xlab = 'x', ylab = 'y', main = NULL)
  
  #### a) Familia de posibles ajustes lineales ####
  s_b <- matrix(data = 1:30, nrow = 15, ncol = 2, byrow = T)
  n <- 1
  j <- 1
  repeat{  
    m <- 1
    repeat{
      if(n != m & n < m){
        s_b[j,] <- GetLinearBy2PointsMB(X1[n],X1[m],Y1[n],Y1[m])
        Ajuste <- linearFun(X1, s_b[j,1], s_b[j,2])
        lines(x=X1, y=Ajuste, lwd=1, type = "l", col="chartreuse3")
        j <- j+1
      }
      m <- m+1
      if(m==7){
        break
      }
    }
    n <- n+1
    if(n==6){
      break
    }
  }
  
  #### b) Error de Chebyshev ####
  Errores_1 <- 1:15
  i <- 1
  repeat{
    Errores_1[i] <- max(abs(Y1 - (s_b[i,1]*X1 + s_b[i,2])))
    i <- i+1
    if(i==16){
      break
    }
  }
  Errores_1
  
  #### b) Least Absolute Deviations Error ####
  Errores_2 <- 1:15
  i <- 1
  repeat{
    Errores_2[i] <- sum(abs(Y1 - (s_b[i,1]*X1 + s_b[i,2])))/6
    i <- i+1
    if(i==16){
      break
    }
  }
  Errores_2
  
  #### b) Least squares error ####
  Errores_3 <- 1:15
  i <- 1
  repeat{
    Errores_3[i] <- sum((Y1 - (s_b[i,1]*X1 + s_b[i,2]))^2)/6
    i <- i+1
    if(i==16){
      break
    }
  }
  Errores_3
  
  plot(x=1:15,y = Errores_1, col="red3", xlab = "Ajustes", ylab = "Error")
  axis(1, at = 1:15, cex.axis=1)
  
  plot(x=1:15,y = Errores_2, col= "red3", xlab = "Ajustes", ylab = "Error")
  axis(1, at = 1:15, cex.axis=1)
  
  plot(x=1:15,y = Errores_3, col="red3", xlab = "Ajustes", ylab = "Error")
  axis(1, at = 1:15, cex.axis=1)
  
  
  #### d) ALO = Ajuste Lineal Óptimo ####
  ALO <- ajusteLinealOptimo(X1, Y1)
  recta <- ALO$m*X1 + ALO$b
  
  plot(x=X1, y=Y1, xlab = 'x', ylab = 'y') 
  lines(x=X1, y=recta, lwd=2, col='dodgerblue2')
  
  Chebyschev_Error <- max(abs(Y1 - (recta)))
  LeastAD_Error <- sum(abs(Y1 - (recta)))/6
  LeastSquares_Error <- sum((Y1 - (recta))^2)/6
  
  #### f) AC = Ajuste cuadrático #### 
  AC <- lm(Y1~poly(X1,2,raw = T))
  AC$coefficients
  recta2 <- 0.6001301*X1^2 + 4.9990107*X1 + 0.1124135
  plot(x = X1, y= Y1, xlab = 'x', ylab = 'y')
  lines(x = X1, y= recta2,lwd=2, col='darkorange1')
 
  #### g) Errores del ajuste cuadrático ####
  Chebyschev_Error <- max(abs(Y1 - (recta2)))
  LeastAD_Error <- sum(abs(Y1 - (recta2)))/6
  LeastSquares_Error <- sum((Y1 - (recta2))^2)/6
  
#### Problema 2 ####
  
  ## Generación de lo 50 datos en E2P2-Perla.csv  
  #x <-rchisq(50,9)
  #y <- 10*x + rnorm(50,4,9)
  #datos_problema2 <- data.frame(x = x,y = y)
  #write.csv(datos_problema2,
  #          "C:/Users/HP/Documents/examen modelado datos\\E2P2-Perla.csv",
  #          row.names = TRUE)
  DATOSP2 <- read.csv("E2P2-Perla.csv", as.is = TRUE)
  x <- DATOSP2$x
  y <- DATOSP2$y
  plot(x,y, col="steelblue4")  
  
  x_media <- mean(x)
  y_media <- mean(y)
  x_var <- var(x)
  y_var <- var(y)
  S_xy <- cov(x,y)
  correlación <- r_xy(x,y)
  
  
#### Problema 3 #####
  
  ## Generación de lo 100 datos en E2P3-Perla.csv
  #x <- (rchisq(100,2) + rnorm(100, 2, 5))*rexp(100)
  #datos_problema3 <- data.frame(datos = x)
  #write.csv(datos_problema3,
  #          "C:/Users/HP/Documents/examen modelado datos\\E2P3-Perla.csv",
  #          row.names = TRUE)
  
  DATOSP3 <- read.csv("E2P3-Perla.csv", as.is = TRUE)
  X <- DATOSP3$datos
  hist(X, xlim=c(-15,50), col = "slategray3", border = "black", labels = F,
       xlab = NULL, ylab = "Frecuencia", main = NULL)
  skewness(X)
  kurtosis(X)
  
  
  