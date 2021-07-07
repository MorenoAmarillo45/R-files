setwd("C:/Users/HP/Downloads/Nueva carpeta")

population<- read.csv("population.csv",as.is=TRUE)
# Modelo exponencial
t<- population$t
P<- population$CO2
lnP<- log(P)
plot(t,P)
plot(t,lnP)
getLinearBy2Points<- function(x,x1,x2,y1,y2) {
  predy<- y1*((x-x2)/(x1-x2))+y2*((x-x1)/(x2-x1))
  return(predy)
}


getLinearBy2PointsMB<- function(x1,x2,y1,y2) {
  m<- y1/(x1-x2)+y2/(x2-x1)
  b<- -(y1*x2)/(x1-x2)-(y2*x1)/(x2-x1)
  return(c(m,b))
}
mb<-getLinearBy2PointsMB(t[2],t[10],lnP[2],lnP[10])
mb[1]
# 1/35 se aproxima 
# Nota: aveces la interpretatibilidad del modelo es mejor que
# una caja negra. 

# El modelo es entonces P=P0exp((t-t0)/35)
P0<- P[1]
t0<- t[1]
predP<- P0*exp((t-t0)/35)
predP
plot(x=t[1:11],y=P[1:11],type="b",main="Ajuste exponencial")
lines(x=t[1:11],y=predP[1:11],col="red")
error<- (P[1:11]-predP[1:11])/predP[1:11]
plot(x=t[1:11],y=error,
     xlab="r",ylab="Tp",
     main="Error",
     type="l",col="blue")

## Modelo de crecimiento logistico
reducedPop<- population$P[1:12]
reducedPop[-1]-reducedPop[-length(reducedPop)]
indxc<- which.max(reducedPop[-1]-reducedPop[-length(reducedPop)])
tc<- population$t[indxc]
Pc<-population$P[indxc]
transP<- log(2*(Pc/P)-1)
plot(x=population$t[1:12],y=transP[1:12])
MB<- getLinearBy2PointsMB(population$t[2],population$t[11],
                          transP[2],transP[11])
MB
# -1/tau = m => tau = -1/m
tau<- -1/MB[1]
tau #

predP<- 2*(Pc/(1+exp(-(population$t-tc)/tau)))
plot(x=population$t[1:12],y=population$P[1:12],type="b",main="Ajuste logistico")
lines(x=population$t[1:12],y=predP[1:12],col="red")
error<- (population$P[1:12]-predP[1:12])/predP[1:12]
plot(x=population$t[1:12],y=error,
     xlab="r",ylab="Tp",
     main="Error",
     type="l",col="blue")
#### Ajuste por polinomios
graficaCaso(x_dat=population$t,y_dat=population$P,
            iniX=min(population$t)-1,finX=max(population$t)+1)
### Nueva funcion auxiliar
graficaAjusteParcialPolinomio<- function(x_dat,y_dat,sopPolX,iniX=0,finX=9) {
  # Graficar ajuste polinomial de los datos
  # Por default se grafica en [0,9]
  ejeX<- seq(from=iniX,to=finX,length.out = 100)
  ejeY<- unlist(lapply(ejeX,evaluaPolinomioExacto,
                       dat_x=x_dat[sopPolX],dat_y=y_dat[sopPolX]))
  plot(ejeX,ejeY,type='l')
  lines(x_dat,y_dat,type='p')
}

ajusteParcialPolinomio<- function(x_dat,y_dat,sopPolX) {
  # Graficar ajuste polinomial de los datos
  # Por default se grafica en [0,9]
  predY<- unlist(lapply(x_dat,evaluaPolinomioExacto,
                        dat_x=x_dat[sopPolX],dat_y=y_dat[sopPolX]))
  return(predY)
}


### Ajuste por polinomio cuadratico
population$t[c(1,11,26)]
graficaAjusteParcialPolinomio(x_dat=population$t,y_dat=population$P,
                              sopPolX = c(1,11,26),iniX=1790,finX=2050)
predP<- ajusteParcialPolinomio(x_dat=population$t,y_dat=population$P,
                               sopPolX = c(1,11,26))
error<- (population$P-predP)/predP
plot(x=population$t,y=error,
     xlab="r",ylab="Tp",
     main="Error",
     type="l",col="blue")
# Ajuste lineal
population$t[c(22,26)]
graficaAjusteParcialPolinomio(x_dat=population$t,y_dat=population$P,
                              sopPolX = c(22,26),iniX=1790,finX=2050)
predP<- ajusteParcialPolinomio(x_dat=population$t,y_dat=population$P,
                               sopPolX = c(22,26))
population$t[c(1,11,26)]
error<- (population$P-predP)/predP
plot(x=population$t[22:27],y=error[22:27],
     xlab="r",ylab="Tp",
     main="Error",
     type="l",col="blue")

###### Problema ambiental

### Observar CO2
co2<- read.csv("co2.csv",as.is=TRUE)
co2$t
co2$CO2
plot(co2$t,co2$CO2)
## Modelo 'casi lineal'
# co2(t)= co2(t0)+s(t)(t-t0)
# Modelar s(t)
co20<- co2$CO2[1]
t0<- co2$t[1]
S<- (co2$CO2-co20)/(co2$t-t0)
plot(co2$t,S)
# 17,42, 1975 y 2000
MB<- getLinearBy2PointsMB(co2$t[17],co2$t[42],S[17],S[42])
MB
predCO2<- co20+(MB[1]*co2$t+MB[2])*(co2$t-t0)
plot(x=co2$t,y=co2$CO2)
lines(x=co2$t,y=predCO2,type='l',col='red')
## Por tanto tenemos un modelo cuadratico para CO2.

temperatura<- read.csv("temperatura.csv",as.is = T)
plot(temperatura,type='b')
### Reducir efecto aleatorio al tomar promedios.
install.packages('zoo')
library(zoo)
vec<- 1:10
vecmean<- rollmean(vec,k=4,fill= NA,align='right')
?seq
seq.int(from=4,by=4,length.out=5)
# 145
# de 7 en 7
# 7(1)=7,7(2)=14,7(3)=21, ... ,
# 7*(20)=140 es el ultimo
# son 20 
# 20 = 145 %/% 7
145 / 7
145 %/% 7

temperatura$t
plot(temperatura$t,
     temperatura$T,
     type='b',xlab='Year',ylab='Temp Dif',main='Sin promediar' )
plot(temperatura$t,
     rollmean(temperatura$T,k=5,align='center',fill=NA),
     type='b',xlab='Year',ylab='Temp Dif',main='Promedio 5' )
plot(temperatura$t,
     rollmean(temperatura$T,k=10,align='center',fill=NA),
     type='b',xlab='Year',ylab='Temp Dif',main='Promedio 10')
plot(temperatura$t,
     rollmean(temperatura$T,k=20,align='center',fill=NA),
     type='b',xlab='Year',ylab='Temp Dif',main='Promedio 20')
temp20<-rollmean(temperatura$T,k=20,align='center',fill=NA)
## Se asume modelo que crece como una funcion potencia
## T= -0.35 + ((t-1840)/a)^b
## Modelo ln(T+0.35) = bln(t-1840)-bln(a)
# en notacion y=mx+b esto es
# MB[1] -> b , MB[2] -> -bln(a)
# Entonces  b = MB[1] , a = exp(-MB[2]/b)
transt<- log(temperatura$t-1840)
transT<- log(temp20+0.35)
temperatura$t[c(111,149)]
MB<- getLinearBy2PointsMB(transt[111],transt[149],transT[111],transT[149])
b<- MB[1]
a<- exp(-MB[2]/b)
b
predT<- -0.35 + ((temperatura$t-1840)/a)^b
plot(temperatura$t,temp20)
lines(temperatura$t,predT,type='l',col='red')
plot(temperatura$t,temperatura$T)
lines(temperatura$t,predT,type='l',col='red')
# Nota b casi es 4
# Se asume modelo potencia de grado 4.
## Algebraicamente, se puede relacionar co2 con T.
## Pero el modelo obtenido puede ser muy complejo,
## entonces usamos otra idea. Una aproximacion por el primer
## termino de la serie de taylor.


#### Relacion entre co2 y T
indY<- match(co2$t,temperatura$t)
plot(co2$CO2,temperatura$T[indY])
cor(co2$CO2,temperatura$T[indY])