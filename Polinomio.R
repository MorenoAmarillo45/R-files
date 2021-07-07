# Ajuste polinomial, peligros de errores en las mediciones
rm(list=ls())
n<- 8
# Relacion simple y=x
x<- 1:n
y<- 1:n
# Ruido al 00.01 % 
y_a<- 1:n
for(i in 1:n) {
  y_a[i]<- y[i]+((-1)^(i+1))*y[i]/10000
}
y_a
# Ruido al 00.1%
y_b<- 1:n
for(i in 1:n) {
  y_b[i]<- y[i]+((-1)^(i+1))*y[i]/1000
}
y_b
# Ruido al 1%
y_c<- 1:n
for(i in 1:n) {
  y_c[i]<- y[i]+((-1)^(i+1))*y[i]/100
}
y_c
# Ruido al 5%
y_d<- 1:n
for(i in 1:n) {
  y_d[i]<- y[i]+5*((-1)^(i+1))*y[i]/100
}
y_d
# Ruido del 1% en un elemento
y_e<- 1:n
y_e<- y
y_e[4]<- y[4]+y[4]/100
y_e
# Ruido del 20% en un elemento
y_f<- 1:n
y_f[4]<- y[4]+y[4]*(20/100)
y_f


## n puntos, ajusta polinomio de orden n-1 (grado a lo más n-1)
# Funcion sum, prod

v <-6:10
v
sum(v)
prod(v)
v[-3]


evaluaPolinomioExacto<- function(x,dat_x,dat_y){
  # Objetivo, evaluar el polinomio que ajusta dat_x
  # dat_y en x.
  # Nota 1: los indices aqui estan 1-indexados, y el 
  # polinomio es de grado n-1. Contrastar con formula del libro
  # Nota 2: En este contexto, debemos cuidar que x sea
  # un vector de longitud uno.
  n<- length(dat_x)
  L<- 1:n
  for(k in 1:n){
    #evaluar L_k en x.
    L[k]<- prod(x-dat_x[-k])/prod(dat_x[k]-dat_x[-k])
  }
  ans<- sum(dat_y*L)
  return(ans)
}
## Funcion seq, lapply, unlist

?seq
seq(from=2, to=8,length.out = 1000)
## Los for en R son tardados.
##No se recomienda usar for en R.
## Una alternativa es usar la familia de funciones
## apply
fun<- function(x){
  return(x+2)
}
v<- 1:5
lapply(v,fun)
estr<- lapply(v, fun)
class(estr)
estr[1]


graficaCaso<- function(x_dat,y_dat,iniX=0,finX=9) {
  # Graficar ajuste polinomial de los datos
  #Por default se grafica en [0,9]
  ejeX<- seq(from=iniX,to=finX,length.out = 100)
  ejeY<- unlist(lapply(ejeX,evaluaPolinomioExacto,dat_x=x_dat,dat_y=y_dat))
  plot(ejeX,ejeY,type='l')
  lines(x_dat,y_dat,type='p')
}

graficaCaso(x,y)
graficaCaso(x,y_a)
graficaCaso(x,y_b)
graficaCaso(x,y_c,iniX=0.5,finX=8.5)
graficaCaso(x,y_d,iniX = 0.5,finX=8.5)
graficaCaso(x,y_e)
graficaCaso(x,y_f,iniX=0.9,finX=8.1)

setwd("C:/Users/HP/Documents")


USEnergy <- read.csv("USEnergy.csv", as.is= TRUE)
SD <- read.csv("Stopping")



graficaCaso(USEnergy$t,USEnergy$C,
            iniX=min(USEnergy$t)-1,
            finX=max(USEnergy$t)+1)

graficaCasoLineal<- function(x_dat,y_dat,iniX=0,finX=9,indX1=1,indX2=2) {
  # Graficar ajuste polinomial de los datos
  ejeX<- seq(from=iniX,to=finX,length.out = 100)
  ejeY<- unlist(lapply(ejeX,evaluaPolinomioExacto,
                       dat_x=x_dat[c(indX1,indX2)],
                       dat_y=y_dat[c(indX1,indX2)]))
  plot(ejeX,ejeY,type='l')
  lines(x_dat,y_dat,type='p')
}

graficaCasoLineal(x,y,indX1 = 2,indX2 = length(x)-1)
graficaCasoLineal(x,y_a,indX1 = 2,indX2 = length(x)-1)
graficaCasoLineal(x,y_b,indX1 = 2,indX2 = length(x)-1)
graficaCasoLineal(x,y_c,indX1 = 2,indX2 = length(x)-1)
graficaCasoLineal(x,y_d,indX1 = 2,indX2 = length(x)-1)
graficaCasoLineal(x,y_e,indX1 = 2,indX2 = length(x)-1)
graficaCasoLineal(x,y_f,indX1 = 2,indX2 = length(x)-1)
setwd("C:/Users/angel/Desktop/FCFM_Clases/Semestre Ago 20 - Ene 20/Modelado Matematico AGO-DIC 2020")
getwd()
setwd("DataSets")

USEnergy<- read.csv("USEnergy.csv",as.is=TRUE)
SD<- read.csv("StoppingDistance.csv",as.is = T)
OrbitalPeriods<- read.csv("OrbitalPeriods.csv")
# Ajuste datos de consumo de energia
graficaCaso(USEnergy$t,USEnergy$C,
            iniX=min(USEnergy$t)-1,
            finX=max(USEnergy$t)+1)
# Ajuste datos velocidad de paro
graficaCaso(SD$v,SD$D,
            iniX=min(SD$v)-1,
            finX=max(SD$v)+1)
# Ajuste datos datos periodo orbital
graficaCaso(OrbitalPeriods$r,OrbitalPeriods$Tp,
            iniX=min(OrbitalPeriods$r)-0.01,
            finX=max(OrbitalPeriods$r)+0.01)

population<- read.csv("population.csv",as.is=TRUE)
# Modelo exponencial
t<- population$t
P<- population$P
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


co2<- read.csv("co2.csv",as.is=TRUE)
co2$t
co2$CO2
