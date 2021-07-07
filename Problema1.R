USEnergy<- read.csv("USEnergy.csv",as.is=TRUE)
plot(x=USEnergy$t,y=USEnergy$C)

getLinearBy2Points<- function(x,x1,x2,y1,y2) {
  predy<- y1*((x-x2)/(x1-x2))+y2*((x-x1)/(x2-x1))
  return(predy)
}

predY<- 1:dim(USEnergy)[1] # Crear vector de longitud 12
for(i in 1:dim(USEnergy)[1]){
  predY[i]<- getLinearBy2Points(USEnergy$t[i],
                                USEnergy$t[1],USEnergy$t[10],
                                USEnergy$C[1],USEnergy$C[10])
}
predY
USEnergy$C

lines(x=USEnergy$t,y=predY,col='red')
?lines
errorRelativo = (USEnergy$C-predY)/predY
errorRelativo
?plot
plot(x=USEnergy$t,y=errorRelativo,
     xlab="Year",ylab="Error Relativo",
     main="Error bajo el modelo lineal",
     type="l",col="blue")
## Ejemplo sin for
predY2<- getLinearBy2Points(USEnergy$t,
                            USEnergy$t[1],USEnergy$t[10],
                            USEnergy$C[1],USEnergy$C[10])
predY2
predY
dir()
getwd()
