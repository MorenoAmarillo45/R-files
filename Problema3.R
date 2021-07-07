OrbitalPeriods<- read.csv("OrbitalPeriods.csv")
Tp<- OrbitalPeriods$Tp
r<- OrbitalPeriods$r
getLinearBy2Points<- function(x,x1,x2,y1,y2) {
  predy<- y1*((x-x2)/(x1-x2))+y2*((x-x1)/(x2-x1))
  return(predy)
}
getLinearBy2PointsMB<- function(x1,x2,y1,y2) {
  m<- y1/(x1-x2)+y2/(x2-x1)
  b<- -(y1*x2)/(x1-x2)-(y2*x1)/(x2-x1)
  return(c(m,b))
}
## Ajuste Lineal
plot(x=r,y=Tp,type="b",main="Ajuste lineal")
predTpLineal<- getLinearBy2Points(r,r[2],r[7],Tp[2],Tp[7])
lines(x=r,y=predTpLineal,col="red")
## Ajuste Cuadratico
predTpCuadratico<- r*(getLinearBy2Points(r,r[2],r[7],Tp[2]/r[2],Tp[7]/r[7]))
plot(x=r,y=Tp,type="b",main="Ajuste cuadratico")
lines(x=r,y=predTpCuadratico,col="red")
## Trabajar con modelo Tp = Ar^(B)
lnTp<- log(Tp)
lnr<- log(r)
MB<- getLinearBy2PointsMB(lnr[2],lnr[7],lnTp[2],lnTp[7])
MB ## m = B, b = ln(A)
A<- exp(MB[2])
B<- MB[1]
predTp<- A*(r^B)
predTp
plot(x=r,y=Tp,type="b",main="Ajuste nuevo")
lines(x=r,y=predTp,col="red")
error<- (Tp-predTp)/predTp
plot(x=r,y=error,
     xlab="r",ylab="Tp",
     main="Error",
     type="l",col="blue")
