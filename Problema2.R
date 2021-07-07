rm(list=ls())
SD<- read.csv("StoppingDistance.csv",as.is = T)
SD
v<- SD$v
D<- SD$D
plot(x=v,y=D,type="l")
plot(x=v,y=D/v,type="b") # De la grafica, notamos que un ajuste cuadratico parece adecuado

# Existe un comportamiento lineal entre D/v y v.
getLinearBy2PointsMB<- function(x1,x2,y1,y2) {
  # f(x) = mx+b
  m<- y1/(x1-x2)+y2/(x2-x1)
  b<- -(y1*x2)/(x1-x2)-(y2*x1)/(x2-x1)
  return(c(m,b))
}
MB<-getLinearBy2PointsMB(v[2],v[18],D[2]/v[2],D[18]/v[18])
MB
# y= 0.047x+2.22
# D/v = 0.047v+2.22
# D = 0.047v^2+2.22v
prediceD<- function(x,m,b){
  ansD<- (b+m*x)*x
  return(ansD)  
}
predD<- prediceD(v,MB[1],MB[2])
predD
plot(x=v,y=D,main="Stopping Distance",
     xlab="Velocidad (mph)",ylab="Distancia (feet)")
lines(x=v,y=predD,col="red")

erroresRelativos<- (D-predD)/predD
erroresRelativos
plot(x=v,y=erroresRelativos,
     xlab="v",ylab="D",
     main="Error bajo el modelo cuadratico",
     type="l",col="blue")
