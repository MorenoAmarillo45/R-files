#Ejercicios estructuras de datos

#Vectores

#Múltiplos de 2 desde 1 hasta el 49

2:49 #este da todos los números del 2 al 49, no solo los mpultiplos

seq(2, 49, by=2) #este sí da los múltiplos de 2

2*1:49
2*1:24

## Cuatro veces el 3

c(3,3,3,3)

rep(3,4)

#valores del 1 al 5

1:5

seq(1,5,by=1)
seq(1,5, by=0.5)
seq(5)
sequence(5)

# valores 2,3,1,2,1

c(2,3,1,2,1)

#Los valores "naranja". "rojo", "azul", "amarillo", "morado".

c('naranja', 'rojo', "azul", 'amarillo', 'morado')

#Ejercicio A6. Los valores en la posición 10 al 12 del vector del ej A1

#ojo en R todo empieza en la posición 1 :o

EA6 <- seq(2,49,by=2)
EA6[10]
EA6[11]
EA6[12]
EA
EA6[15]
EA6[10:12]

#Ejercicio A7. Los valores del vector del ej A1, pero sin los primeros 5 elementos

EA6[-24]
-1*1:5
-5:-1
-1:-5
EA6[6:24]
EA6[-5:-1]
EA6

1:5
8:10

#concatenar

c(1:5,8:10)

#ahora si quiero esos elementos del vector EA6, usamos la concatenación

EA6[c(1:5,8:10)]




#Ejercicio A8. Los valores ordenados del vector del eje. A4

EA4 <- c(2,3,1,2,1)
order(EA4)

EA4[order(EA4)]


#Ejercicio A9. El vector que contiene al vector del ej. A4 y del ej. A3 concatenados

EA3 <- 1:5
c(EA4,EA3)

#Ejercicio A10. El vector con los valores del vector del ej. A5 ordenados de la "z"
# a la "a"


EA5 <- c("naranja", "rojo", "azul", "amarillo", "morado")
EA5[order(EA5, decreasing = TRUE)]
EA5[order(EA5, decreasing = FALSE)]

#Ejercicio A11. Gemerar el vector








#Matrices
datos <- 1:12
matrix(datos,nr=4,ncol=3)
matrix(datos, nrow=3, ncol=2)
matrix(datos, nrow=5, ncol=4) ##REVISAR
matrix(datos, nrow=6, ncol=4)



matrix(datos, nrow=3, ncol=2, byrow = TRUE)
matrix(datos, nrow=3, ncol=2, byrow = FALSE)


###Buscar la transpuesta y la inversa, hay funciones pero podríamos 
####hacer nuestra función como ejercicio e.e

a <- 1:4
b <- 11:13
c <- 21:25

(ej1 <- cbind(a,b,c)) ###INVESTIGAR   
#también está la función rbind)


#función dim devuelve las dimenciones de la matriz
length(a)


#DATA FRAMES

#Ejercicio C1: Crea un dataframe vacío.

data.frame()

#Ejercicio C2: Un data frame con 4 vectores previamente dados.

v1 <- c(4.5,5,3,2)
v3 <- c("Ana", "Beto", "Carolina", "Daniel")
v2 <- factor(c("Si", "No", "Si", "No"))
v4 <- c(TRUE, FALSE, TRUE, TRUE)

EC2 <- data.frame(v1,v2,v3,v4)
colnames(EC2) <-  c("numero", "SiNO", "nombre", "lógico")

#Ejercicio C3: Obtener la estructura del data frame 2

str(EC2)


#Ejercicio C4 : Obtén el resumen estadístico y naturaleza de los datos del dataframe del ej. C2


summary(EC2)


#Ejercicio C5: Extra una columna específica del dataframe del ej. C2 usando su nombre de columna

EC2$nombre

#Ejercicio C6: Extrae las primeras 2 filas del dataframe C2.
EC2[1:2,]
EC2[,1:2]


#Ejercicio C7: Extraer la 3er y 5ta dila de la 1er y 3er columna del dataframe 2

EC2[c(3,4),c(1,3)] ##Revisar

#Ejercicio C8: Añadir una quinta columna al dataframe 2.

numero2 <- c(6,3,5,2)

EC2 <- cbind(EC2,numero2)
EC2


#Ejercicio C9: Añadir más filas al data frame 2

datosextra <- data.frame(8.4, factor("Si"), "Federico", FALSE, 4)
names(datosextra) <- c("numero", "SiNO", "nombre", "lógico", "numero2")

EC2 <- rbind(EC2, datosextra)
EC2


#ejercicio extra

numero <- c(23,45,23,34,56)
EC2 <- cbind(EC2,numero)
EC2
EC2$numero
 #solo te da la primer columna con ese nombre, no la segunda ):

#Ejercicio C12: Revisa que 
state.center
as.data.frame(state.center)

VADeaths
is.data.frame(VADeaths)
class(VADeaths)

EC15 <- as.data.frame(VADeaths)
EC15

Total <- rowSums(EC15)
Total


data.frame(Total, EC15)

getwd()





#ejemplos If /else

if(4>2){
  "verdadero"
}
if(3>5){
  "Verdadero"
}
if(4>2){
  "verdadero"
}else{"Falso"}

if(3>5){
  "verdadero"
}else{"Falso"}


x <- 40
y <- 40

if(x>y){
  "verdadero"
}else{"Falso"}



if(x>y){
  "verdadero"
}else if(x==y){
  "iguales"
}else{"Falso"}


vec <- 1:10

if(vec < 3){
  "verdadero"
}else{
  "Falso"
}

vec <3 
ifelse(vec<3,"Verdadero", "Falso")
ifelse(vec<3, "Menor que 3", ifelse(vec ==3, "Igual a 3", "Mayor que 3"))

x <- c(2,3,4,5,6,7,8,9)

  
my_mean <- function(x) {
  count = 0
  sum = 0
  length(x)

  for(i in 1:length(x)){
    #i= 2 
    count <- count +1
    sum <- sum + x[i]
  }
  mean_x <- sum/count
  return(mean_x)
}

y <- c(58,79,56,10)



mean_y <- my_mean(y)
mean_y

z <- c(1,2,3,4,5,6,NA,7,8,9,10)
k <- c(1,2,3,4,5,6,7,8,9,10)


length(z)

length(k)

mean_Z <-my_mean(z)


my_mean2 <- function(x){
suma <- 0
count <- 0
for(j in 1:length(x)){
  if(is.na(x[i])){
    next
  }else{
    count <- count + 1
    suma <- sum+x[i]
  }
}
mean_x <- suma/count  
return(mean_x)
}

my_mean2(z)
my_mean2(k)


datos5 <- c(1,2,6,2,6,1,63,5,2,3,0,5,3,20,45,9)
sd(datos5)












