#Clase del 12 de septiembre 
#Introducción a R

 #Aritmética 
  1+2
  #R es como una calculadora
  1 / (2+17)
  
  #Asignación de valor a una variable
  x <- 1+2
  y <- x/2
  #Función class
  ?class
  class(y) #¿qué clase tiene el objeto y?
  class(x=y) #El argumento x toma el valor de y
  #las dos anteriores funcionan igual pero
  #eso no sucede siempre
  class(x=class)
  
 #Vectores numéricos
  
  vec <- c(1,10,100)
  class(vec)
  #Los objetos numéricos son vectores
  #por ejemplo x, y son vectores de tamaño 1
  
  vec+10
  #se suma 10 a cada elemento de vec
  
  1:50
  #números del 1 al 50
  
  #función length, refresa la cantidad de elementos
  #en un vector
  
  length(x=10:42) #x es un parámetro de la func.
  
 #Vectores lógicos
  
  ### Func, "ls" ver variables actuales
  ls()
  ###Remove, remueve variables actuales
  rm(list = ls()) 
  
  vec <- 1:10
  logvec <- (vec >=5)  
  logvec
  class(logvec)
  vec == 4
  logical1 <- c(FALSE, FALSE, TRUE, TRUE)
  logical2 <- c(FALSE, TRUE, FALSE, TRUE)
  
  ###Operadores lógicos
  #or
  logical1 | logical2 
  #and
  logical1 & logical2
  #negación
  !logical1
  
  logical1 + logical2
  
  
 #Subconjuntos
  
  rm(list = ls())
  vec <- 101:132
  vec
  vec[1]
  vec[20]
  vec[20] <- -4
  vec
  
  #Acceder a múltimos elementos
  vec[c(1,3,5)]
  vec[5:10]
  vec[c(1,1,1)]
  5:1
  vec[length(vec):1]
  
  #Modificar multiplos elementos
  vec[1:10] <- 1
  vec
  
  #Índices negativos
  
  vec2 <- 8:12
  vec2
  vec2[-1] #muestra todos los elementos excepto el 1
  vec2[-c(1,2)] #muestra todos menos el 1ro y 2do
  
  vec2[-c(1, length(vec2))] #quitar las orillas
  
  #Índices lógicos
  vec2
  vec2[c(T,T,T,T,T)] #T es abreviasión de TRUE
  vec2[c(T,T,T,T,F)]
  vec2[c(T,F)]  #es como escribir (T,F,T,F,T)
  vec2 > 10
  vec2[vec2>10]
  vec2[vec2 > 10] <- 10
  vec2
  
  #Función sum, regresa la suma de los elementos del vector
  sum(vec2)
  
  #Vectores de caractéres 
  stringVec <- c("pera", "manzana", "naranja")
  class(stringVec)
  class("pera")
  stringVec
  
  #Función paste
  paste(stringVec, "jugo", sep=" ")
  paste("judo de", stringVec, sep=" ")
  
  #Función substr
  substr(stringVec, start = 2, stop = 4) #Toma del segundo al cuarto caracter
  
  #Función nchar
  nchar(stringVec) #cuando caracteres tienes cada elemento
  substr(stringVec, start = 2, stop = nchar(stringVec)) #
  #a veces no es necesario poner stop ni start
  substr(stringVec, 2, nchar(stringVec)) 
  
  #Función grep 
  stringVec
  indices <- grep(pattern = "an", x=stringVec)
  indices
  #devuelve indices de string con el patron 'an' dentro delvector
  
  ####Casteo
  paste("hola", 1)
  "1"+1 #no funciona esta expresión
  paste(4:12, "th", sep = "")
  
  #Función as.numeric
  x <- as.numeric("123")
  as.numeric("hola") #Na not available
  
  #función as.logical
  as.logical("TRUE") #TRUE
  as.logical("1") #Na
  as.logical(1) #TRUE
  as.logical("T") #TRUE
  
  #Función as.character
  as.character(12342)
  as.character(T)

  #Matrices y Data Frames
  rm(list = ls())
  matriz <- matrix(data = 1:30, nrow = 15, ncol = 2, byrow = T)
  #byrow es el orden en el llenas la matriz
  # por columna o por fila, puede ser Falso o Verdadero
  matriz
  class(matriz)
  #si no se llenan todos los espacios te arroja error
  
  ##Subsetteo de matrices
  matriz[2,3]#elementos en ela fila 2, columna 3
  class(matriz[2,3])
  matriz[1:2,1:2] #
  class(matriz[1:2,1:2])
  matriz[1:3, 1:3]
  matriz[,2:3]
  matriz[,]  
  
  #dame la fila i con todas las columas
  matriz[1,]
  class(matriz[1,]) #Lo castea a vector  
  matriz[1,,drop=F] #Impide el re-casteo
  class(matriz[1,,drop=F])
  
  matriz[,1]
  matriz[,1,drop=F]
  
  matCol <- matrix(1:12, nrow=3,ncol = 4, byrow = F )
  matCol
  matCol+1
  matriz
  matCol
  matriz + matCol
  matriz + matCol[,2:3] #arreglos de dimensión de compatibles
  
  matriz
  mat2 <- matrix(data = 1:12, nrow = 4, ncol = 3, byrow = T)
  mat2
  mat2 * matriz #Trata de hacer multiplicación elemento por elemento, como suma
 
  #multiplicación de matrices %*%
  matriz
  mat2
  matriz %*% mat2
  
  #se pueden tener matrices de datos lógicos
  #numericos o de caracteres
  
  ##Data frame (HOja de excel basicamente)
  ##Es una matriz donde cada columna no necesariamente
  ##tiene el mismo tipo de datos
 
  
  df <- data.frame(a=1:5, b=21:25, c=1:5+0.5)
  df ##obs.filas, variables columnas 
  class(df)
  dim(df) # número de filos, número de columna
  colnames(df)
  rownames(df)
  colnames(df)[2] <- "Nuevo"
  
  #lo anterior funciona con matrices
  dim(matriz)
  colnames(matriz)
  rownames(matriz)
  rownames(matriz) <- c(paste("fila", 1:nrow(matriz), sep = ""))
  colnames(matriz) <- c(paste("Columna",1:ncol(matriz), sep = ""))
  matriz
  
  #Operador $
  df$Nuevo
  class(df$Nuevo)
  class(df$c)
  #Agregat una nueva columna
  df$ColumnaNuevo <- 5:1
  df
  
  # la notación con $ no funciona con matrices
  
  
  #lectura de datos
  rm(list = ls())
  #wd working directory
  getwd()
  #me imprime "C:/Users/HP/Documents/R Scripts"
  #para cambiar la carpeta usamos
  #####setwd()###
  ##el argumento es la dirección 
  ##de la carpeta que ahora se quiere usar
  ## hay que cambiar manualmente \ por /
  
  #aparecen todos los archivos en la carpeta
  dir()
  
  #marcará erros porque no tengo los archivos
  USEnergy<- read.csv("USEnergy.csv",as.is=TRUE)
  class(USEnergy)
  class(USEnergy$t)
  class(USEnergy$C)
  write.csv(x=USEnergy$C,file="auxiliar.csv")
  write.csv(x=USEnergy$C,file="auxiliar2.csv",row.names = F)
  ?write.csv
  # CSV se puede usar facilmente en otros lenguajes de programacion
  # Otra opcion es usar un archivo rds
  saveRDS(object=USEnergy$C,file="EnergyC.rds")
  rel<- readRDS(file="EnergyC.rds")
  rel
  rm(list=ls())
  ## Subseteo avanzado
  USEnergy<- read.csv("USEnergy.csv",as.is=TRUE)
  indices<- (USEnergy$t==1950 | USEnergy$t==1955)
  indices
  USEnergy[indices,]
  # Operador %in%
  USEnergy$t %in% c(1950,1955,1960)
  USEnergy[USEnergy$t %in% c(1950,1955,1960),]
  
  # Funcion order
  # Indices en los que el vector esta ordenado
  indices<- order(USEnergy$t)
  indices
  indices<- order(USEnergy$t,decreasing = T)
  indices
  USEnergy[order(USEnergy$t,decreasing = T),]
  
  ## Funcion Match
  ## Indices de la primera aparicion de cada elemento
  ## de x en table
  rm(list=ls())
  fruitDataJuice <- read.csv(file="fruitData.csv")
  class(fruitDataJuice$Fruit)
  fruitNutr <- read.csv("fruitNutrition.csv", as.is=TRUE)
  indices<- match(x=fruitDataJuice$Fruit,table = fruitNutr$Fruit)
  indices
  is.na(indices)
  fruitDataJuice$Calories<- NA
  fruitDataJuice$Calories[!is.na(indices)] <- fruitNutr$Calories[indices[!is.na(indices)]]
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  