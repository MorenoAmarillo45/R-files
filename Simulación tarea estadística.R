
library(ggplot2)
library(tidyverse)

help("Distributions")

población <- rbinom(350000, 5, 0.25)
head(población)
mean(población)
var(población)



muestra <- sample(población, size= 100)
head(muestra)
mean(muestra)

muestras <- replicate(n=5, 
                       sample(población, size=100))

dim(muestras)
medias <- apply(muestras, MARGIN = 2, FUN = mean)
head(medias)
mean(medias)


muestras2 <- replicate(n=10, 
                      sample(población, size=100))
dim(muestras2)
medias2 <- apply(muestras2, MARGIN = 2, FUN = mean)
head(medias2, 10)
mean(medias2)
hist(medias2)


muestras3 <- replicate(n=20, 
                       sample(población, size=100))
dim(muestras3)
medias3 <- apply(muestras3, MARGIN = 2, FUN = mean)
head(medias3, 20)
mean(medias3)
hist(medias3)


muestras4 <- replicate(n=30, 
                       sample(población, size=100))
dim(muestras4)
medias4 <- apply(muestras4, MARGIN = 2, FUN = mean)
head(medias4, 30)
mean(medias4)
hist(medias4)

muestras5 <- replicate(n=80, 
                       sample(población, size=100))
dim(muestras5)
medias5 <- apply(muestras5, MARGIN = 2, FUN = mean)
head(medias5, 50)
mean(medias5)
hist(medias5)









