library(ggplot2)
library(tidyverse)

población <- rgamma(n=1000000,shape = 138, scale = 2)
mean(población)
var(población)


muestra_1.5 <- replicate(n=5, sample(población, size = 500))
media_1.5 <- apply(muestra_1.5, MARGIN = 2,FUN=mean)

muestra_1.10 <- replicate(n=10, sample(población, size =500))
media_1.10 <- apply(muestra_1.10, MARGIN = 2, FUN = mean)

muestra_1.20 <- replicate(n=20, sample(población, size =500))
media_1.20 <- apply(muestra_1.20, MARGIN = 2, FUN = mean)

muestra_1.30 <- replicate(n=30, sample(población, size =500))
media_1.30 <- apply(muestra_1.30, MARGIN = 2, FUN = mean)

par(mfrow =c(2,2))
hist(media_1.5, main = NULL, xlab = "(a)")
hist(media_1.10, main = NULL, xlab = "(b)")
hist(media_1.20, main = NULL, xlab = "(c)")
hist(media_1.30, main = NULL, xlab= "(d)")

i <- c(1:100)
for(j in 1:100){
  muestra_j <- replicate(n=j, sample(población, size = 500))
  media_j <- apply( muestra_j, MARGIN = 2, FUN=mean)
}

for(j in 1:100){
  hist(media_j[j], main = NULL, xlab = NULL, ylab = NULL)
}

hist(media_j[100], main = NULL, xlab = NULL, ylab = NULL)


media_j[30]
media_j[100]

