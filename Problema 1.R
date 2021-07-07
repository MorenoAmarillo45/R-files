energía <- c(205,206,204,209,207,205,202,207,208,206,201,207,204,199,198,206,
             203,205,209,204,198,203,204,202,196,198,201,202,199,197,210,208,
             209,210,214,209,215,211,211,210)
máquina <- rep(c(rep("1",5),rep("2",5),rep("3",5),rep("4",5)),2)
operador <- c(rep(c("A","B"),20))
length(operador)              
length(energia)
length(máquina)

datos <- data.frame(máquina=máquina, operador=operador,energía=energía)

anova <- aov(energía ~ máquina*operador, data=datos)
summary(anova)

