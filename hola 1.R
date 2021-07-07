x<- c(46,53,37,42,34,29,60,44,41,48,33,40)
y <- c(12,14,11,13,10,8,17,12,10,15,9,13)
probelma_14_18 <- data.frame(Humedad = x, C_humedad = y)

ajuste_14_18 <- lm(y ~ poly(x,1,raw=T))
summary(ajuste_14_18)

ajuste_14_18$coefficients

plot(ajuste_14_18)

############################

#ALVARO EDUARDO CORDERO FRANCO

Year <- c(2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,
          2017,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016)

Month <- c(12, 11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1)

Interest_Rate <- c(2.75,2.5,2.5,2.5,2.5,2.5,2.5,2.25,2.25,2.25,2,2,2,1.75,
                   1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75)

Unemployment_Rate <- c(5.3,5.3,5.3,5.3,5.4,5.6,5.5,5.5,5.5,5.6,5.7,5.9,6,5.9,
                       5.8,6.1,6.2,6.1,6.1,6.1,5.9,6.2,6.2,6.1)

Stock_Index_Price <- c(1464,1394,1357,1293,1256,1254,1234,1195,1159,1167,1130,
                       1075,1047,965,943,958,971,949,884,866,876,822,704,719)

Interest_Rate <- c(2.75,2.5,2.5,2.5,2.5,2.5,2.5,2.25,2.25,2.25,2,2,2,1.75,1.75,
                   1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75)


#nuestra variable de salida es: Stock_Index_Price


plot(x=Interest_Rate, y=Stock_Index_Price)

model <- lm(Stock_Index_Price ~ Interest_Rate + Unemployment_Rate)

summary(model)

#si el valor P es muy pequeño, rechazo Ho
#¿qué tan pequeño? cuando p-valor < alpha 
#(recordemos que alpha puede ser 0.01 o 0.05)





