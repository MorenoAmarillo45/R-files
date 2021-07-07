X<- c(46,53,37,42,34,29,60,44,41,48,33,40)
Y <- c(12,14,11,13,10,8,17,12,10,15,9,13)

plot(x=X,y=Y)

beta <- cov(X,Y)/var(X)
alpha <- mean(Y) - beta*mean(X)

ajuste <- alpha + beta*X
lines(x=X,y=ajuste)

y_38 <- alpha + beta*38




