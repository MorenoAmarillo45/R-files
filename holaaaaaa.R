
#H_0: mu=37 vs H_1: mu=/37
  n <- 100 # n�mero de observaciones hechas
  N <- 0
  rechazos <- 0 #contador de los rechazos
  
  mu <- 37
  poblaci�n <- rnorm(10000, mean = mu, sd=1)
  mu_T <- n*(n+1)/4
  sigma_T <- sqrt(n*(n+1)*(2*n+1)/24)
  Z_alpha <- qnorm(0.025, mean= 0, sd=1)
  
  
repeat{
  muestra <- sample(poblaci�n, size = n)
  
  d_poblaci�n <- muestra - mu

  #vector para las asignaciones de gerarqu�a
  cnt <- 1:n
  
  #con este for se asiga una gerarqu�a a cada elemento
  for(i in 1:n){
    for(j in 1:n){
      if(abs(d_poblaci�n[i]) > abs(d_poblaci�n[j]) | abs(d_poblaci�n[i]) == abs(d_poblaci�n[j])){
        cnt[i] = cnt[i] +1
      }
    }
  }

  #con este for se les agrega el signo a cada gerarqu�a, seg�n corresponda
  for(i in 1:n){
    if(d_poblaci�n[i]< 0){
      cnt[i] <- -1*cnt[i]
    }
  }
  
  #contamos las transformaciones positivas, T+ 
  Tmas <- 0
  for(i in 1:16){
    if(cnt[i]>0 | cnt[i]==0){
      Tmas = Tmas + cnt[i]
    }
  }
  
  #Estad�stico Z
  Z_T <- (Tmas - mu)/sigmma_T
  
  #Contador de los rechazo
  if(Z_T < -Z_alpha | Z_T > Z_alpha){
  rechazos = rechazos + 1
  }
  
  N = N+1
  if(N == 101){
    break
  }
}

