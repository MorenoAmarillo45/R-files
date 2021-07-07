
#H_0: mu=37 vs H_1: mu=/37
  n <- 100 # número de observaciones hechas
  N <- 0
  rechazos <- 0 #contador de los rechazos
  
  mu <- 37
  población <- rnorm(10000, mean = mu, sd=1)
  mu_T <- n*(n+1)/4
  sigma_T <- sqrt(n*(n+1)*(2*n+1)/24)
  Z_alpha <- qnorm(0.025, mean= 0, sd=1)
  
  
repeat{
  muestra <- sample(población, size = n)
  
  d_población <- muestra - mu

  #vector para las asignaciones de gerarquía
  cnt <- 1:n
  
  #con este for se asiga una gerarquía a cada elemento
  for(i in 1:n){
    for(j in 1:n){
      if(abs(d_población[i]) > abs(d_población[j]) | abs(d_población[i]) == abs(d_población[j])){
        cnt[i] = cnt[i] +1
      }
    }
  }

  #con este for se les agrega el signo a cada gerarquía, según corresponda
  for(i in 1:n){
    if(d_población[i]< 0){
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
  
  #Estadístico Z
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

