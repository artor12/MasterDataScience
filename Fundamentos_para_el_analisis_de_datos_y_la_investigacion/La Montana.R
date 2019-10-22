#Cargamos librerías necesarias

library(queueing)

#a) Probabilidad de que haya "n" autos en el sistema
#Calculamos las probabilidades desde P0 hasta P8.Es hasta P8 porque es el máximo de 
#coches en el sistema (5 aparcados y 3 esperando)

Probabilidades <- function (n,c,e) {
  lambda <- 6
  mu <- 2
  a <- 0
  for (i in (1:(c+e))) {
    if (i <= c) {
      a <- a + (((lambda/mu)^i)/factorial(i))
    }
    else{
      a <- a + (((lambda/mu)^i)/(factorial(c)*c^(i-c)))
    }
  }
  p0 <- 1/(1+a) 
  if (n <=c){
    return((((lambda/mu)^n)/factorial(n))*p0)
  }
  else {
    return(((lambda/mu)^n)/(factorial(c)*c^(n-c))*p0)
  }
}

Probabilidades(6,5,3)

Probabilidad_todos <-for(i in 0:8) {
  print(Probabilidades(i,5,3))
}

#Guardamos las probabilidades para futuros cálculos

P0 <- 0.04811995
P1 <- 0.1443599
P2 <- 0.2165398
P3 <- 0.2165398
P4 <- 0.1624048
P5 <- 0.09744291
P6 <- 0.05846574
P7 <- 0.03507945
P8 <- 0.02104767


#b) frecuencia efectiva de llegada para autos que usen el parking

6-(6*P8)

#c) La cantidad media de autos en el parking

P1 + (2*P2) + (3*P3) + (4*P4) + (5*P5) + (6*P6)+ (7*P7) + (8*P8)

#d) El tiempo medio que espera un auto para estacionar, estando en una plaza provisional

(((P1 + (2*P2) + (3*P3) + (4*P4) + (5*P5) + (6*P6)+ (7*P7) + (8*P8)) / (6-(6*P8)))-1/2)*60 #minutos

#e) Cantidad promedio de plazas de estacionamiento ocupadas

P1 + (2*P2) + (3*P3) + (4*P4) + (5*P5)


