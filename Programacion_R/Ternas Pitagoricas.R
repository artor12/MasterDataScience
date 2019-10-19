### Ejercicio 1 ###

fibonacci <- function(n) { #Creamos una funcion con condicioales, ya que los primeros terminos no funcionan de igual manera que a partir del tercero. A partir del tercero, como todos siguen la misma formula, se puede poner un bucle
  if (n < 1) {
    stop("El menor termino de la sucesion de fibonnaci es el 1. Introducir un n igual o superior a 1"). #Pongo esta linea con # porque no me deja generar el HTML
  } 
  
  if (n < 3) { #Con este condicional obtenemos los dos primeros terminos de la sucesion de Fibonacci. Dependiendo de si el n otorgado es 1 o 2, devolvera uno o dos resultados
    return(c(0, 1)[1:n])
  } else {
    
    sucesion <- numeric(n)
    sucesion[2] <- 1 #Indicamos que el segundo termino de la sucesion es siempre igual a 1
    for (i in 3:n) { #Usamos el bucle for, que ira moviendo el puntero desde el 3 hasta el n que se indique 
      sucesion[i] <- sucesion[i - 2] + sucesion[i - 1] #Formula para calcular los terminos de Fibonacci a partir del tercero
    }
    return(sucesion) #Muestra el resultado de la sucesion
  }
}
fibonacci(0) #Como el primer numero de la sucesion de Fibonacci es el termino 1, si introducimos un n menor que 1, se pide que se introduzca una n mayor o igual a 1.
fibonacci (15)

ternas<-function(n){
  cuatro<-c(fibonacci(n+4)) 
  cuatro<-cuatro[-1]
  m<-matrix(nrow=n,ncol=3) #Creamos una matriz para que nos muestre los resultados de forma ordenada
  for (i in 1:n) {
    v1<-cuatro[i]
    v2<-cuatro[i+1]
    v3<-cuatro[i+2]
    v4<-cuatro[i+3]
    a<- v1*v4 
    b<-2*v2*v3
    h<-v2*v2+v3*v3
    m[i,]<-c(a,b,h) #Asignamos los valores a la matirz
    
  }
  print(m) #Pinta la matriz
}
ternas(5) #Comprobamos resultados

