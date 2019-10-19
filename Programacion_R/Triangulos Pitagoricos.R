# PROGRAMMING WITH R

### 1. Pythagoras triplets  ###

a<- 34 #Crea el objeto a
b<- 12 #Crea el objeto b
h<- 66 #Crea el objeto h

# Check by calculations
a*a+b*b #Operacion con los objetos a y b
h*h     #Operación con el objeto h
# Better with a function

terna <-function (a, b, h){ #function nos indica que lo siguiente es una función con unos parametros. Entre llaves se mete el cuerpo de la función (lo que va a pasar cuando "llamemos" al objeto terna)
  if (a*a+b*b==h*h) {print('Yes, it is a pythagoras triplet')}
  else {print(' No, this is not a p.t.')}  # Creamos un condicional por el cual si se cumple el teorema de Pitagoras con a,b y h, nos dice "Yes, it is a pythagoras triplet" y "No, this is not a p.t." si no lo cumple
}                                         #Esta función condicional la creamos como un objeto llamado "terna"
# lets try with some examples
terna(a,b,h) #Se prueba el condicional para los actuales valores de a, b y h. Vemos que no cumple Pitagoras. Si no hubiese valores a, b y h asignados, no podria ejecutarse
terna(3,4,5) #Se prueba el condicional para los valores a=3,b=4 y h=5. Vemos que cumple Pitagoras
# 
terna(12, 34, 66) #Se prueba el condicional para los valores a=12,b=34 y h=66. Vemos que no cumple Pitagoras. 
terna(40, 9, 41)  #Se prueba el condicional para los valores a=40,b=9 y h=41. Vemos que cumple Pitagoras


### 2.  multiple Pythagoras triplets by brute force ###
#2.1 check all triplets with legs between 1-n
ternasenm_n<-function(n){ #Creamos el objeto ternasenm_n que hace el siguiente bucle cuando se especifica un número n
  t<-0 #Se pone a cero el valor de t (numero de resultados que ha encontrado)
  for (a in 1:n){   #Se indica que el valor de a debe estar entre 1 y n. Dentro de la función, a es una variable local, es decir, es gnérica, no toma el valor especificado en el workspace
    for (b in 1:n) { #Se indica que el valor de b debe estar entre 1 y n. Dentro de la función, a es una variable local, es decir, es gnérica, no toma el valor especificado en el workspace
      h<-sqrt(a*a+b*b) #Se indica que h debe ser la raiz cuadrada de a*a+b*b
      if (h==round(h)){ #Se indica que h debe ser un número entero, ya que ponemos la condición de que el resultado de la raiz debe ser igual a un numero entero para que lo considere para el listado resultante
        t<-t+1;   #Le suma al t anterior un 1 cada vez que encuentra un resultado que cumpla las condiciones
        print(c(a,b,h))} #Muestra los valores que arroja el bucle y que cumplen los criterios establecidos
    }
  }
  print(t) #Indica que muestre el número de resultados que ha encontrado
}
ternasenm_n(20) #Ejecutamos el objeto ternasenm_n para un n=20 (es decir, lado máximo de a y b que sea 20)
#2.2 check all triplets with legs between n-m
ternasenm_n<-function(m, n){   #Hace lo mismo que el #2.1 pero establecemos el rango inferior de a y b con un número m, pudiendo ir ahora el rango desde m a n y no de 1 a n como antes
  t<-0
  for (a in m:n){        #Se indica que el valor de a debe estar entre m y n
    for (b in m:n) {     #Se indica que el valor de b debe estar entre m y n
      h<-sqrt(a*a+b*b)
      if (h==round(h)){ 
        t<-t+1;
        print(c(a,b,h))}
    }
  }
  print(t)
}
ternasenm_n(1, 20)
#2.3 avoiding repetitions       
ternasenm_n<-function(m, n){
t<-0
for (a in m:n){
  for (b in a:n) {       #Evitamos las repeticiones poniendo que b no puede tomar todos los valores inferiores, sino sólo coger el valor que tome previamente a (que por su parte sigue tomando valores entre m y n de forma normal)
    h<-sqrt(a*a+b*b)
      if (h==round(h)){ 
             t<-t+1;
        print(c(a,b,h))}
    }
  }
print(t)
}
ternasenm_n(1,10000)

#3.1 Euclides equations
## this is an example
m<- 56 #Se crea objeto m
n<- 34 #Se crea objeto n
## Euclides values are: #Esablecemos las ecuaciones de Euclides para crear ternas pitagoricas. Independientemente del valor que tomen n y m, siempre obtendremos ternas pitagoricas
a<-m*m-n*n #Se crea el objeto a, que es una operación matemática simple siguiendo ecuacion de Euclides
b<-2*m*n #Se crea el objeto b, que es una operación matemática simple siguiendo ecuacion de Euclides
h<-m*m+n*n #Se crea el objeto h, que es una operación matemática simple siguiendo ecuacion de Euclides
print(c(a,b,h)) #Muestra los resultados de los objetos recien creados

## function terna can check the values
terna(a,b,h) #Vemos si los resultados cumplen Pitagoras con el objeto terna, creado al principio


#3.2 Pythagoras equations #Pitágoras mejora las ecuaciones de Euclides y simplifica necesitando únicamente una k cualquiera (no una m y n, que son dos variables) para crear ternas pitagoricas
## this is an example

k<- 87 #Creamos objeto
## Pythagoras values are:
a<-k*k-1  #Se crea el objeto a, que es una operación matemática simple
b<-2*k #Se crea el objeto b, que es una operación matemática simple
h<-k*k+1 #Se crea el objeto h, que es una operación matemática simple
print(c(a,b,h)) #Muestra los resultados de los objetos recien creados
# also we can write a function for making several  triplets
ternasPyt<-function(n){ #Se crea el objeto ternasPyt, que asigna valores en bucle a k de 1 a n, y calcula a,b y h según tome valor k
  for (k in 1:n){
  a<-k*k-1
  b<-2*k
  h<-k*k+1
  print(c(a,b,h))} #Muestra los distintos tripletes que se forman al darle valores a k
  } #No hace falta preguntar si es una terna pitagórica porque lo va a ser siempre al aplicar las ecuaciones de Pitagoras

ternasPyt(10) #Hacemos el ejemplo para un n=10, es decir, k tomara los valores del 1 al 10 en bucle
#La primera terna que muestra lleva un a=0, que podría evitarse poniendo en la linea 95 un "(k in 2:(n+1))")

# 4. Fibonacci series and Pythagoras triplets
# First items within the Fibonacci series
fib<-c(0,1,1,2,3,5,8,13,21,34,55,89) #Creamos un objeto con los datos de la sucesión de Fibonacci
#  V1 - V4 store 4 Fibonacci values in a row
v1<-89; v2<-55+89; v3<-v1+v2; v4<-v3+v2;        #Creamos formulas matemáticas siguiendo la ley de Fibonacci   
# now we use them to compute a new triplet: #Usamos los valores resultantes para crear un triplete siguindo la ecuacion de Fibonacci para ternas
a<- v1*v4 
b<-2*v2*v3
h<-v2*v2+v3*v3
# Checking the result
terna(a,b,h) #Comprobamos si el triplete resultante cumple Pitagoras. Vemos que si


## the end.

