
#Preparamos las librerias necesarias

library(mosaicCalc)
library(mosaic)


#Ejercicio 1

g <- mosaicCalc::D(3*x^2 - 2*x + 4 ~ x)

# a) What is the value of the derivative at x=0?

g(0)

#La respuesta es -2

# b) What does a graph of the derivative function look like?

plotFun(g, x.lim=range(0,10)) 

# La respuesta es B


# Ejercicio 2


h <- mosaicCalc::D(5*exp(.2*x) ~ x )


# a) What is the value of the derivative at x=0?

h(0)

# La respuesta es 1

# b) Plot out both the original exponential expression and its derivative.
# How are they related to each other?


plotFun(h, x.lim=range(0,10))   # primera derivada

plotFun(5*exp(.2*x) ~ x, xlim = range(0,10))   # sin derivar

# La respuesta es B


#Ejercicio 3

j <- mosaicCalc::D(exp(-(x^2)) ~ x  )

# What does the graph look like??

j # Vemos el resultado

plotFun(j, x.lim=range(-2,2)) 

# La respuesta es C


#Ejercicio 4

# What will be the value of this derivative?

p <- mosaicCalc::D(fred^2 - ginger ~ x)

p

#La respuesta es A


# Ejercicio 5

# a)

r<-mosaicCalc::D(cos(2 * t) ~t) #Primera derivada
r

dr<-mosaicCalc::D(r(t) ~t) #Segunda derivada
dr

ddr<-mosaicCalc::D(dr(t) ~t) #Tercera derivada
ddr

# La respuesta es D

#b)

dddr<-mosaicCalc::D(ddr(t) ~t) #Cuarta derivada
dddr

# La respuesta es E


#Ejercicio 6

# a)Computar y hacer la grafica de la cuarta derivada de la funcion cos(2*t^2 ~ t)

# desde t = 0 hasta t = 5. ¿Como es la grafica?

pri <- mosaicCalc::D(cos(2*t^2) ~ t)

seg <- mosaicCalc::D(pri(t) ~ t)

ter <- mosaicCalc::D(seg(t) ~ t)

cua <- mosaicCalc::D(ter(t) ~ t)

cua

plotFun(cua, x.lim=range(0, 8)) 

# La respuesta es C


#b) La respuesta es C


## Ejercicio 7 ##


parcialx <- mosaicCalc::D(x*sin(y) ~ x)

parcialy <- mosaicCalc::D(x*sin(y) ~ y)

parcialx2 <- mosaicCalc::D(sin(y) ~ x)

parcialy2 <- mosaicCalc::D(x * cos(y) ~ y)

pxy <- mosaicCalc::D(x*sin(y) ~ x&y)

pyx <- mosaicCalc::D(x*sin(y) ~ y&x)

parcialx(2,3)

parcialy(3,2)

parcialx(1,0)

parcialy(0,1)

# El primer apartado es FALSO

parcialx2(3,5)

parcialy2(5,3)

parcialx2(2,1)

parcialy2(1,2)

# El segundo apartado es FALSO

pxy(0,1)

pyx(1,0)

pxy(5,3)

pyx(3,5)

# El tercer apartado es VERDADERO
