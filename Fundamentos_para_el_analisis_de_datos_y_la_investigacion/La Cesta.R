## Practica La Cesta ##

# Preparamos librerias y datos

library (readr)
clientes_data<-read.csv("C:/Users/atorn/Desktop/Máster/Primer cuatrimestre/Fundamentos para el análisis de Datos y la Investigación/Casos prácticos/Caso La Cesta/LA CESTA.csv")

#Vemos los datos de entrada de clientes al supermercado

view(clientes_data)

#Calculo lOs principales datos estadisticos (cuartiles, media, minimo, maximo, mediana y varianza)

summary(clientes_data$x)
media <- mean(clientes_data$x)
var(clientes_data$x)
desv_tip <- sd(clientes_data$x)

#Calculamos las probabilidades que se piden:

# a) Que en un minuto no entre ningun cliente

dpois(x=0, lambda = 0.78)

# b)Que en un minuto accedan entre dos y cinco clientes

sum(dpois(x=2:5, lambda = 0.78))
ppois(2:5, 0.78)


# c)Que en 5 minutos accedan mas de 10 clientes

#Seguimos aplicando Poisson

mean(clientes_data$x)*5

dpois(x=10, lambda=3.9)


#Vemos que es importante conocer entre que valores se podria encontrar el numero medio de clientes accediendo a la cola, con un nivel de confianza del 95%

alpha<- 0.05
cuantil<- qnorm(1 - alpha/2)
lim_inf<- media - cuantil * sqrt(desv_tip^2) / sqrt(600)
lim_inf
lim_sup<- media + cuantil * sqrt(desv_tip^2) / sqrt(600)
lim_sup

# d) Uno de  los miembreos consigue datos relativos de otra empresa, la cual contiene 500 registros, media 0.69 y desviacion tipica 0.96. Los miembros del equipo discuten entre la similitud de ambos comercios.

comp<-rnorm(500, mean = 0.69, sd = 0.96)
comp<-as.data.frame(comp)

dif_mean <- t.test(comp, clientes_data$x)
dif_mean


