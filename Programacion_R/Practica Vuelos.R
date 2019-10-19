library(nycflights13)
library(tidyverse)

vuelos <- nycflights13::flights
jan1 <- filter (flights, month == 1, day == 1) #Filtramos solo para que aparezcan los vuelos del 01 de Enero
dic25 <- filter (flights, month == 12, day == 25) #Filtramos solo para que aparezcan los vuelos del 25 de Diciembre


##EJERCICIO 1 FILTER

filter(flights, arr_delay>=120)->retrasos120   #Vuelos retrasados mas de 2 horas (10200)
retrasos120
filter(flights, (dest=="IAH"|dest=="HOU"))->vuelos_hou #Vuelos a HOU o IAH (9313)
vuelos_hou
filter(flights,(carrier=="AA"|carrier=="DL"|carrier=="UA")) -> vuelos_Delta #Vuelos de esas 3 companias (139504)
vuelos_Delta
filter(flights, month>=7&month<=9) -> vuelos_verano #Vuelos del verano (86326)
vuelos_verano
filter(flights, dep_delay<=0&arr_delay>120) -> vuelos_retraso #Vuelos retrasados (29)
vuelos_retraso
filter(flights, dep_delay>=60 & (dep_delay-arr_delay>30))-> vuelos_recup #Vuelos recuperados (1844)
vuelos_recup
filter(flights,hour>=0&hour<=6) -> vuelos_medianoche #Vuelos de madrugada (27905)
vuelos_medianoche

## Ejercicio 2 ##

filter(flights, between(dep_time, 000, 600))

## Ejercicio 3 ##

is.na(flights$dep_time)
sum(is.na(flights$dep_time))

## Ejercicio 4##

NA | TRUE
FALSE&NA
NA^0 #Cualquier número elevado a cero es 1
NA*0

##EJERCICIO 1 ARRANGE (Mas retraso y mas adelanto)
retraso<-arrange(flights, desc(arr_delay)) #Ordena por retraso en llegada (llegada)
head(retraso,10) #Muestra los primeros 10 vuelos (que son los que mas retraso han acumulado en llegada)

adelanto<-arrange(flights, dep_delay) #Ordena por el menos retraso en la salida (origen)
head(adelanto, 10) #Muestra los 10 vuelos mas retrasados en salida

##EJERCICIO 2 ARRANGE (Mas rapido)
rapidos<-arrange(flights, air_time)
head(rapidos,10) #Muestra los 10 vuelos mas rapidos (en cuanto a tiempo en el aire)

##EJERCICIO 3 ARRANGE (Mas largo y corto)
distancia<-arrange(flights, distance)
head(distancia, 10) #Muestra los 10 vuelos mas cortos (en distancia)
tail(distancia, 10) #Muestra los 10 vuelos mas largos (en distancia)

