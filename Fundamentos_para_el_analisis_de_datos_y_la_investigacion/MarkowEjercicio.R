#### EJERCICIO 1 ####

library(markovchain)

estados <- c("A","B","C")
byRow <- TRUE

mt <- matrix( data = c(0.9, 0.04, 0.06, 0.14, 0.76, 0.1, 0.1, 0.08, 0.82), nrow = 3, byrow = byRow,
              dimnames = list(estados, estados))

cuota <- new("markovchain", states = estados, byrow = byRow, 
             transitionMatrix = mt, name = "cuota")

#caracteristicas generales de la cadena de markov
cuota

#situacion inicial en febrero
inicial <- c(700/2000,750/2000,550/2000)

#en marzo
marzo <- inicial * cuota
marzo

#en mayo
mayo <- inicial * cuota^3
mayo

#es un proceso evolutivo, el vector que recogemos mes a mes va cambiando

#buscamos el estado estacionario
steadyStates(cuota)


#### EJERCICIO2 ####

medios <- c("Prensa", "TV")

mt <- matrix(data = c(0.7, 0.3, 0.4, 0.6), nrow = 2, byrow = TRUE,
                 dimnames = list(medios, medios))

anuncio <- new("markovchain", states = medios, byrow = TRUE,
               transitionMatrix = mt, name = "anuncio")
anuncio

#situacion inicial
inicial <- c(1, 0)

##NOTA: al ser dicotomico 1,0 o 0,1 no haria falta multiplicar por el vector inicial
#pero vamos, que si lo haces aseguras

#que en 2 meses se vuelva a anunciar en prensa
result1 <- inicial * anuncio^2
result1

#que se anuncie en TV al cabo de 3 meses
result2 <- inicial * anuncio^3
result2

#dadas estas condiciones que se anuncie 2 meses seguidos en TV // el de ahora y otro
result3 <- result2 * 0.6
result3

#si fuese 3 meses seguidos TV, por ejemplo
result4 <- result2 * (0.6)^2
result4

#buscamos el estado estacionario
steadyStates(anuncio)
