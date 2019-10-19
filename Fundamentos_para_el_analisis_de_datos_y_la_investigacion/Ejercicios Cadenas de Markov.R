## Ejercicio 1 ##




library("markovchain") #Cargamos la libreria

## Apartado a) ##

empresas<- c("A", "B", "C")

byRow <- TRUE

mt <- matrix(data = c(630/700, 28/700, 42/700,105/750, 570/750, 75/750,55/550, 44/550, 451/550), byrow = byRow, #Creamos la matriz de transicion
             nrow = 3,dimnames = list(empresas, empresas))
mt

cmempresas <- new("markovchain", states = empresas, byrow = byRow, #Creamos la cadena de Markov
               transitionMatrix = mt, name = "empresas")
cmempresas #Muestra la matriz resultante

plot(cmempresas)

## Apartado b ##

## Situacion inicial ##

sinicial <- c(700/2000, 750/2000, 550/2000) #Situacion incial (mes de febrero)

#Mes de marzo ##
marzo <- sinicial * (cmempresas ^ 1)
marzo

## Apartado c ##

mayo <- sinicial * (cmempresas ^ 3)
mayo

## Apartado d ##

steadyStates(cmempresas) #Es un proceso evolutivo porque el vector que recoge mes a mes va cambiando.




### Ejercicio 2 ###




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
