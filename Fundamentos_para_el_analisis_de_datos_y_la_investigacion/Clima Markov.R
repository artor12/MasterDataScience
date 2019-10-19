
################### EJERCICIO CLIMA #####################################

library("markovchain") #Cargamos la libreria

estados<- c("soleado", "nublado", "lluvioso") #Asignamos nombres
byRow <- TRUE #Indicamos que queremos construir la matriz por filas
mt <- matrix(data = c(0.60, 0.3, 0.1,0.4, 0.4, 0.2,0.25, 0.45, 0.30), byrow = byRow, #Creamos la matriz de transicion
             nrow = 3,dimnames = list(estados, estados))
cmclima <- new("markovchain", states = estados, byrow = byRow, #Creamos la cadena de Markov
               transitionMatrix = mt, name = "clima")
cmclima #Muestra la matriz resultante

plot(cmclima)

sinicial <- c(1, 0, 0) #Situacion incial como soleado
dosdespues <- sinicial * (cmclima ^ 2) #Posibilidades del tiempo a los dos dias
sietedespues <- sinicial * (cmclima ^ 7) #Posibilidades del tiempo a los siete dias
dosdespues
sietedespues

#Estado estacionario
steadyStates(cmclima) #Si existe el estado estacionario (o estable), lo arroja, sino, da error

#Simulacion aleatoria del estado del clima durante 100 días (comprobar que >n... <DS)
simulacionclima <- rmarkovchain(n = 1000, object =cmclima , t0 = "soleado") #El "r" antes del nombre de la funcion indica que es una simulacion. Indicamos la situacion de partida como soleado

#simulacion del estado del tiempo proxima semana
simulacionclima[1:7] #Simulacion unicamnete arrojando los primeros siete dias

#Estimador maximo verosimilitud (mle=maximum likelihood estimator)
emvclima <- markovchainFit(data = simulacionclima, method = "mle", name = "EMVclima") # "mle" significa maxima verosimilitud                                
#markovchainFit, devuelve una CM de una secuencia dada.

emvclima$estimate #Estimaciones

emvclima$standardError #Desviaciones tipicas

