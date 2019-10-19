# Individual work with  DATA FRAME

#1. Creating a new data frame
edad <- c(22, 34, 29, 25, 30, 33, 31, 27, 25, 25) #Se asignan valores a la variable edad y se crea como objeto
tiempo <- c(14.21, 10.36, 11.89, 13.81, 12.03, 10.99, 12.48, 13.37, 12.29, 11.92) #Se asignan valores a la variable tiempo y se crea como objeto
sexo <- c("M","H","H","M","M","H","M","M","H","H") #Se asignan valores a la variable sexo y se crea como objeto
misDatos <- data.frame(edad,tiempo,sexo) #Creamos un objeto con la tabla resultante, que incluye edad, tiempo y sexo
misDatos #Vemos cómo queda la tabla 
#Al crear el objeto "misDatos", deja de ser necesario el tener "edad","sexo" y "tiempo" en "Values" (a la derecha), por lo que se pueden eliminar:
rm(list=c("edad","tiempo","sexo")) #Limpiamos la lista de "values"
str(misDatos)    # Estructura de 'misDatos' #Muestra el número y el tipo de observaciones para cada variable
names(misDatos)  # Nombre de las variables contenidas en 'misDatos'
#2. Checking variables and data within the data frame
misDatos[3:6,] #Muestra las observaciones de las filas numero 3 a la numero 6 incluidas
misDatos[,1] #Muestra únicamente los datos de la columna 1, en este caso corresponden a la edad
misDatos$edad #Muestra únicamente los datos de la lista edad
misDatos[,"edad"] #Muestra únicamente los datos de la columna con el nombre "Edad"

#3. Some operations 
mean(misDatos[,1]) #Realiza la media de la columna 1
mean(misDatos$edad) #Realiza la media de la lista que se llame "Edad"
mean(misDatos[,"edad"]) #Relaiza la media de la columna con el nombre "Edad"
mean(misDatos[["edad"]]) 
#4. Functions ATTACH and DETACH
attach(misDatos) #Asigna una prioridad de busqueda de variables en "values" (a la derecha) y luego en "Data"
table(tiempo) #Muestra las frecuencias frecuencias absolutas de forma ordenada para la variable tiempo
table(sexo) #Muestra las frecuencias absolutas de la variable sexo
table(edad) #Muestra las frecuencias frecuencias absolutas de forma ordenada para la variable edad
table(sexo,edad) #Muestra las frecuencias absolutas de dos variables (sexo y edad)
mean(edad[sexo=="M"]) #Calcula la media de la edad de las mujeres
mean(edad[sexo=="H"]) #Calcula la media de la edad de los hombres
detach(misDatos) #Deshacer el attach

#5. New dataframe 'currencies' store some currencies
currencies <- data.frame(amount=c(1,2),currency=c("Dolar", "Euro"), exchange=c(1, 0.9)) #Creamos un nuevo objeto compuesto por las variables "Amount", "Currancy" y "Exchange". A cada variable le corresponden dos observaciones
currencies #Muestra el objeto
Countries<-data.frame(names=c("UK","Spain","Russia"),currency=c("Pound", "Euro", "Rublo"), exchange=c(1.2, 1, 0.02)) #Creamos un nuevo objeto compuesto por tres variables y tres observaciones
attach(currencies) #Indica que buscará de forma preferente las variables en "currencies" 
currency          #Muestra el objeto
attach(Countries)  # Indica que ahora pasa a buscar las variables de forma preferente en "countries"
currency          # Muestra currency del objeto "Countries" porque está este objeto en busqueda preferente por el attach
exchange          # Muestra exchange del objeto "Countries" porque está este objeto en busqueda preferente por el attach
amount   # Muestra amount del objeto "Currencies" ya que aunque la busqueda preferente está en "Countries", este objeto no contiene la variable "Amount"

detach(Countries)  # Elimina la busqueda preferente en "Countries" y vuelve a la busqueda preferente en "currencies" ya que en este objeto aún no hemos hecho detach
currency          # Al hacer detach en "countries" y no haberlo hecho en "currencies", busca preferentemente en "currencies" y nos muestra la variable "currency" de "currencies"
detach(currencies) # Deshacemos el attach del objeto "currencies"
#currency          # Error. No podría darnos una respuesta porque no sabe dónde buscar preferentemente debido a los dos desatach. (lo dejamos como #currency para poder ejecutar el HTML y que a la hora de compilar no de errores)

#6. In short
longitud<-c(12,10,11,13,14,17) #Creamos un nuevo objeto
medidas<-data.frame(longitud=c(6,4,7), peso=c(240,326,315), diametro=c(8,9,9)) #Creamos un nuevo objeto compuesto por 3 variables y 3 observaciones
mean(longitud) #Obtenemos la media del objeto longitud
mean(medidas$longitud) #Obtenemos la media de la variable "longitud" dentro del objeto "medidas"
mean(medidas$peso) #Obtenemos la media de la variable "peso" dentro del objeto "medidas"
mean(medidas$diametro) #Obtenemos la media de la variable "diametro" dentro del objeto "medidas"

attach(medidas) #Establecemos la búsqueda preferente en el objeto "medidas"
mean(peso) #Indica la media del peso teniendo en cuenta en attach en "medidas"
mean(diametro) #Indica la media del diametro teniendo en cuenta en attach en "medidas"
mean(longitud) #Indica la media de la longitud teniendo en cuenta en attach en "medidas"
detach(medidas) #Deshacemos la búsqueda preferente en el objeto "medidas"

# 7. Function WITH      #Creamos unas variables temporales (no van a quedar guardadas) y locales (dentro de With) (en enste caso vol y dens)
with(medidas,{
  vol=longitud*pi*(diametro/2)^2  # volume
  dens=peso/vol               # density
  dens       # local variable                   
})

medidas
medidas$dens<-with(medidas,{                 #Añadimos la densidad al objeto "medidas"
  vol=longitud*pi*(diametro/2)^2  # volume
  dens=peso/vol               #  density
  dens                        
})
medidas # 

#8. SUBSET
hombres<-subset(misDatos,sexo=="H") #Nos quedamos únicamente con los datos procedentes de hombres
hombres
mujeres<-subset(misDatos,sexo=="M") #Nos quedamos únicamente con los datos procedentes de mujeres
mujeres

mayores<-subset(misDatos,sexo=="H" & edad>30) #Nos quedamos únicamente con los datos procedentes de hombres mayores de 30 años
mayores

jov_habladores<-subset(misDatos,sexo=="H" & edad<30 & tiempo>12) #Nos quedamos únicamente con los datos procedentes de hombres menores de 30 años y tiempo superior a 12

extremos<-subset(misDatos,edad<25|edad>30) #Nos quedamos con los datos que cumplan menos de 25 años o mayores de 30 años
extremos

hombres<-subset(misDatos,sexo=="H", select=c(edad, tiempo)) #Indica la edad y el tiempo filtrado para que aparezcan unicamente los hombres
hombres


# 9. MERGE - RBIND

animales1 <- data.frame(animal=c("vaca","perro","rana","lagarto","mosca","jilguero"), 
                       clase=c("mamÃ­fero","mamÃ­fero","anfibio","reptil","insecto","ave")) #Crea una tabla con las variables animal y clase
animales1
animales2 <- data.frame(animal=c("atÃºn", "cocodrilo", "gato","rana"), clase=c("pez", "reptil", "mamÃ­fero","anfibio")) #Crea una tabla con las variables animal y clase
animales2
animales3 <- rbind(animales1,animales2)  #Concatena las dos tablas de animales
animales3
animales4<-merge(animales1,animales2) #Muestra los datos repetidos (tantas veces como aparezcan repetidos)
animales4
animales5<-merge(animales1,animales2,all=TRUE) #Muestra todos los datos suprimiendo las filas repetidas
animales5
superficieAnimales=data.frame(animal=c("perro","tortuga","jilguero", 
                                       "cocodrilo","vaca","lagarto","sardina"),
                              superficie=c("pelo","placas Ã³seas","plumas",
                                           "escamas","pelo","escamas","escamas"))
superficieAnimales
merge(animales3,superficieAnimales) # Muestra solo los animales comunes a ambos dataframes
merge(animales3,superficieAnimales, all.x=TRUE) # Muestra todos los animales del primer dataframe.
merge(animales3,superficieAnimales, all.y=TRUE) # Muestra todos los animales del segundo dataframe.
merge(animales3,superficieAnimales, all=TRUE) # Muestra todos los animales de ambos dataframes.

# 10. sorting DATAFRAMES
ordenacion<-order(animales1$animal) #Ordena los datos siguiendo un criterio de ordenación y crea a su vez un objeto nuevo
ordenacion  # Posiciones dentro del dataframe 'animales1' de los animales ordenados alfabÃ©ticamente
animales1<-animales1[ordenacion,]  # Se reordenan las filas del dataframe animales1
animales1
animales1<-animales1[order(animales1$animal),]  #Cambia el orden del objeto "animales1" según el criterio especificado
misDatos<-misDatos[order(misDatos$edad,misDatos$tiempo),] #Cambia el orden del objeto "misDatos" según los criterios especificados
misDatos #Visualizamos el objeto
