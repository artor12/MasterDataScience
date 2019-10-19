library(readr)
babymales<-read.csv("ontario_top_baby_names_male_1917-2016_english.csv",skip=1) #Cambiamos el nombre a la tabla
getwd()
#rm(list = "ontario_top_baby_names_male_1917-2016_english") #Eliminamos la tabla a la que le acabamos de cambiar el nombre, para no tenerla repetida
head(babymales) #Muestra los primeros valores de la tabla
tail (babymales) #Muestra los ultimos valores de la tabla
max(babymales$Year) #Muestra el año más reciente del que se tiene datos
nombresmasrecientes<-babymales[babymales$Year==max(babymales$Year),] #Solo coge los datos que en el campo Year tengan el máximo
nrow(nombresmasrecientes) #Da el numero de nombres registrados en el último año
nombresmasrecientes<-subset(babymales,babymales$Year==2016) #Hace lo mismo que lo anterior pero de otra forma
nrow(nombresmasrecientes)
nombresmasrecientes<-nombresmasrecientes[,c("Name","Frequency")] #Como en la lista todo tiene el mismo año (2016), podemos filtrarlo para que no aparezca, entonces seleccionamos todas las filas (con el  ,) y mantenemos las columnas de Nombre y Frecuencia
head(nombresmasrecientes,2) #Vemos los dos primeros datos de la tabla. Vemos que ya no aparece el año
nombresmasrecientes<-nombresmasrecientes[order(nombresmasrecientes$Frequency,decreasing = TRUE),] #Ordena los datos de forma decreciente por frecuencia, y actualiza el objeto
write.csv(nombresmasrecientes,file="nombres_populares_2016.csv") #Guardamos la tabla como un archivo csv. Por defecto se va a guardar en la carpeta del proyecto. Hay que poner siempre la extensión del archivo (csv en este caso)
head(nombresmasrecientes,5) #Vemos los primeros 5 datos resultantes, en este caso los nombres mas frecuentes
#nombre.deseado<-readline(prompt="¿Que nombre quiere buscar?: ") #Aparece este mensaje en pantalla, para que se escriba algo justo después en la consola (en este caso un nombre)
nombre.deseado<-"Armando" #Hacemos este paso para poder generar el HTML, pero el correcto es el anterior y este debe desaparecer
freq.year<-babymales[babymales$Name == toupper(nombre.deseado),c("Year","Frequency")] #Toupper lo que hace es poner todo el texto que se introduzca en mayusculas. La función lo que hace es devolver el dato de cuántas veces se ha repetido el nombre introducido en cada año. Muestra los datos en el objeto "freq.year"
plot.title<-paste("Bebes llamados", toupper(nombre.deseado)) #Cambiamos el título del plot (el grafico)
plot(freq.year, main=plot.title, type='s') #Creamos un gráfico con la frecuencia por años, el titulo que acabamos de programar y la "s" significa que queremos que el dibujo sea escalonado
