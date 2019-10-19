# EJERCICIO  1
#1. INTRODUCIR DATOS Y CREAR OBJETOS CON METADATOS
edad<-c(18,19,NA,18,24,17,22,15,22,25,NA,16,23,16) #Se asignan valores a la variable edad y se crea como objeto
sexo<-c(0,1,0,0,1,0,0,1,1,0,0,1,0,1) #Se asignan valores a la variable sexo y se crea como objeto
estudios<-c(1,2,0,1,3,2,3,1,2,3,1,2,3,0) #Se asignan valores a la varible estudios y se crea como objeto
sexo<-factor(sexo, levels=c(0,1),
            labels=c("Hombre","Mujer"))     #En la variable sexo, hacemos que 0 sea Hombre y 1 Mujer y con ello modificamos el objeto
estudios<-factor(estudios, levels=c(0,1,2,3),
                labels=c("Ninguno","Primarios",
                         "Secundarios","Superiores")) #Asignamos a los numeros de estudios la etiqueta del nivel de estudios que corresponden y a su vez modificamos el objeto
#2. ESTUDIAR LOS DATOS: TABLA DE FRECUENCIAS UNIDIMENSIONALES
table(edad) #Vemos la frecuencia absoluta por edad
prop.table(table(edad)) #Vemos la frecuencia relativa por edad
table(edad,useNA="ifany") #Vemos la frecuencia absoluta por edad, incluyendo los NA

#3.ESTUDIAR LOS DATOS:  TABLA DE REFERENCIAS CRUZADAS
table(estudios,sexo) #Tabla de frecuencia aobsoluta mostrando dos variables a la vez, estudios y sexo
prop.table(table(estudios,sexo)) #Tabla de frecuencias relativas considerando dos variables
prop.table(table(estudios,sexo),1) #Tabla de frecuencias relativas según el nivel de estudios
prop.table(table(estudios,sexo),2) #Tabla de frecuencias relativas según el sexo

#4. ESTUDIAR LOS DATOS: SIMPLIFICACAR USANDO OBJETOS DE ALMACENAMIENTO
t<-table(estudios,sexo) #Creamos el objeto t
prop.table(t) #Lo mismo que linea 18 pero ahorrando memoria
prop.table(t,1) #Lo mismo que linea 19 pero ahorrando memoria
prop.table(t,2) #Lo mismo que linea 20 pero ahorrando memoria

#5. AGRUPAR DATOS PARA VARIABLES CONTINUAS
range(edad,na.rm=TRUE) #Con na.rm estamos diciendo que no cuente con los NA para hacer el cálculo
#La línea 29 nos da el máximo y el mínimo de edad
#Con la siguiente función vemos en cuantos grupos vamos a agrupar los datos
nc<-nclass.Sturges(edad)  # N. de intervalos+1 (limites) #Creamos objeto
nc #Muestra el numero de intervalos
lc<-seq(15,25,length=nclass.Sturges(edad))  # Limites de los intervalos-clases #Creamos el objeto lc
lc #Vemos que los grupos serían de 15-17.5 , 17.5-20 , 20-22.5 y 22.5-25


#6. CONSTRUIR INTERVALOS CON cut():

?cut #Vemos info sobre la función cut

intervalosEdad<-cut(edad,breaks=seq(15,25,length=nc),include.lowest=TRUE) #Vemos a qué intervalo corresponde cada observación de la edad, incluyendo el límite inferior del primer intervalo, y creamos un obejo con esto
intervalosEdad # Se muestran los intervalos de edad, uno correspondiente a cada edad observada

table(intervalosEdad) #Muestra las frecuencias absolutas de cada intervalo


#7. ESTADISTICOS DESCRIPTIVOS
mean(edad,na.rm=TRUE) #Calcular media sin contar los NA
sd(edad,na.rm=TRUE) #Calcular desviación típica sin contar los NA
summary(edad) #Hace un resumen de los principales estadisticos para la edad (mínimo, máximo, media, cuartiles y nº de NA)
summary(estudios) #En este caso no da los datos igual que en la función anterior porque son datos cualitativos, no cuantitativos
summary(sexo) #Resumen por sexo, en este caso al ser un dato cualitativo, muestra la frecuencia
misDatos<-data.frame(edad,estudios,sexo) #Se crea una tabla tipo Excel que incluye edad, estudios y sexo #Creamos objeto
summary(misDatos) #Hace un resumen por cada una de las variables (para las cualitativas da solo frecuencias)

#8. DATOS AGREGADOS POR GRUPOS       #Trata de resumir los datos     #Ej: f(X1,X2,X3)=y
aggregate(edad,by=list(sexo),mean)  #Incluyendo NA

aggregate(edad,by=list(sexo),mean,na.rm=TRUE) #Da la edad media de los hombres y mujeres sin contar los NA



#9. REPRESENTACIONES GRAFICAS  #En este apartado vemos distintas representaciones gráficas de los datos, que pueden ser exportados
#help(pie)
pie(table(estudios)) #Da un gráfico tipo tarta
#help(barplot)
barplot(table(sexo)) #Da un gráfico tipo barras
#help(hist)
hist(edad) #Da un histograma de edad
#help(boxplot)
boxplot(edad~sexo) #Da un gráfico que compara dos variables, siendo la barra en negrita la MEDIANA de edad de cada sexo. Las barras finas dan la variación respecto de la MEDIA

