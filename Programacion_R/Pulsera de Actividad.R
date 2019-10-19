#-------------- PRIMERA PARTE  2 PUNTOS ------------------#


#1. Crear un nuevo proyecto denominado practica 4.

# 2. Mediante la libreria readr, o mediante los menus de RStudio, leer el dataset activities.csv #ambos archivos deben estar previamente en la carpeta del proyecto creado


library(readr)
library(tidyverse)

activities<-read_csv("C:/Users/atorn/Desktop/Máster/Primer cuatrimestre/Programación R/Práctica/Pulsera de Actividad/activities.csv")


# 3.Comprobar el contenido con View y contar cuantos NAs hay en la columna GPS del dataset activities


view(activities)

sum(is.na(activities$GPS))


# 4. Crear un objeto R denominado act_new que contenga solo las variables siguientes: 1,2,5-6


act_new<-select(activities,'de','a','Timezone','Activity type') #Ac


# 5. Renombrar la variable 'Activity type' con el nombre 'tipo' y la variable 'Time zone' como 'ciudad'


act_new<-rename(act_new,'tipo'='Activity type','ciudad'="Timezone")


# 6. Realizar un recuento de tipo de actividad con summary. Para ello debes transformar previamente la variable tipo a factor con as.factor.


nueva<-mutate(act_new, tipo =as.factor(act_new$'tipo'))

summary(nueva$tipo)


# Crea un grafico de barras con dicha variable par visualizar las frecuencias.


plot(nueva$tipo, main="Tipo de Actividad")


# Haz lo mismo para la variable ciudad


nueva<-mutate(act_new, tipo =as.factor(act_new$'ciudad'))

summary(nueva$ciudad)


#7. Filtrar los registros de act_new que correspondan con ciudad Amsterdam en otro objeto


act_new_ams<-filter(act_new, ciudad=="Europe/Amsterdam")


# y lo mismo con Madrid. Con esos nuevos objetos determina los deportes que
# no se practican en Amsterdam y si en Madrid y viceversa. Genera graficos para visualizar los resultados


act_new_mad<-filter(act_new, ciudad=="Europe/Madrid")

setdiff(act_new_ams$tipo, act_new_mad$tipo) #Vemos los deportes unicamente presentes en Amsterdam

setdiff(act_new_mad$tipo, act_new_ams$tipo) #Vemos los deportes unicamente presentes en Madrid

new_act_new_mad<-mutate(act_new_mad, tipo =as.factor(act_new_mad$tipo)) #Preparamos objeto para graficar Madrid

new_act_new_ams<-mutate(act_new_ams, tipo =as.factor(act_new_ams$tipo)) #Preparamos objeto para graficar Amsterdam

plot(new_act_new_mad$tipo, main = "Madrid") #Graficamos Madrid

plot(new_act_new_ams$tipo, main = "Amsterdam") #Graficamos Amsterdam


#8. Encontrar las fechas en las que se ha practicado bicicleta o pilates en Amsterdam en el ano 2019


filter(act_new_ams, act_new_ams$tipo == "Cycling" | act_new_ams$tipo == "Pilates")


#9. Crear una nueva variable  llamada dif con los minutos de realizacion de cada actividad en Amsterdam
# y realizar una representacion grafica de los resultados con plot y determinar que deporte o deportes
# se han practicado durante dos horas o mas


dif<-mutate(act_new_ams, dif=a-de)

amsterdam_minutos<-dif%>%
  group_by(tipo) %>%
  summarize(tiempo=as.integer(sum(dif)))

#Graficamos
ggplot(amsterdam_minutos, aes(x=tipo, y=tiempo))+
  geom_bar(stat="identity")+
  xlab("Deporte")+
  ylab("Tiempo")


#10. Guardar los nuevos dataset en unos archivos llamados "act_new_mad.csv" y "act_new_ams.csv"


write.csv(act_new_ams, file="act_new_ams.csv")

write.csv(act_new_mad, file="act_new_mad.csv")




#----------------SEGUNDA PARTE   3 PUNTOS-------------


# 11. Cargar el dataset sleep en un objeto llamado sleep


sleep<-read_csv("C:/Users/atorn/Desktop/Máster/Primer cuatrimestre/Programación R/Práctica/Pulsera de Actividad/sleep.csv")


#12. crear un nuevo data set llamado sleep_new que contenga solo las variables
#que contengan informacion, que no sean todo cero.

summary(sleep) #Vemos que variables son siempre cero

sleep_new <- data.frame(select(sleep, -"Rem (seg)", -"Snoring episodes", -"Average heart rate", -"Heart rate (min)", -"Heart rate (max)"))

#13. Renombrar las variables de sleep_new a nombres cortos:


sleep_new<-rename(sleep_new,"ligero"="ligero..s.",'profundo'='profundo..s.','despierto'='despierto..s.', 'tiempo_en_dormir'='Duration.to.sleep..s.', 'tiempo_en_despertar'='Duration.to.wake.up..s.', 'ronquidos'='Snoring..s.' )
summary(sleep_new)


#14. Eliminar todas las filas que contengan algun NA


sleep_new <- na.omit(sleep_new)


# 15. Calcular cuanto tiempo en total se ha dormido cada noche: ligero+profundo


total_dormido <- apply (sleep_new[ , 3:4], 1, sum)

total_dormido / 3600 #Da el resultado en horas


# 16. Visualizacion de la relacion ligero-profundo-total


plot(tiempos_descanso$ligero,tiempos_descanso$profundo)
plot(tiempos_descanso$total_dormido,tiempos_descanso$profundo)
plot(tiempos_descanso$total_dormido,tiempos_descanso$ligero)


# A la vista de los resultados, que tipo de sueno es mas relevante?


### A la vista de los resultados, en cuanto a tiempo durmiendo es mas importante el ligero, pero medicamente seguramente sea mas importante el profundo porque es donde se alcanzan las fases REM


# 17. Realizar un analisis de diferencias entre los dos tipos de sueno e interpretar los resultados
# usar la funcion ICalpha o el 'One sample t-test' de TeachingDemos: t.test()


tiempos_descanso_test <-data.frame(sleep_new$ligero, sleep_new$profundo)

library(TeachingDemos)

t.test(tiempos_descanso_test, mu=0)

#Planteamos el contraste de hipotesis (Ho:no hay diferencias significativas, H1: si hay diferencias significativas)


ICalpha<-function(ModeloA, ModeloB, alfa=0.05)  ##Creamos la funcion ICalpha
{
  n<-length(ModeloA)
  diferencias<-ModeloA-ModeloB
  mediad<-mean(diferencias)
  #mediad2<-mean(diferencias^2)
  s<-sqrt(var(diferencias))
  #s<-sqrt(mediad2-mediad^2)
  valort<-qt(alfa/2,n-1,lower.tail = F)
  valor<-valort*s/sqrt(n)
  cotaInf<-mediad-valor
  cotaSup<-mediad+valor
  df<-data.frame(cotaInf, cotaSup)
  return(df)
}


ICalpha(sleep_new$ligero, sleep_new$profundo)   #El cero no esta en el intervalo. Para un alfa del 5% se rechaza la Ho

#18. Crear una nueva variable 'ciudad' en sleep_new con la informacion de act_new.


fecha <- substr(act_new$de, 1, 10) #Creo variable para almacenar unicamente la fecha
act_new$fecha <- as.factor(fecha)

fecha <- substr(sleep_new$de, 1, 10)
sleep_new$fecha <- as.factor(fecha)

act_new <- select (act_new, -de, -a)      #Eliminamos las variables de y a ya que ya no nos son utiles e incordian para los proximos calculos
sleep_new <- select (sleep_new, -de, -a)

act_new_temp <- act_new %>%
  group_by(fecha, ciudad) %>%
  summarize (count = sum(dif)) #Generamos nuevo item segun la fecha y ciudad, y el tiempo total de deporte

sleep_new <- inner_join(act_new_temp, sleep_new) #Unios todo en un unico objeto


#19. Representar la relacion totalsleep y profundo usando como facetas el factor ciudad


sleep_new_temp <- sleep_new %>% group_by(act_new$ciudad) %>% summarize(profundo = sum(profundo), total = sum(total_dormido)) #Se agrupa por ciudad #Creamos agregaciones y vemos porcentajes sobre el total de sueno


#20. Guardar el dataset sleep_new en un archivo "sleep_new.csv"


write.csv(sleep_new, file = "sleep_new.csv", row.names = FALSE)


#21. Guardar el proyecto completo. Subir la carpeta del proyecto al campus.