#### Ejercicio 2 ####

##Librerias##

library(readr)

#Preparar datos

calificaciones<-read_delim("C:/Users/atorn/Desktop/Máster/Primer cuatrimestre/Programación R/Práctica/Calificaciones/calificaciones ECO 2019.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ","), na = "0",trim_ws = TRUE)

## Apartado a) ##
nrow(calificaciones) #Vemos el numero de alumnos

presentados_exfinal<-subset(calificaciones, calificaciones$`Ex. JUNIO-12P`!="NA")
View(presentados_exfinal)
media_junio<-mean(presentados_exfinal$`Ex. JUNIO-12P`) 
print(media_junio)*10/12 #Muestra la media de la nota del examen final sobre 12 y luego corregido sobre 10 (ya que inicialmente es sobre 12)

## Apartado b) ##

presentados_exfinal<-subset(calificaciones, calificaciones$`Ex. JUNIO-12P`!="NA") #Filtramos los presentados que no sean NA
View(presentados_EF)
  aprueba_sin_asistencia <- function(presentados_exfinal){
    num.alumnos<-length(presentados_exfinal$`NOMBRE ALUMNO`)
    lista_nombres<-c()
    for (i in 1:num.alumnos){
      if (presentados_exfinal$`Asistencia -1P`[i]<0.5 & presentados_exfinal$`Ex. JUNIO-12P`[i]>=6){
        lista_nombres<-append(lista_nombres,presentados_exfinal$`NOMBRE ALUMNO`[i])
      }
    }
    print(lista_nombres)
  }
  
aprueba_sin_asistencia(presentados_exfinal)

## Apartado c) ##

for (i in 1:nrow(calificaciones)){    #Aqui decimos que si alguno esta calificado previamente como suspenso, que se mantenga
  if (calificaciones$CALIFICACION[i]=="SUSPENSO"){
    calificaciones$CALIFICACION[i]<-"SUSPENSO"
  }
  else {
    if (calificaciones$Nota_FINAL[i]>=9){
      calificaciones$CALIFICACION[i]<-"SOBRESALIENTE"
    }
    if (calificaciones$Nota_FINAL[i]>=7 & calificaciones$Nota_FINAL[i]<9){
      calificaciones$CALIFICACION[i]<-"NOTABLE"
    }
    if (calificaciones$Nota_FINAL[i]>=5 & calificaciones$Nota_FINAL[i]<7){
      calificaciones$CALIFICACION[i]<-"APROBADO"
    }
    if (calificaciones$Nota_FINAL[i]<5){
      calificaciones$CALIFICACION[i]<-"SUSPENSO"
    }
  }
}

Calificacionesnew<-calificaciones

View(Calificacionesnew)

## Apartado d) ##

write.csv(Calificacionesnew, file="Calificaciones2019.csv")
