#Ejercicio practico 1:
#Los porcentajes de las notas del curso son: 10% asistencia, 40% prácticas y 50% examen final
#La nota mínima necesaria para aprobar el examen final es 5. Si no se saca al menos un 5 en el final, se debe devolver un 4 como nota final

minota<-function(a,p,e){ #Si por ejemplo queremos que la asistencia siempre fuese un 10, hay que poner en esta linea a=10
  nota<-a*.1+p*.4+e*.5 
  if(e<5|nota<5)
    return(4)
  else
    return(nota)
}
minota(10,10,10)
minota(10,10,4)
minota(5,5,5)
minota(5,5,4)

#Ejercicio 2:
#Crear una funcion que nos diga el nombre de los alumnos que no han aprobado el examen final. Conocemos la nota calculada y la nota definitiva
#Si la nota calculada esta aprobada y la definitiva no, es porque ha suspendido el examen

nombres <- c("Maria", "Marta", "Javier", "Alvaro", "Beltran")
nota_calculada <- c(4.5, 6, 7.3, 2.5, 5.6)
notas_definitivas <- c(4, 4, 7.3, 2.5, 4)
notas_alumnos <- data.frame(nombres, nota_calculada, notas_definitivas)
suspendidos<-c()
noSuperan <- function(notas_alumnos){
  t = 0
  for(i in 1:length(notas_alumnos$nombres)){
    if (as.integer(nota_calculada[i])!= as.integer(notas_definitivas[i]) | notas_definitivas[i]<=4){
      t = t+1
      ?append
      suspendidos<-append(suspendidos, as.character(notas_alumnos$nombres[i]))
      
      
    }
  }
  cat("Total suspendidos:", t, "\n")
  cat("Nombres Suspendidos:", suspendidos)
}

noSuperan(notas_alumnos)

