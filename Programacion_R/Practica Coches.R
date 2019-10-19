library(tidyverse)
data(mpg) #Importamos los datos MPG, que estan incluidos en la bibiloteca tidyverse
mpg
help(mpg)
ggplot(data = mpg) + #Pedimos que prepare una hoja en blanco para hacer un grafico
  geom_point(mapping = aes (x= displ, y = hwy, color = class)) + #Graficamos la relacion entre las variables displ y hwy. Ademas pedimos que cada punto lo pinte en funcion de la clase del coche (variable class)
  facet_wrap(~ class, nrow = 2) #Pedimos que lo separe por clases en distintos graficos

ggplot(data = mpg) +
  geom_point(mapping = aes (x = drv, y = cyl, color = class))+
  facet_grid(drv~cyl) #En este caso vemos la comparacion de las variables drv y cyl clasificando por colores segun class

ggplot(data = mpg) +
  geom_point(mapping = aes (x = drv, y = cyl))+
  facet_grid(drv~.) #Clasificamos la cilindrada segun la traccion

ggplot(data = mpg) +
  geom_point(mapping = aes (x = drv, y = cyl))+
  facet_grid(.~drv) #Igual que la anterior pero cambiando las filas por las columnas

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv)) #Vemos la tendencia con sus desviaciones por colores segun la traccion

