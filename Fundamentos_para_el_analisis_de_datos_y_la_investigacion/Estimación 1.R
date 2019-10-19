#####################  INFERENCIA PARAMETRICA #######################

###########################  ESTIMACION  ###########################


library(MASS)



set.seed(123)   #No fijar semilla y comentar los resultados

x<- rnorm(250, mean=1, sd=0.45)     


#Ajuste máxima verosimilitud de distribuciones univariables
fit <- fitdistr(x, densfun="normal") 
fit

hist(x, pch=20, breaks=25, prob=TRUE, main="HISTOGRAMA")
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="red", lwd=2, add=T)



#Estimación por el método de los momentos 
fitMME <- fitdistr(x, "normal", method="mme")
fitMME
curve(dnorm(x, fitMME$estimate[1], fitMME$estimate[2]), col="blue", lwd=2, add=T)



#Estimación máxima bondad del ajuste
#Diversos estudios destacan la mejor estimación paramétrica 

fitMGE <-fitdistr(x, "normal", method="mge")
fitMGE



