library(FactoMineR)
library(party)
library(rminer)
library(lawstat)
library(dplyr)
library(openxlsx) #Librería que interactúa con MSExcel
library(corrplot) #Librería para el gráfico de correlaciones
library(corrr) #Otra opción de librería para el cálculo y gráfico de correlaciones
library(nycflights13)
library(psych)

datos <- flights
#mydata <- datos[,3:8]
mydata <- iris[,1:4]

cor(mydata)

KMO(mydata)
cortest.bartlett(mydata)

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values

scree(mydata, pc=FALSE)

fa.parallel(mydata, fa="fa")


Nfacs <- 1  # This is for four factors. You can change this as needed.

fit <- factanal(mydata, Nfacs, rotation="promax")


print(fit, digits=2, cutoff=0.3, sort=TRUE)

load <- fit$loadings[,1]
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)


loads <- fit$loadings

fa.diagram(loads)

##########################
AF <- factor(mydata, levels=c(1:2))
AF
table(AF)
