library(FactoMineR)
library(party)
library(rminer)
library(lawstat)

############CON VARIABLES CUANTITATIVAS####################

###Llamando la base de datos###
data(iris)
View(iris)
names(iris)
###Aplicación del Análisis de Componentes Principales###
ACP=PCA(iris[,1:4],ncp=2)
ACP

ACP$ind$coord
ACP$ind


############APLICACIÓN JERÁRQUICA con ACP##################
###Aplicación de clasificación jerárquica###
Cong=HCPC(ACP)
Cong
Cong$data.clust
Cong$call$t
IdCong=Cong$data.clust[,5]
IdCong
###Observar la varianza### 
CACP=ACP$ind$coord
CACP
levene.test(CACP[,1],as.factor(IdCong))
fit=aov(CACP[,1]~as.factor(IdCong))
anova(fit)