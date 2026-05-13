library(FactoMineR)
library(party)
library(rminer)
library(lawstat)
####################APLICACIÓN NO JERÁRQUICA CON ACM###########################
data(tea)
names(tea)
View(tea)
#tea$breakfast <- NULL
#tea$breakfast

###Aplicación del Análisis de Componentes Principales###
#windows()
res.mca1<- MCA(tea,ncp=4,quanti.sup=19,quali.sup=20:36)
res.mca1
res.mca1$ind$coord
res.mca1$var

summary(res.mca1)

#Visualizar el aporte por dimensiones
max.porc <- max(1/(dim(tea)-1)*100)

library(factoextra)

fviz_screeplot(res.mca1) +
  geom_hline(yintercept = max.porc, linetype = 2, color = "red") + 
  labs(title = "Gráfico de sedimentación", x = "Dimensiones", 
       y = "Porcentaje de variabilidad explicada")

###Visualizaciones
fviz_mca_var(res.mca1, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)


###Visualizaciones
fviz_mca_var(res.mca1, axes = c(1,3),col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)


fviz_mca_var(res.mca1, axes = c(1,3),
             repel = TRUE, choice = "var",
             ggtheme= theme_minimal())

fviz_mca_var(res.mca1, axes = c(1,4),
             repel = TRUE, choice = "var",
             ggtheme= theme_minimal())

fviz_mca_var(res.mca1, axes = c(2,3),
             repel = TRUE, choice = "var",
             ggtheme= theme_minimal())

fviz_mca_var(res.mca1, axes = c(2,4),
             repel = TRUE, choice = "var",
             ggtheme= theme_minimal())



#################################################

kmMCA <- kmeans(res.mca1$ind$coord, 3) 
kmMCA
kmMCA$cluster
kmMCA$centers

table(tea$breakfast, kmMCA$cluster)
plot(res.mca1$ind$coord, col=kmMCA$cluster,pch=20)
points(kmMCA$centers,col=1:3, pch=8, cex=2)