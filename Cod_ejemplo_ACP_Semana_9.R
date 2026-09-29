# Paquetes
install.packages(c("FactoMineR","factoextra","tibble"))
library(FactoMineR)
library(factoextra)
library(readxl)
library(tibble)


bd_gorriones <- read_excel("C:/Users/martinezr.dm/OneDrive - Pontificia Universidad Javeriana/Documents/PUJ_202602/Multivariado/ACP/gorriones.xlsx", 
                        sheet = "gorriones")
View(bd_gorriones)

dim(bd_gorriones)
head(bd_gorriones)

#----

# La primera columna contiene el id de los pájaros.
# Las variables de las medidas corporales  quedan desde la columna 2 hasta la 6.
# La columna X6 se deja fuera del ACP inicial.

gorriones <- bd_gorriones[,2:6]
summary(gorriones)


# ========= 1 Exploración inicial

pairs(gorriones, main = "Matriz de dispersión: medidas Gorriones")
cor_gorr <- cor(gorriones)
round(cor_gorr, 3)




# Preguntas
# ¿Qué variables presentan correlaciones fuertes?
#  ¿Hay variables con relación inversa respecto del resto?


####  ACP conFactorMiner

pca1 <- PCA(gorriones,graph=T)
plot(pca1)
# Resumen de resultados del PCA
summary(pca1)


lam<-pca1$eig #valores propios y porcentaje de varianza explicada.
pca1$var$coord #Vectores propios: coordenadas de las variables en los componentes principales.
pca1$var$contrib #contribución de cada variable a los componentes principales: Mide cuánto aporta cada variable a la construcción de una componente principal. Un valor alto indica que la variable es clave para definir el eje
pca1$var$cos2 # Calidad de la representación (cos2): Mide la calidad de representación de una variable en un componente. Un valor alto indica que la variable está bien representada en ese componente
pca1$ind$coord #coordenadas de los individuos en el nuevoespacio.


# 
plot(lam[,1],type="b", pch=19,xlab="Componente",ylab=expression(hat(lambda)[i]))
abline ( h = mean ( lam[,1] ) , lty = 2)

# Plano factoria
fviz_pca_biplot(pca1,repel=T)

# Plano factorial por vivo/muerto
fviz_pca_biplot(pca1,habillage = bd_gorriones$X6,repel=T,
                col.var="black",
                palette=c("hotpink2","forestgreen"))


###########  análisis de componentes principales desde la matriz  de correlaciones
acp<-princomp(gorriones,cor=TRUE)
summary(acp)
# gráfico scree
plot(acp)
# La desviaci´on est´andar de cada componente principal es decir la raiz de los valores propios de la matriz
acp$sdev
# Matriz con los vectores propios
acp$loadings
# la media de las variables originales con la que se corrigen
# las obs
acp$center
# numero de observaciones
acp$n.obs

# las coordenadas factoriales
acp$scores
#biplots
par(mfrow=c(2,2))
# primer plano factorial
biplot(acp)
#segundo plano factorial
biplot(acp,choices = c(1,3))
# tercer plano factorial
biplot(acp,choices = c(2,3))
# Acp desde la matriz de covarianzas
# ( opci´on por defecto )
acpCov<-princomp(ejemp5_1)