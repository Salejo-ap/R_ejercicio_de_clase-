# Ejercicio de componenetes principales (PCA) en R
## Librerias necesarias
install.packages(c("FactoMineR", "factoextra", "tibble", "ggplot2"))
install.packages("rlang")
library(factoextra)
library(FactoMineR)
library(tibble)
library(ggplot2)

## Importacion de los datos
data <- read.table("Olimpiadas.txt", header = TRUE, sep = "\t")
data <- data[, 2:8]# Eliminamos la primera columna (nombres de los paises)
data
summary(data)

## Exploración inicial
pairs(data, main = "Matriz de dispersión: medidas Olimpiadas")
cor_data <- cor(data)
round(cor_data, 3)

##ACP conFactorMiner
pca1 <- PCA(data, graph = TRUE)
plot(pca1)
summary(pca1) # Resumen de resultados del PCA

lam <- pca1$eig #valores propios y porcentaje de varianza explicada.
lam
pca1$var$coord #Vectores propios: coordenadas de las variables en los componentes principales. # nolint: line_length_linter.
pca1$var$contrib #contribución de cada variable a los componentes principales: Mide cuánto aporta cada variable a la construcción de una componente principal. Un valor alto indica que la variable es clave para definir el eje # nolint: line_length_linter.
pca1$var$cos2 # Calidad de la representación (cos2): Mide la calidad de representación de una variable en un componente. Un valor alto indica que la variable está bien representada en ese componente # nolint: line_length_linter.
pca1$ind$coord

# Grafica del codo para evaluar el número de componentes principales a retener
plot(lam[, 1], type = "b", pch = 19, xlab = "Componente", ylab = expression(hat(lambda)[i])) # nolint: line_length_linter.
abline(h = mean(lam[, 1]), lty = 2)

## Plano factorial
fviz_pca_biplot(pca1, repel = True)

## análisis de componentes principales desde la matriz  de correlaciones
acp <- princomp(data, cor = TRUE)
summary(acp)

# gráfico scree
plot(acp)

# La desviacion estandar de cada componente principal es decir la raiz de los valores propios de la matriz # nolint: line_length_linter.
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
par(mfrow = c(2, 2))
# primer plano factorial
biplot(acp)

#segundo plano factorial
biplot(acp, choices = c(1, 3))

# tercer plano factorial
biplot(acp, choices = c(2, 3))

# Acp desde la matriz de covarianzas
# ( opcion por defecto )
acpCov <- princomp(ejemp5_1)
