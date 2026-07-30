# Semana 1 - vector de medias y matriz de covarianza

## Importar datos necesarios

path <-"C:\\Users\\glopa\\Desktop\\R\\Olimpiadas.txt"
datos <- read.table(path, header = TRUE, sep = "\t")
datos

## sacar la matriz  de variables numericas

datos_numericos<-datos[,sapply(datos, is.numeric)]
matriz<-data.frame(datos_numericos)

## calcular el vector de medias

matriz_media<- apply ( matriz , 2 , mean )
matriz_media

## calcular la matriz de varianzas y covarianza

matriz_covarianza <- cov(matriz)
matriz_covarianza

## calcular la matriz de correlacion

matriz_correlacion <- cor(matriz)
matriz_correlacion

# instalar paquete moments pra calcualar la curtosis y asimetria

install.packages("moments")
require(moments)

## calcular asiemtria

asimetria <- apply(matriz, 2, skewness)

## calcular curtosis

curtosis <- apply(matriz, 2, kurtosis)

## asimetría de la distribución de los datos respecto a la media muestral:
### sk(xk) > 0: distribución asimétrica positiva o a derecha.
### sk(xk) < 0: distribución asimétrica negativa o a izquierda.

asimetria

## Describe el comportamiento en las colas de la distribución de los datos: 
###Datos de una distribución normal: k(xk) ≈ 3.
###k(xk) > 3: distribución leptocúrtica.
###k(xk) < 3: distribución platicúrtica.

curtosis