# Análisis de Componentes Principales (ACP) en R
## Importacion de los datos
datos <- mtcars
datos
data <- datos[, c("mpg", "disp", "hp", "wt","drat", "qsec")]

## media varianza y matriz de correlacion
media <- apply(data, 2, mean)
varianza <- cov(data)
correlacion <- cor(data)

media
varianza
correlacion
## Valores y vectores propios
eigenvalores <- eigen(correlacion)$values
eigenvectores <- eigen(correlacion)$vectors

eigenvalores
eigenvectores

## Cálculo de la proporción de varianza explicada
proporcion_varianza <- eigenvalores / sum(eigenvalores)
proporcion_varianza