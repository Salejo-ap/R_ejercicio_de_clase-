# Ejercicio de componenetes principales (PCA) en R
## Importacion de los datos
data <- read.table("Olimpiadas.txt", header = TRUE, sep = "\t")
data <- data[, - 1] # Eliminamos la primera columna (nombres de los paises)
data
## Calculo de la matriz de correlacion
cor_matrix <- cor(data)
cor_matrix
