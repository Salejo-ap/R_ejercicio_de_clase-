# Prueba de normalidad
## importacion de los datos
data <- read.table("DataExample_4_14.txt", header=TRUE)
data
## Librerias necesarias
library(ggplot2)
install.packages("HSAUR2")
install.packages("MVA")
library(HSAUR2)
library(MVA)

## Generar una distribución para cada variable numérica

### x1
distribucion_x1<-ggplot(data, aes(x = x1)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    geom_density(color = "red", size = 1) +
    theme_minimal() +
    labs(title = "Distribución de x1", x = "x1", y = "Densidad")
distribucion_x1

###x2

distribucion_x2<-ggplot(data, aes(x = x2)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    geom_density(color = "red", size = 1) +
    theme_minimal() +
    labs(title = "Distribución de x2", x = "x2", y = "Densidad")
distribucion_x2

### Distribucion x3

distribucion_x3<-ggplot(data, aes(x = x3)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    geom_density(color = "red", size = 1) +
    theme_minimal() +
    labs(title = "Distribución de x3", x = "x3", y = "Densidad")
distribucion_x3

### Distribucion x4

distribucion_x4<-ggplot(data, aes(x = x4)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    geom_density(color = "red", size = 1) +
    theme_minimal() +
    labs(title = "Distribución de x4", x = "x4", y = "Densidad")
distribucion_x4

## Elipses entre pares de variables

### x1 y x2 
x1x2 <- data.frame(
data$x1,
data$x2
)

matriz_x1_x2 <- as.matrix.data.frame(x1x2)

# Generar gráfico bvbox
MVA::bvbox(matriz_x1_x2, method = "robust",
xlab = expression(x[1]),
ylab = expression(x[2]))

### x1 y x3
x1x3 <- data.frame(
  data$x1,
  data$x3
)

matriz_x1_x3 <- as.matrix.data.frame(x1x3)

# Generar gráfico bvbox
MVA::bvbox(matriz_x1_x3, method = "robust",
  xlab = expression(x[1]),
  ylab = expression(x[3]))

### x1 y x4
x1x4 <- data.frame( 
  data$x1,
  data$x4
)

matriz_x1_x4 <- as.matrix.data.frame(x1x4)

# Generar gráfico bvbox
MVA::bvbox(matriz_x1_x4, method = "robust",
  xlab = expression(x[1]),
  ylab = expression(x[4]))

### x2 y x3
x2x3 <- data.frame(
  data$x2,
  data$x3
)
matriz_x2_x3 <- as.matrix.data.frame(x2x3)

MVA::bvbox(matriz_x2_x3, method = "robust",
  xlab = expression(x[2]),
  ylab = expression(x[3]))

### x2 y x4
x2x4 <- data.frame(
  data$x2,
  data$x4
)
matriz_x2_x4 <- as.matrix.data.frame(x2x4)

MVA::bvbox(matriz_x2_x4, method = "robust",
  xlab = expression(x[2]),
  ylab = expression(x[4]))

### x3 y x4
x3x4 <- data.frame(
  data$x3,
  data$x4
)
matriz_x3_x4 <- as.matrix.data.frame(x3x4)

MVA::bvbox(matriz_x3_x4, method = "robust",
  xlab = expression(x[3]),
  ylab = expression(x[4]))

## Grafico Q-Q de x^2
