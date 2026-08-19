# Sergio Alejandro Amaya Paez ---- Taller 1 ----18/08/2026
# instalacion e importacion de librerias necesarias y datos

install.packages("moments")
require(moments)
library(ggplot2)
install.packages("HSAUR2")
install.packages("MVA")
library(HSAUR2)
library(MVA)
library(readxl)
install.packages("MVN")
library(MVN)
install.packages("mvShapiroTest")
library(mvShapiroTest)
datos <- read_excel("mineral.xlsx")
datos

# convertir los datos a numerico
datos <- as.data.frame(lapply(datos, as.numeric))
datos
#---------------------------------------------------------------------------------------------------------------------

# 0.1. Análisis descriptivo

##A. ¿Cuál es el contenido mineral promedio de los dos lados de estos tres huesos? ¿Cuál es su covarianza? Interprete
##¿La acumulación de mineral en las estructuras óseas ocurre de manera homogénea? (10 puntos).

### Vector de medias
medias <- apply(datos, 2, mean)
medias

#### Matriz de varianzas y covarianzas
covarianza <- cov(datos)
covarianza

##B. Existe correlación entre las mediciones de los dos lados de estos tres huesos considerando todas las variables
##vs todas? De qué magnitud es? Interprete. (10 puntos)
correlacion <- cor(datos)
correlacion

##C. Interprete el siguiente gráfico multivariado
#------------------------------------------------------------------------------------------------------------------
# 0.2. Normalidad Multivariante

## A. Grafique la distribución univariada de cada variable, y evalue la normalidad univariada usando esos gráficos 
## y una prueba de hipótesis de cada variable. Concluya.

### Graficar la distribucion univariada de cada variable
#### domradius

distribucion_domradius <- ggplot(datos, aes(x = domradius)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    geom_density(color = "red", size = 1) +
    theme_minimal() +
    labs(title = "Distribución de domradius", x = "domradius", y = "Densidad")

distribucion_domradius
#### radius
distribucion_radius <- ggplot(datos, aes(x = radius)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    geom_density(color = "red", size = 1) +
    theme_minimal() +
    labs(title = "Distribución de radius", x = "radius", y = "Densidad")

distribucion_radius
#### Domhumerus
distribucion_domhumerus <- ggplot(datos, aes(x = domhumerus)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    geom_density(color = "red", size = 1) +
    theme_minimal() +
    labs(title = "Distribución de domhumerus", x = "domhumerus", y = "Densidad")

distribucion_domhumerus
#### Humerus
distribucion_humerus <- ggplot(datos, aes(x = humerus)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    geom_density(color = "red", size = 1) +
    theme_minimal() +
    labs(title = "Distribución de humerus", x = "humerus", y = "Densidad")

distribucion_humerus

#### domulna
distribucion_domulna <- ggplot(datos, aes(x = domulna)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    geom_density(color = "red", size = 1) +
    theme_minimal() +
    labs(title = "Distribución de domulna", x = "domulna", y = "Densidad")

distribucion_domulna

#### ulna
distribucion_ulna <- ggplot(datos, aes(x = ulna)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    geom_density(color = "red", size = 1) +
    theme_minimal() +
    labs(title = "Distribución de ulna", x = "ulna", y = "Densidad")

distribucion_ulna

## B. Dibuje las elipeses entre cada posible par de variables. ¿Qué puede decir sobre la
## distribución normal bivariada de esos pares de variables?.
### Prueba de normalidad bivariada por cada par de variables
normalidad_bivariada <- function(data, var1, var2) {
    xy <- data.frame(data[[var1]], data[[var2]])
    testMardia <- mvn(data = xy, mvn_test = "mardia")
    return(testMardia$multivariate_normality)
}
## domradius y radius
normalidad_bivariada(datos, "domradius", "radius")
## domradius y domhumerus
normalidad_bivariada(datos, "domradius", "domhumerus")
## domradius y humerus
normalidad_bivariada(datos, "domradius", "humerus")
## domradius y domulna
normalidad_bivariada(datos, "domradius", "domulna")
## domradius y ulna
normalidad_bivariada(datos, "domradius", "ulna")
## domhumerus y radius
normalidad_bivariada(datos, "radius", "domhumerus")
## radius y humerus
normalidad_bivariada(datos, "radius", "humerus")
## radius y domulna
normalidad_bivariada(datos, "radius", "domulna")
## radius y ulna
normalidad_bivariada(datos, "radius", "ulna")
## domhumerus y humerus
normalidad_bivariada(datos, "domhumerus", "humerus")
## domhumerus y domulna
normalidad_bivariada(datos, "domhumerus", "domulna")
## domhumerus y ulna
normalidad_bivariada(datos, "domhumerus", "ulna")
## humerus y domulna
normalidad_bivariada(datos, "humerus", "domulna")
## humerus y ulna
normalidad_bivariada(datos, "humerus", "ulna")
## domulna y ulna
normalidad_bivariada(datos, "domulna", "ulna")

# Generar gráfico bvbox
x1x2 <- data.frame(
DataExample_4_14$x1,
DataExample_4_14$x2
)

x1x2 <- data.frame(datos$domradius, datos$radius)
mat12 <- as.matrix.data.frame(x1x2)
MVA::bvbox(mat12, method = "robust",
xlab = expression(datos[x[1]]),
ylab = expression(datos[x[2]]))
x1x2 <- data.frame(datos$domradius, datos$domhumerus)
mat12 <- as.matrix.data.frame(x1x2)
MVA::bvbox(mat12, method = "robust",
xlab = expression(datos[x[1]]),
ylab = expression(datos[x[2]]))
## C. Calcule las distancias de Mahalanobis, grafique el qqplot e interprete ¿Hay sospecha de normalidad multivariada?
# Calcular la distancia de Mahalanobis
distancias <- mahalanobis(datos, center = medias, cov = covarianza)

# Ordenar 
distancias_ordenadas <- sort(distancias)

# Crear los cuantiles teóricos de una chi-cuadrado

cuantiles_teoricos <- qchisq(ppoints(length(distancias_ordenadas)), df = ncol(datos))

# Generar el qq-plot
plot(cuantiles_teoricos, distancias_ordenadas,
     xlab = "Cuantiles Chi-cuadrado (df=6)",
     ylab = "Distancias de Mahalanobis ordenadas",
     main = "QQ-plot para evaluar normalidad multivariada")

## D. Identifique si hay observacines atípicas. Si las hay, indique cuáles son y responda qué haría con ellas ¿Tendrán 
## efecto sobre la normalidad multivariada?
out<-mvn(datos, multivariate_outlier_method = "adj")
summary(out, select = "outliers")

## E. Elija una prueba de normalidad multivariada (justificando su elección) y úsela para concluir si los datos 
## siguen una distribución normal multivariada.
### aplicar test de normalidad multivariada de shapiro-wilk
resultado <- mvShapiro.Test(as.matrix(datos))
print(resultado)
testdornik <- mvn(data = datos, mvn_test = "doornik_hansen")
testdornik$multivariate_normality
