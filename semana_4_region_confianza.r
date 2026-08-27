# Semana 4 probar normal multivariado y encontrar regiones de confianza
## importar librerias necesarias

install.packages("HSAUR2")
install.packages("MVA")
install.packages ("MVTests")
install.packages("MVN")
install.packages("mvShapiroTest")
install.packages("ICSNP")
require(MVTests)
library(ICSNP)
library(HSAUR2)
library(MVA)
library(MVN)
library(mvShapiroTest)

## importar datos TXT
datos <- read.table("sweat_data.txt", header = TRUE)
datos

#--------------------------------------------------------------------------------------------------# # nolint
## Preparacion
#--------------------------------------------------------------------------------------------------# # nolint

### convercion de los datos a numerico
datos <- as.data.frame(lapply(datos, as.numeric))
datos

### vector de medias
medias <- colMeans(datos [,-1])
medias

### matriz de varianza y covarianza
covarianza <- cov(datos)
covarianza

### correlacion
correlacion <- cor(datos)
correlacion

#------------------------------------------------------------------------------------------------------------------------------# nolint
## Prueba de normalidad
#------------------------------------------------------------------------------------------------------------------------------# nolint

### Para hacer una prueba de normalidad multivariada se tiene la libreria MVA, dado las caracteristicas del conjunto de datos
### para la realizacion de este test formal se selecciona el test de shapiro por p=<5 y n>5p ademas que se comporta muy bien en
### conjuntos de datos pequeños.

### test de shapiro
variables <- data.frame(datos$X1_Sweat_rate, datos$X2_Sodium, datos$X3_Potassium)
matriz <- as.matrix.data.frame(variables)
mvShapiro.Test(matriz)

# los resultados del test muestra un p-valor de 0.2567 no rechazando la hiportesis nula, siendo una distribucion normal multivariada

#-------------------------------------------------------------------------------------------------------------------------------#
## Region de confianza
#-------------------------------------------------------------------------------------------------------------------------------#

###Calcular la region de confianza univariada
IC1 <- t.test (datos$X1_Sweat_rate , mu = medias[1] , conf.level = 0.90)
IC2 <- t.test (datos$X2_Sodium , mu = medias[1] , conf.level = 0.90)
IC3 <- t.test ( datos$X3_Potassium , mu = medias[1] , conf.level = 0.90)

IC1$conf.int
IC2$conf.int
IC3$conf.int

###----- Longitud
L1 <- IC1$conf.int[2]-IC1$conf.int[1]
L2 <- IC2$conf.int[2]-IC2$conf.int [1]
L3 <- IC3$conf.int[2]-IC3$conf.int[1]

###en este caso con se esta haciendo de forma univariada, el la confianza es igual a (1-alpha)^p lo cual da 0.729

### Intervalos simultaneos con el T de hotelling
OneSampleHT2(datos[,-1], mu0 = medias, alpha = 0.10)$CI

### intervalos de Bonferroni

ICB <- function(x, alpha, p, n){
    xbarra <- mean(x)
    sii <- var(x)
    tc <- qt(alpha/(2*p), n - 1, lower.tail = FALSE )
    error <- tc*sqrt(sii/n)
    linf <- xbarra - error
    lsup <- xbarra + error
    ICB <- c(linf, lsup)
    return(ICB)
}

ICB1 <- ICB(datos$X1_Sweat_rate, 0.10, 3, 20)
ICB2 <- ICB(datos$X2_Sodium, 0.10, 3, 20)
ICB3 <- ICB(datos$X3_Potassium, 0.10, 3, 20)

l1 <- round(ICB1[2] - ICB1 [1], 2)
l2 <- round(ICB2[2] - ICB2 [1], 2)
l3 <- round(ICB3[2] - ICB3 [1], 2)

### segun entendi, el T de hotteling se usa para la inferencia con respecto al vector de medias
### el test de Bonferroni es para la inferencua individual de cada una de las medias 

n <- nrow(variables) # Muestra
p <- ncol(variables) # Variables
alpha <- 0.05
# 3. Multiplicador c r t i c o de Hotelling
f_critico <- qf(1 - alpha, df1 = p, df2 = n - p)
mult_hotelling <- sqrt((p * (n - 1) / (n - p)) * f_critico)

a <- rep(0, p) ; names(a) <- colnames(variables)
a [" X1 _ Sweat _ rate "] <- 1
a [" X2 _ Sodium "] <- -1

# 5. C l c u l o del intervalo
media_comb <- sum(a * medias)
ee_comb <- as.numeric(sqrt(t(a) %*% (covarianza/n) %*% a))
lim_inf <- media_comb - (mult_hotelling * ee_comb)
lim_sup <- media_comb + (mult_hotelling * ee_comb)
