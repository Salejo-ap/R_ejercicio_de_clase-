# Solucion Preparcial:

#----------## ejercicio ACC:----------------------------------------------------------------

# Librerias necesarias
install.packages(c("CCA", "CCP", "robustbase"))
library(CCA)
library(CCP)
library(robustbase)

#Contexo: Asociacion entre variables demograficas(2) y consumo(3) de 70 familias
n <- 70
R <- as.matrix(c(1, 0.8, 0.26, 0.67, 0.34,
                 0.8, 1, 0.33, 0.59, 0.34,
                 0.26, 0.33, 1, 0.37, 0.21,
                 0.67, 0.59, 0.37, 1, 0.35,
                 0.34, 0.34, 0.21, 0.35, 1), nrow=5, ncol=5)

# Separar las variabkes demograficas y de consumo
X <- as.matrix(R[, 1:2])
Y <- as.matrix(R[, 3:5])