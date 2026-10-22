# Solucion Preparcial:

#----------## ejercicio ACC:----------------------------------------------------------------

# Librerias necesarias
install.packages(c("CCA", "CCP", "robustbase"))
library(CCA)
library(CCP)
library(robustbase)

#Contexo: Asociacion entre variables demograficas(2) y consumo(3) de 70 familias
n <- 70
# Define R
R <- matrix(c(1.0, .80, .26, .67, .34,
              .80, 1.0, .33, .59, .34,
              .26, .33, 1.0, .37, .21,
              .67, .59, .37, 1.0, .35,
              .34, .34, .21, .35, 1.0), nrow=5, byrow=TRUE)

# Name variables for clarity (optional but good practice)
colnames(R) <- rownames(R) <- c("Rest", "Cine", "Edad", "Ingreso", "Educ")

# Partition
R11 <- matrix(c(1, 0.8, 0.8, 1), 2, 2)
R22 <- matrix(c(1, 0.26, 0.37, 0.26, 1, 0.35, 0.37, 0.35, 1), 3, 3)
R12 <- matrix(c(0.26, 0.67, 0.34, 0.33, 0.59, 0.34), 2, 3, byrow = TRUE)
R21 <- t(R12)

# Compute M matrix
M <- solve(R11) %*% R12 %*% solve(R22) %*% R21

# Eigenvalues and eigenvectors
eigen_result <- eigen(M)
lambda <- eigen_result$values
rho <- sqrt(lambda)
eigen_result
lambda
rho
qchisq(0.95, df=6)
a1 <- Re(eigen_result$vectors[, 1])
a2 <- Re(eigen_result$vectors[, 2])

# 4. Calculamos los coeficientes para la Variable Canónica V (Demográfico)
# Fórmula: M = R22^-1 * R21 * R11^-1 * R12
M_V <- solve(R22) %*% R21 %*% solve(R11) %*% R12
eig_V <- eigen(M_V)
b1 <- Re(eig_V$vectors[, 1])

# 5. ¡Paso crucial! Normalización
# Los autovectores de R tienen norma 1, pero en CCA necesitamos que su varianza sea 1
# Fórmula: a / sqrt(a' * R11 * a)
a1_norm <- a1 / sqrt(t(a1) %*% R11 %*% a1)
b1_norm <- b1 / sqrt(t(b1) %*% R22 %*% b1)

# 6. Mostramos los resultados
cat("Coeficientes Canónicos para U1 (Consumo):\n")
print(a1_norm)
cat("\nCoeficientes Canónicos para V1 (Demográfico):\n")
print(b1_norm)

# Cargas Canónicas para el grupo Consumo (U1)
cargas_U1 <- R11 %*% a1_norm

# Cargas Canónicas para el grupo Demográfico (V1)
cargas_V1 <- R22 %*% b1_norm

print(cargas_U1)
print(cargas_V1)
