# Sergio Alejandro Amaya Paez
# Datos del Ejemplo de Variedades A y B

x1_bar <-c(6.6, 1.985)
x2_bar <-c(5.56, 1.824)
S1 <-matrix(c(1.42,-0.0504,-0.0504, 0.005), 2, 2)
S2 <-matrix(c(1.543,-0.037,-0.037, 0.0045), 2, 2)
n1 <-6; n2 <-5; p <-2; alpha <-0.05

# Covarianza Pooled

S_pool <-((n1- 1)*S1 + (n2- 1)*S2) / (n1 + n2- 2)
# Estadistica T2
dif <-x1_bar- x2_bar
T2 <-t(dif) %*% solve((1/n1 + 1/n2) * S_pool) %*% dif
# Valor critico
vc <-((n1 + n2- 2)*p / (n1 + n2- p- 1)) * qf(alpha, p, n1 + n2- p- 1, lower.
tail = FALSE)
cat("T2 =", T2, "| Valor critico =", vc, "\n")
