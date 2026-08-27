# importar datos
datos <- read.table("effluent_Example6_1.txt")
# Calculo de diferencias
Dj1 <-datos$BOD_COM- datos$BOD_ST
Dj2 <-datos$SS_COM- datos$SS_ST
Dbarra <-c(mean(Dj1), mean(Dj2))
SD <-cov(cbind(Dj1, Dj2))
n <-nrow(datos); p <-2; alpha <-0.05
# Estadistica T2 de Hotelling
T2 <-n * t(Dbarra) %*% solve(SD) %*% Dbarra # solve() calcula SD^-1
# Valor critico
vc <-(((n- 1) * p) / (n- p)) * qf(alpha, p, n- p, lower.tail = FALSE)
vc
