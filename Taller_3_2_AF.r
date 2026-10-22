# Taller 3.2 AF

# Libreiras necesarias 
install.packages("psych")
install.packages("GPArotation")
install.packages("readxl")
install.packages("MVN")

library(MVN)
library(psych)        # cortest.bartlett, KMO, principal, fa
library(GPArotation) 
library(readxl) 

#leer los datos
datos<- read_xlsx("C:/Users/glopa/Desktop/R_ejercicio_de_clase-/desempeño.xlsx", sheet = 1)
datos
datos<-as.data.frame(datos)
datos<-datos[,-1]
datos

# Matriz de correlaciones
correlaciones<-cor(datos)
cor.plot(correlaciones, numbers = TRUE, main = "Matriz de correlación")

#prueba esfereicidad de bartlett y KMO vale la pena un AF
cortest.bartlett(correlaciones, n = 20) 
KMO(correlaciones)

# cuantos factores?
auto   <- eigen(correlaciones)
lambda <- auto$values
PVE    <- lambda / 7

round(rbind(autovalor = lambda, PVE = PVE, acumulada = cumsum(PVE)), 3)
plot(lambda, type = "b", pch = 19, xlab = "j", ylab = expression(hat(lambda)[j]),
     main = "Scree plot"); abline(h = 1, lty = 2)
fa.parallel(correlaciones, n.obs = 20, fa = "both", main = "Análisis paralelo")
lambda
#ajuste de un modelo de factores 
AF.fp <- principal(correlaciones, nfactors = 2, rotate = "none")
AF.fp

e   <- auto$vectors
L_m <- e[, 1:2] %*% diag(sqrt(lambda[1:2]))   # l_ij = sqrt(lambda_j) e_ij
L_m <- -L_m                                    # el signo de una columna es arbitrario
h2  <- rowSums(L_m^2)
Psi <- 1 - h2
round(cbind(L_m, h2 = h2, u2 = Psi), 3)
MRES <- correlaciones - (L_m %*% t(L_m) + diag(Psi))      # matriz residual
round(MRES, 3)
sqrt(sum(MRES^2) / (7 * 6))   
# MLE
testdornik <- mvn(data = datos, mvn_test = "doornik_hansen")
testdornik$multivariate_normality
AF.ml <- fa(correlaciones, nfactors = 2, fm = "ml", rotate = "none", n.obs = 20)
print(AF.ml$loadings, cutoff = 0.3)
round(AF.ml$uniquenesses, 3)
c(estadistico = AF.ml$STATISTIC, gl = AF.ml$dof, valor.p = AF.ml$PVAL)
