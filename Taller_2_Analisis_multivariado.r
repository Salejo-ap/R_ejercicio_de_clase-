# Taller #2 Analisis multivariado, comparacion de medias y varianza

install.packages("readxl")
install.packages("mvShapiroTest")
install.packages("MVN")
library(MVN)
library(mvShapiroTest)
library(readxl)
#---------------------------------------------------------------------#

## Punto 1

#Objetivo: estudiar el Test para la diferencia del vector de medias y matriz de covarianzas con muestras # nolint
#independienets. Describir y aplicar el test de diferencia de medias para datos multivariados independientes  # nolint
# (utilizar un nivel de significancia (α = 0,05).Usar base de datos Tortugas
#Contexto: Se ha medido la longitud, la anchura y la altura del caparazón de 48 tortugas, 24 hembras y 24 machos.  # nolint

## importar la base de datos de excel
archivo <- "Bd_Taller2.xlsx"
hojas <- excel_sheets(archivo)
datos_completos <- lapply(hojas, function(hoja) read_excel(archivo, sheet = hoja)) # nolint
names(datos_completos) <- hojas

## separar el data set de tortugas.
data_tortugas <- datos_completos[["Tortugas"]]
data_tortugas

## separar registros por sexo
tortugas_machos <- subset(data_tortugas, Group == "M")
tortugas_hembras <- subset(data_tortugas, Group == "F")
tortugas_machos
tortugas_hembras

# eliminar columnas no numericas
tortugas_machos$Group <- NULL
tortugas_hembras$Group <- NULL
tortugas_machos
tortugas_hembras

# Vector de medias y matriz de covarianzas para machos y hembras
medias_machos <- colMeans(tortugas_machos)
medias_hembras <- colMeans(tortugas_hembras)
medias_machos
medias_hembras
covarianza_machos <- cov(tortugas_machos)
covarianza_hembras <- cov(tortugas_hembras)
covarianza_machos <- as.matrix(covarianza_machos)
covarianza_hembras <- as.matrix(covarianza_hembras)
covarianza_machos
covarianza_hembras

#A) Supuestos poblacionales: normalidad multivariada.
testshapiro_machos <- mvShapiro.Test(as.matrix(tortugas_machos))
testshapiro_hembras <- mvShapiro.Test(as.matrix(tortugas_hembras))
testshapiro_machos
testshapiro_hembras
test_mardia_machos <- mvn(data = tortugas_hembras, mvn_test = "mardia")
test_mardia_machos$multivariate_normality
test_mardia_hembras <- mvn(data = tortugas_hembras, mvn_test = "mardia")
test_mardia_hembras$multivariate_normality

#B) Muestre la formula del estadístico de prueba, reemplazando por los valores que corresponden al ejercicio.  # nolint
#Cuál es el estadístico teórico?

alpha <- 0.05

# Covarianza Pooled
S_pool <- (( 24 - 1)*covarianza_machos + ( 24 - 1)*covarianza_hembras ) / ( 24 + 24 - 2) # nolint
S_pool

# Estadistica T2
dif <- medias_machos - medias_hembras
T2 <- t(dif) %*% solve((1/24 + 1/24) * S_pool) %*% dif

# Valor critico
vc <- (( 24 + 24 - 2)*p / ( 24 + 24 - p - 1) ) * qf( alpha, p, 24 + 24 - p - 1, lower.tail = FALSE ) # nolint

#D) Contraste que los vectores de medias son iguales en ambos sexos, suponiendo que # nolint
#las matrices de covarianzas son iguales. Interprete el resultado.
cat("T2 =", T2, "| Valor critico =", vc, "\n")

#E) Contraste que las matrices de covarianzas son iguales entre sexos. Exhiba las hipótesis a contrastar y el  # nolint
#estadístico de prueba. Interprete el resultado
#parametros
p <- 3
q <- 2
n1 <- 24
n2 <- 24
v1 <- n1 - 1
v2 <- n2 - 1
v <- v1 + v2
Sp <- (1 / v) * (v1 * covarianza_machos + v2 * covarianza_hembras)
Lambda3 <- v * log(det(Sp)) - (v1 * log(det(covarianza_machos)) + v2 * log(det(covarianza_hembras))) # nolint: line_length_linter.
b <- (1 / v1 + 1 / v2 - 1 / v)
rho <- 1 - (((2 * (p^2)) + 3 * p - 1) / (6 * (p + 1) * (q - 1))) * b
varphi <- rho * Lambda3
vc <- qchisq(0.05, (1 / 2) * p * (p + 1) * (q - 1), lower.tail = FALSE)
varphi
vc
#----------------------------------------------------------------------#

##Punto 2

#Objetivo: estudiar el Test para la diferencia de medias con muestras pareadas. Describir y aplicar el test de  # nolint: line_length_linter.
#diferencia de medias para datos multivariados pareados (utilizar un nivel de significancia (α = 0,05). Usar base  # nolint
#de datos Huesos-pareados Contexto: Un estudio inició para determinar si el ejercicio y suplementos dietarios # nolint
#retardaría la perdida ósea en mujeres adultas. Un investigador midió el contenido mineral óseo por absorciometría  # nolint
#de fotones. Se registraron mediciones para tres huesos sobre los lados dominante y no dominante. # nolint
# Un año después se volvieron a medir los mismos 24 participantes.
#A) Exhiba las hipótesis del test y verifique los supuestos poblacionales de la muestra # nolint
#B) Verifique los supuestos para aplicar el test.
#C) Aplique el test para datos multivariados pareados. Determine si ha habido pérdidade hueso. # nolint

