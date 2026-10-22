# ==============================================================================
# Actividad: Análisis de Correlación Canónica
# Desempeño de vendedores y pruebas de aptitud (Johnson & Wichern, Tabla 9.12)
# Análisis Multivariado · ID 033521 · Pontificia Universidad Javeriana
# ==============================================================================
# Convertido desde el .Rmd/.html de la actividad. Las preguntas quedan como
# comentarios; los espacios "___" del original ya están completados.
#
# PREGUNTA DE LA GERENCIA: ¿las pruebas de selección permiten anticipar el
# desempeño de un vendedor? ¿Qué prueba aporta más?
# ==============================================================================


# ------------------------------------------------------------------------------
# 2. PAQUETES Y DATOS
# ------------------------------------------------------------------------------
# install.packages(c("CCA", "CCP"))   # solo la primera vez
library(CCA)   # cc, matcor, img.matcor, comput, plt.cc
library(CCP)   # p.asym

vendedores <- read.table(header = TRUE, text = "
id   crec  rent nuevas creat mec abst mat
 1   93.0  96.0  97.8     9  12    9  20
 2   88.8  91.8  96.8     7  10   10  15
 3   95.0 100.3  99.0     8  12    9  26
 4  101.3 103.8 106.8    13  14   12  29
 5  102.0 107.8 103.0    10  15   12  32
 6   95.8  97.5  99.3    10  14   11  21
 7   95.5  99.5  99.0     9  12    9  25
 8  110.8 122.0 115.3    18  20   15  51
 9  102.8 108.3 103.8    10  17   13  31
10  106.8 120.5 102.0    14  18   11  39
11  103.3 109.8 104.0    12  17   12  32
12   99.5 111.8 100.3    10  18    8  31
13  103.5 112.5 107.0    16  17   11  34
14   99.5 105.5 102.3     8  10   11  34
15  100.0 107.0 102.8    13  10    8  34
16   81.5  93.5  95.0     7   9    5  16
17  101.3 105.3 102.8    11  12   11  32
18  103.3 110.8 103.5    11  14   11  35
19   95.3 104.3 103.0     5  14   13  30
20   99.5 105.3 106.3    17  17   11  27
21   88.5  95.3  95.8    10  12    7  15
22   99.3 115.0 104.3     5  11   11  42
23   87.5  92.5  95.8     9   9    7  16
24  105.3 114.0 105.3    12  15   12  37
25  107.0 121.0 109.0    16  19   12  39
26   93.3 102.0  97.8    10  15    7  23
27  106.8 118.0 107.3    14  16   12  39
28  106.8 120.0 104.8    10  16   11  49
29   92.3  90.8  99.8     8  10   13  17
30  106.3 121.0 104.5     9  17   11  44
31  106.0 119.5 110.5    18  15   10  43
32   88.3  92.8  96.8    13  11    8  10
33   96.0 103.3 100.5     7  15   11  27
34   94.3  94.5  99.0    10  12   11  19
35  106.5 121.5 110.5    18  17   10  42
36  106.5 115.5 107.0     8  13   14  47
37   92.0  99.5 103.5    18  16    8  18
38  102.0  99.8 103.3    13  12   14  28
39  108.3 122.3 108.5    15  19   12  41
40  106.8 119.0 106.8    14  20   12  37
41  102.5 109.3 103.8     9  17   13  32
42   92.5 102.5  99.3    13  15    6  23
43  102.8 113.8 106.8    17  20   10  32
44   83.3  87.3  96.3     1   5    9  15
45   94.8 101.8  99.8     7  16   11  24
46  103.5 112.0 110.8    18  13   12  37
47   89.5  96.0  97.3     7  15   11  14
48   84.3  89.8  94.3     8   8    8   9
49  104.3 109.5 106.5    14  12   12  36
50  106.0 118.5 105.0    12  16   11  39
")

X <- as.matrix(vendedores[, c("crec", "rent", "nuevas")])        # desempeño (p = 3)
Y <- as.matrix(vendedores[, c("creat", "mec", "abst", "mat")])   # pruebas   (q = 4)
dim(X); dim(Y)   # 50 x 3  y  50 x 4


# ------------------------------------------------------------------------------
# 3. PERTINENCIA DEL ACC
# ------------------------------------------------------------------------------
# 1. Calcule la matriz de correlaciones con matcor() y grafíquela con
#    img.matcor(..., type = 2).
# 2. Describa las correlaciones DENTRO de cada conjunto. ¿Hay colinealidad
#    en alguno?
# 3. Describa las correlaciones CRUZADAS. ¿Qué prueba se relaciona más con el
#    desempeño? ¿Vale la pena hacer un ACC?

correl <- matcor(X, Y)
img.matcor(correl, type = 2)
round(correl$XYcor, 2)


# ------------------------------------------------------------------------------
# 4. AJUSTE Y CORRELACIONES CANÓNICAS
# ------------------------------------------------------------------------------
# 4. Estandarice las variables y ajuste el ACC con cc(). ¿Cuántos pares
#    canónicos hay y por qué?
# 5. Reporte las correlaciones canónicas y sus cuadrados. Interprete rho1*^2.
# 6. Compare rho1* con la mayor correlación cruzada y con la correlación
#    múltiple de cada indicador de desempeño con las cuatro pruebas
#    (use summary(lm(...))$r.squared). ¿Se cumple lo visto en clase?

zX <- scale(X); zY <- scale(Y)
acc <- cc(zX, zY)
acc$cor
acc$cor^2   # cuadrados (pregunta 5)

# Correlación múltiple de la primera variable de X con todas las Y:
sqrt(summary(lm(zX[, 1] ~ zY))$r.squared)
# Para las tres variables de desempeño a la vez (pregunta 6):
sapply(1:ncol(zX), function(j) sqrt(summary(lm(zX[, j] ~ zY))$r.squared))


# ------------------------------------------------------------------------------
# 5. ¿CUÁNTOS PARES RETENER?
# ------------------------------------------------------------------------------
# 7. Aplique la prueba de Wilks con p.asym() y la de Bartlett con la función
#    bartlett_acc() de ACC_codigos.R.
# 8. ¿Cuántos pares son significativos con alpha = 0.05? ¿Y con alpha = 0.01?
# 9. Calcule a mano la Lambda de Wilks de la primera fila y compárela con la
#    salida.

# NOTA: ACC_codigos.R no venía en el PDF. Si ya lo tienes, usa:
#   source("ACC_codigos.R")
# y borra esta definición. Esta versión implementa el test secuencial de
# Bartlett: para el par k, H0: rho_k = ... = rho_s = 0, con
#   estadístico = -(n - 1 - (p + q + 1)/2) * ln(prod_{i>=k}(1 - rho_i^2))
#   gl          = (p - k + 1) * (q - k + 1)
bartlett_acc <- function(rho, n, p, q) {
  s <- length(rho)
  m <- n - 1 - (p + q + 1) / 2
  res <- data.frame(par = 1:s, stat = NA_real_, gl = NA_real_, p.valor = NA_real_)
  for (k in 1:s) {
    lambda <- prod(1 - rho[k:s]^2)
    res$stat[k]    <- -m * log(lambda)
    res$gl[k]      <- (p - k + 1) * (q - k + 1)
    res$p.valor[k] <- pchisq(res$stat[k], res$gl[k], lower.tail = FALSE)
  }
  res
}

p.asym(acc$cor, nrow(X), ncol(X), ncol(Y))
prod(1 - acc$cor^2)           # Lambda de Wilks de la primera fila
bartlett_acc(acc$cor, n = nrow(X), p = ncol(X), q = ncol(Y))


# ------------------------------------------------------------------------------
# 6. COEFICIENTES CANÓNICOS FRENTE A CARGAS
# ------------------------------------------------------------------------------
# 10. Escriba u1 y v1 con los coeficientes estandarizados
#     (acc$xcoef, acc$ycoef).
# 11. Calcule las cargas con comput(). Busque una prueba cuyo coeficiente en
#     v1 sea pequeño pero cuya carga sea alta. ¿Por qué ocurre?

acc$xcoef; acc$ycoef
cargas <- comput(zX, zY, acc)
cargas$corr.X.xscores    # desempeño con U
cargas$corr.Y.yscores    # pruebas con V


# ------------------------------------------------------------------------------
# 7. INTERPRETACIÓN DE LOS PARES CANÓNICOS
# ------------------------------------------------------------------------------
# 12. Con las cargas, dé un nombre a u1 y a v1. ¿La relación entre los
#     conjuntos es directa o inversa?
# 13. Calcule las cargas cruzadas y verifique que
#     r(v1, Xj) = rho1* * r(u1, Xj).
# 14. Intente interpretar el segundo par. ¿Qué dificultades encuentra?

cargas$corr.X.yscores    # desempeño con V (cargas cruzadas)
cargas$corr.Y.xscores    # pruebas con U (cargas cruzadas)

# Verificación de la pregunta 13 (debe dar ~0 en todas las entradas):
round(cargas$corr.X.yscores[, 1] - acc$cor[1] * cargas$corr.X.xscores[, 1], 6)


# ------------------------------------------------------------------------------
# 8. PROPORCIÓN DE VARIANZA EXPLICADA Y REDUNDANCIA
# ------------------------------------------------------------------------------
# 15. Calcule la proporción de varianza estandarizada que cada variable
#     canónica explica de su propio conjunto.
# 16. Calcule las redundancias Rd(z1 | v_k) y Rd(z2 | u_k).
# 17. ¿Qué porcentaje de la variabilidad del desempeño explican las pruebas?
#     ¿Y al revés? ¿Por qué no coinciden?
# 18. Con la redundancia, ¿cuántos pares retendría? Compare con la pregunta 8.

propX <- colMeans(cargas$corr.X.xscores^2)
propY <- colMeans(cargas$corr.Y.yscores^2)
redX  <- acc$cor^2 * propX   # redundancia de X dado V
redY  <- acc$cor^2 * propY   # redundancia de Y dado U

propX; propY
redX; redY
sum(redX); sum(redY)         # redundancia total (pregunta 17)


# ------------------------------------------------------------------------------
# 9. GRÁFICOS
# ------------------------------------------------------------------------------
# 19. Haga el gráfico plt.cc(acc, var.label = TRUE) e interprete los dos
#     paneles.
# 20. Grafique u1 contra v1. ¿Hay vendedores que se aparten del patrón
#     general o que estén en los extremos?

plt.cc(acc, var.label = TRUE)
u <- acc$scores$xscores; v <- acc$scores$yscores
plot(u[, 1], v[, 1], xlab = "u1", ylab = "v1")
# Opcional: identificar atípicos / extremos con el id del vendedor
# text(u[, 1], v[, 1], labels = vendedores$id, pos = 3, cex = 0.7)


# ------------------------------------------------------------------------------
# 10. CONCLUSIÓN PARA LA GERENCIA
# ------------------------------------------------------------------------------
# 21. Redacte un párrafo (máximo 8 líneas) que responda la pregunta de la
#     gerencia. Incluya: la fuerza de la asociación, cuántos pares considera
#     relevantes, qué prueba aporta más y una advertencia sobre los
#     resultados.
#
# (Escriba aquí su respuesta, después de ver las salidas.)


# ------------------------------------------------------------------------------
# 11. REFERENCIA
# ------------------------------------------------------------------------------
# Johnson, R. A. & Wichern, D. W. (2007). Applied Multivariate Statistical
# Analysis (6.a ed.). Pearson. Tabla 9.12 y capítulo 10.
