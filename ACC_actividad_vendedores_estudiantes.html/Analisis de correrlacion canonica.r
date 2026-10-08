# Analisis de correrlacion canonica 
## Cargar librerias
install.packages(c("CCA", "CCP"))
library(CCA)   # cc, matcor, img.matcor, comput, plt.cc
library(CCP)   # p.asym

## datos
vendedores <- read.table(header = TRUE, text = "
id  crec  rent nuevas creat mec abst mat
1  93.0  96.0  97.8   9  12   9  20
2  88.8  91.8  96.8   7  10  10  15
3  95.0 100.3  99.0   8  12   9  26
4 101.3 103.8 106.8  13  14  12  29
5 102.0 107.8 103.0  10  15  12  32
6  95.8  97.5  99.3  10  14  11  21
7  95.5  99.5  99.0   9  12   9  25
8 110.8 122.0 115.3  18  20  15  51
9 102.8 108.3 103.8  10  17  13  31
10 106.8 120.5 102.0  14  18  11  39
11 103.3 109.8 104.0  12  17  12  32
12  99.5 111.8 100.3  10  18   8  31
13 103.5 112.5 107.0  16  17  11  34
14  99.5 105.5 102.3   8  10  11  34
15 100.0 107.0 102.8  13  10   8  34
16  81.5  93.5  95.0   7   9   5  16
17 101.3 105.3 102.8  11  12  11  32
18 103.3 110.8 103.5  11  14  11  35
19  95.3 104.3 103.0   5  14  13  30
20  99.5 105.3 106.3  17  17  11  27
21  88.5  95.3  95.8  10  12   7  15
22  99.3 115.0 104.3   5  11  11  42
23  87.5  92.5  95.8   9   9   7  16
24 105.3 114.0 105.3  12  15  12  37
25 107.0 121.0 109.0  16  19  12  39
26  93.3 102.0  97.8  10  15   7  23
27 106.8 118.0 107.3  14  16  12  39
28 106.8 120.0 104.8  10  16  11  49
29  92.3  90.8  99.8   8  10  13  17
30 106.3 121.0 104.5   9  17  11  44
31 106.0 119.5 110.5  18  15  10  43
32  88.3  92.8  96.8  13  11   8  10
33  96.0 103.3 100.5   7  15  11  27
34  94.3  94.5  99.0  10  12  11  19
35 106.5 121.5 110.5  18  17  10  42
36 106.5 115.5 107.0   8  13  14  47
37  92.0  99.5 103.5  18  16   8  18
38 102.0  99.8 103.3  13  12  14  28
39 108.3 122.3 108.5  15  19  12  41
40 106.8 119.0 106.8  14  20  12  37
41 102.5 109.3 103.8   9  17  13  32
42  92.5 102.5  99.3  13  15   6  23
43 102.8 113.8 106.8  17  20  10  32
44  83.3  87.3  96.3   1   5   9  15
45  94.8 101.8  99.8   7  16  11  24
46 103.5 112.0 110.8  18  13  12  37
47  89.5  96.0  97.3   7  15  11  14
48  84.3  89.8  94.3   8   8   8   9
49 104.3 109.5 106.5  14  12  12  36
50 106.0 118.5 105.0  12  16  11  39
")

X <- as.matrix(vendedores[, c("crec", "rent", "nuevas")])          # desempeño (p = 3)
Y <- as.matrix(vendedores[, c("creat", "mec", "abst", "mat")])     # pruebas  (q = 4)
dim(X); dim(Y)

## Calculo correlacion y correlaciones cruzadas
correl <- matcor(X, Y)
img.matcor(correl, type = 2)
round(correl$XYcor[1:3, 4:7], 2)
round(correl$Xcor, 2)
round(correl$Ycor, 2)

### Preguntas relacionadas a la correlacion entre variables y conjunto de variables 
#### 1. Describa las correlaciones dentro de cada conjunto. ¿Hay colinealidad en alguno?
# rta: En el conjunto de desempeño se puede observar muy altas correlaciones entre las tres variabeles de desempeño(crec-rent: 0.93, crec-nuevas: 0.88, rent-nuevas: 0.84),
# indicando un colinealidad entre ellas.
# En el conjunto de pruebas, se observa que las correlaciones bajas entre las cuatro variables de pruebas(creat-mec: 0.59, creat-abst: 0.15, creat-mat: 0.41, mec-abst: 0.39, mec-mat: 0.57, abst-mat: 0.57),
# con algunas correlaciones altas entre ciertas variables.
#### 2. Describa las correlaciones cruzadas. ¿Qué prueba se relaciona más con el desempeño? 
# En la validacion cruzada se puede ver variedad de correlacion entre las variables en general de moderadas a altas, siendo la variable mat la que tiene mayor correlacion con las variables de desempeño.
#### 3. ¿Vale la pena hacer un ACC?
#Si se justifica el uso del ACC, ya que hay correlaciones significativas entre los conjuntos de variables, lo que indica que existe una relación lineal entre el desempeño y las pruebas. 
#Además, la presencia de colinealidad en el conjunto de desempeño sugiere que un análisis de correlación canónica puede ayudar a identificar las combinaciones lineales de variables que mejor explican la relación entre los dos conjuntos.

## Ajuste de las correlaciones canonicas
# Escalar variables
zX <- scale(X); zY <- scale(Y)
acc <- cc(zX, zY)

#correlaciones canonicas
acc$cor

# Correlación múltiple de la primera variable de X con todas las Y:
sqrt(summary(lm(zX[, 1] ~ zY))$r.squared)

# Correlación múltiple de la segunda variable de X con todas las Y:
sqrt(summary(lm(zX[, 2] ~ zY))$r.squared)

# Correlación múltiple de la tercera variable de X con todas las Y:
sqrt(summary(lm(zX[, 3] ~ zY))$r.squared)

#grafica caida de las correlaciones canonicas
plot(acc$cor, type = "b", pch = 19, ylim = c(0, 1), xaxt = "n",
     xlab = "Par canónico", ylab = "Correlación canónica",
     main = "Correlaciones canónicas")
axis(1, at = 1:3)