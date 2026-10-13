# Ejemplo Analisis Factorial

#Nombres de las variables
v <- c(" NDVI "," Tmin "," Tmax "," Tmed "," Alcant "," Basuras ",
" Agua "," Deficit "," Precip "," Tasa ")

#matriz de correlaciones
R <- matrix (c(
1.00 ,0.65 ,0.82 ,0.77 ,0.04 , -0.03 ,0.04 ,0.08 , -0.39 ,0.10 ,
0.65 ,1.00 ,0.92 ,0.97 ,0.53 ,0.42 ,0.55 ,0.52 ,0.12 ,0.28 ,
0.82 ,0.92 ,1.00 ,0.97 ,0.44 ,0.37 ,0.51 ,0.50 , -0.01 ,0.33 ,
0.77 ,0.97 ,0.97 ,1.00 ,0.48 ,0.38 ,0.49 ,0.45 , -0.01 ,0.27 ,
0.04 ,0.53 ,0.44 ,0.48 ,1.00 ,0.94 ,0.88 ,0.76 ,0.44 ,0.33 ,
-0.03 ,0.42 ,0.37 ,0.38 ,0.94 ,1.00 ,0.86 ,0.75 ,0.37 ,0.37 ,
0.04 ,0.55 ,0.51 ,0.49 ,0.88 ,0.86 ,1.00 ,0.90 ,0.56 ,0.47 ,
0.08 ,0.52 ,0.50 ,0.45 ,0.76 ,0.75 ,0.90 ,1.00 ,0.61 ,0.63 ,
-0.39 ,0.12 , -0.01 , -0.01 ,0.44 ,0.37 ,0.56 ,0.61 ,1.00 ,0.27 ,
0.10 ,0.28 ,0.33 ,0.27 ,0.33 ,0.37 ,0.47 ,0.63 ,0.27 ,1.00) , 10)

dimnames (R) <- list (v , v)

install.packages("psych")
install.packages("GPArotation")
library(psych)        # cortest.bartlett, KMO, principal, fa
library(GPArotation) 


# Paso 1 Bloques evidentes con la matriz de correlaciones
cor.plot(R, numbers = TRUE, main = "Matriz de correlación")

#en esta matriz de correlacion se puede observar dos bloques evidentes, uno de ellos formado por las variables NDVI, Tmin, Tmax y Tmed
#y otro bloque formado por las variables Alcant, Basuras, Agua y Deficit. Las variables Precip y Tasa no parecen pertenecer a ninguno de los dos bloques.

# Paso 2. Prueba de esfericidad de Bartlett y MSA global
cortest . bartlett (R, n = 32) # chi2 = 445.9 , gl = 45
KMO (R) # MSA global = 0.73
round ( eigen (R)$ values , 2)