

# --- Ingresamos los datos --- #
X <- matrix ( c (42 ,52 ,48 ,58 , 4 ,5 ,4 ,3) , ncol = 2 , nrow = 4)
X <- data.frame ( X )
colnames(X)<- c("Ventas","No.Libros")
# --- Vector de medias muestrales --- #
x_media <- apply (X , 2 , mean ) # Ventas = 50 , No . Libros = 4
# --- Matriz de varianzas y covarianzas muestral --- #
S <- cov (X) # diag : 45.33 , 0.67 ; s12 = -2
# --- Matriz de correlacion muestral --- #
R <- cor ( X ) # r12 = -0.3638
# --- Coeficientes de asimetria y curtosis --- #
install.packages("moments")
require ( moments )
skewness (X$Ventas);skewness (X$No.Libros) # 0 y 0
kurtosis (X$Ventas) ; kurtosis (X$No.Libros) # 1.78 y 2





#----- Graficos

install.packages("ggplot2")
library(ggplot2)


# --- Gráfico de dispersión
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point(color="red", size=3) +
  labs(title="Relación Peso vs Consumo", x="Peso", y="Millas por galón")

# ---- Matrix de dispersión

# Opción 1: 
pairs(~mpg+disp+hp+drat+wt+qsec,data=mtcars,pch=16,col="deepskyblue")


# Caras de Chernoff

install.packages("aplpack")
library(aplpack)
datos <- scale( mtcars[1:10, c("mpg", "cyl", "disp", "hp", "wt")])
faces(
  datos,
  labels = rownames(datos),
  main = "Caras de Chernoff con variables estandarizadas"
)

# Gráfico de estrellas
stars(datos,
      labels = rownames(mtcars)[1:10],
      draw.segments = TRUE)
legend("topright",
       legend = colnames(datos),
       bty = "n",
       title = "Variables")

install.packages("fmsb")

#--- Gráfico de radar con dos individuos

radar <- rbind(
  apply(datos, 2, max),
  apply(datos, 2, min),
  datos[1:2, ]
)

radarchart(radar,axistype = 1,
  pcol = c("red", "blue"),
  pfcol = c(rgb(1,0,0,0.3), rgb(0,0,1,0.3)),
  plwd = 2,plty = 1, cglcol = "grey",cglty = 1,
  axislabcol = "grey30",vlcex = 1.1,
  title = "Comparación de dos automóviles")

legend("right",legend = rownames(datos)[1:2],
  col = c("red", "blue"),lwd = 2, bty = "n")

