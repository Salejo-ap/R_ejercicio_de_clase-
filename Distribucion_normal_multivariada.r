#Distribucion normal multivariada 

## Librerias necesarias

library(mvtnorm)

### Paremetros

n<-100

#### vector de medias

mu<-c(70,170)

#### matriz de varianza y covarianza

n <- 100
mu <- c(70 , 170)
rho <- 0.8
s11 <- 100
s22 <- 64
s12 <- rho * sqrt ( s11 ) * sqrt ( s22 )
Sigma <- matrix (c( s11 , s12 , s12 , s22 ) , nrow = 2, byrow = TRUE )
sd_1 <- sqrt ( s11 ) ; sd_2 <- sqrt ( s22 )


### Malla de puntos

Peso <- seq ( mu [1] - 3*sd_1, mu [1] + 3*sd_1, length = n )
Altura <- seq( mu [2] - 3*sd_2, mu [2] + 3*sd_2 , length = n)

### obtener la densidad de probabilidad

set.seed(123)
Densidad <- function (x1 , x2 )
    dmvnorm ( cbind ( x1 , x2 ) , mu , Sigma )
f <- outer ( Peso , Altura , FUN = Densidad)
f
