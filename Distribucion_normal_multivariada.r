#Distribucion normal multivariada 

## Librerias necesarias

library(mvtnorm)

### Paremetros

n <- 100

#### vector de medias

mu <- c(70, 170)

#### matriz de varianza y covarianza

n <- 100
mu <- c(70 , 170)
rho <- 0.8
s11 <- 100
s22 <- 64
s12 <- rho * sqrt(s11) * sqrt(s22)
sigma <- matrix(c(s11, s12, s12, s22), nrow = 2, byrow = TRUE)
sd_1 <- sqrt(s11)
sd_2 <- sqrt(s22)


### Malla de puntos

peso <- seq(mu [1] - 3 * sd_1, mu [1] + 3 * sd_1, length = n)
altura <- seq(mu [2] - 3 * sd_2, mu [2] + 3 * sd_2, length = n)

### obtener la densidad de probabilidad

set.seed(123)
densidad <- function(x1, x2){
    dmvnorm(cbind(x1, x2), mu, sigma)
}
f <- outer(peso, altura, FUN = "densidad")

persp(peso, altura, f,
    theta = 20 , phi = 30 ,
    xlab = " Peso ", ylab = " Altura ", zlab = "f(Peso , Edad ) ",
    zlim = c(0 , 0.005) ,
    ticktype = " detailed ", nticks = 6,
    col = "# FFE1FF ")
title (main = bquote (" D i s t r i b u c i n Normal Bivariada (" ~ rho == .( rho ) ~ ")"))