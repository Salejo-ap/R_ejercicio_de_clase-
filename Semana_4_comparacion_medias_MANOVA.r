#-----Ex. Datos Skulls-----#
install.packages("heplots")
require(heplots)
data("Skulls")
require(mvShapiroTest)
#-----Prueba de Normalidad Multivariada-----#
mvShapiro.Test(as.matrix(Skulls[,2:5]))
# MVW = 0.99212, p-value = 0.755 (No se rechaza normalidad)
#-----MANOVA-----#
mod <-manova(cbind(mb, bh, bl, nh) ~ epoch, data = Skulls)
summary(mod, test = "Wilks")
# Df Wilks approx F num Df den Df Pr(>F)
# epoch 4 0.66359 3.9009 16 434.45 7.01e-07 ***
# Residuals 145
#----- Modelo para cada variable----- #
mod.skulls <- aov(mod, data = Skulls)
summary(mod.skulls)
