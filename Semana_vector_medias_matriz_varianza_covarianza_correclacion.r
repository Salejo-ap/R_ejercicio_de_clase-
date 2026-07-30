# Semana 1 - vector de medias y matriz de covarianza 
## Importar datos necesarios
path <-"C:\\Users\\glopa\\Desktop\\R\\Olimpiadas.txt"
datos <- read.table(path, header = TRUE, sep = "\t")
datos
## sacar las variables numericas
datos_numericos<-datos[,sapply(datos, is.numeric)]
matriz<-data.frame(datos_numericos)
## calcular el vector de medias
matriz_media<- apply ( matriz , 2 , mean )
matriz_media
## calcular la matriz de varianzas y covarianza
matriz_covarianza <- cov(matriz)
matriz_covarianza
## calcular la matriz de correlacion
matriz_correlacion <- cor(matriz)
matriz_correlacion
