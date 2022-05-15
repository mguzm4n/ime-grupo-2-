library(ggpubr)
library(rcompanion)

# 1.

instanciaA <- c(129, 109, 28, 178, 74, 16, 87, 108, 149, 78)
tiempoA <- c(1510394, 402929, 885722, 4428151,48667,834565, 70599, 783108,210041, 37449)
algoritmoA <- data.frame(instanciaA, tiempoA)

instanciaB <- c(134, 193, 10, 88, 142, 86, 36, 190, 163, 33)
tiempoB <- c(1252837, 2196277, 120276, 4629726, 5743260, 6701654, 6568968, 180141, 6684497, 35974)
algoritmoB <- data.frame(instanciaB, tiempoB)

algoritmos <- data.frame(tiempoA, tiempoB)

g <- gghistogram(algoritmoA, x = "instanciaA")
print(g)

transformarDatos <- function(datos){
  lambda <- transformTukey(datos, start = -1, end = 1, 
                             int = 0.1, plotit = TRUE, 
                             returnLambda = TRUE)
  
  datosTukey <- transformTukey(datos, start = -1, end = 1, 
                                int = 0.1, plotit = TRUE, 
                                returnLambda = FALSE)
  
  return(list(lambda, datosTukey))
}

transfA <- transformarDatos(algoritmoA$tiempoA)
lambda_A <- transfA[1]
datosTukeyA <- transfA[2]

transfB <- transformarDatos(algoritmoB$tiempoB)
lambda_B <- transfB[1]
datosTukeyB <- transfB[2]

tiemposTukey <- data.frame(datosTukeyA, datosTukeyB)

alfa <- 0.05
prueba <- t.test(x = datosTukeyA[[1]], 
                 y = datosTukeyB[[1]],
                 paired = FALSE,
                 alternative = "greater",
                 mu = 0,
                 conf.level = 1 - alfa)

print(prueba$p.value)


# 2.

