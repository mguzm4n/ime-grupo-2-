library(ggpubr)
library(rcompanion)
library(WRS2)

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


# 3

library(tidyverse)

datos <- read.csv2("EP11 Datos.csv")

# Contexto e hipótesis:

# 2.
# Dependiendo de cuánto ganan (personas de chile) influye en si van a pie, en vehiculo 
# o en transporte publico en su movilización en el día a día.

# H_0: El ingreso promedio de los hogares es igual para cada grupo que utiliza distintos 
# tipos de transporte para movilizarse.
# H_A: El ingreso promedio de los hogares es diferente para cada grupo que utiliza distintos 
# tipos de transporte para movilizarse.

# Obtención de datos para cada grupo - es el mismo código que para el EP 11.

set.seed(891)
n2 <- 450

datos2 <- datos %>% sample_n(n2) %>% select(region, ytotcorh, o25c)


motorizado <- datos2 %>% filter(o25c == "Vehículo motorizado particular (auto, camioneta, motocicleta") 
motorizado <- motorizado$ytotcorh

publico <- datos2 %>% filter(o25c == "Transporte público (bus, microbús, metro, taxi colectivo, et") 
publico <- publico$ytotcorh

pie <- datos2 %>% filter(o25c == "A pie") 
pie <- pie$ytotcorh

# Veamos la distribución de los ingresos, en forma de histogramas:

g_01 <- gghistogram(motorizado, bins = 15, xlab = "Ingreso (grupo motorizado)")
g_02 <- gghistogram(publico, bins = 15, xlab = "Ingreso (grupo trans. púb.)")
g_03 <- gghistogram(pie, bins = 20, xlab = "Ingreso (grupo 'a pie')")

histogramas_ingresos <- ggarrange(g_01, g_02, g_03,
                                  nrow = 1, ncol = 3)

# Vemos que la distribución en la cola derecha tiene una larga separación
# aumentando increíblemente el ingreso, y con una frecuencia mínima (entre 0 y 5)
# Por otro lado, vemos en la cola izquierda el mismo caso, solo que mejor agrupado
# a las barras con más frecuencia, sin embargo, son datos muy alejados de la
# distribución que tiene la mayoría de la muestra cerca de la media.
# Por ello, utilizaremos un método robusto, pues estamos en presencia
# de datos problemáticos y no podríamos realizar métodos convencionales como ANOVA
# ya que existen ciertos datos muy grandes o muy pequeños que arruinan nuestros 
# supuestos.

print(histogramas_ingresos)


# Construimos el data frame necesario con los ingresos agrupados 

ingreso <- c(motorizado, publico, pie)
tipo_transporte <- c(rep("motorizado", length(motorizado)),
                     rep("publico", length(publico)),
                     rep("pie", length(pie))
                     )
  
datos_transporte <- data.frame(ingreso, tipo_transporte)

# Ahora, como se ha explicado, usaremos una prueba robusta para los datos, ya que, como siempre se
# explica, los datos de "ingresos" son la mayoría de las veces problemáticos.

# En nuestro caso, esto se ve en los histogramas presentados para mirar la forma de los datos 
# de los ingresos (figura histogramas_ingresos).

# Con esto en mente, se utiliza el método t1way, que utiliza una medida robusta como
# la media truncada, justo lo que necesitamos en este caso, para eliminar
# cierto porcentaje de datos en ambas colas de la distribución, que son los menos
# probables y tienen valores extremos (demasiado altos y demasiado pequeños)
# evitando un gran salto de datos mínimos y máximos.


set.seed(565) # Semilla para reproducibilidad de datos

alfa <- 0.05 # Fijamos el nivel de significación
gamma <- 0.15 # Usamos una poda de "X" % para la media

# Realizamos la prueba como tal:

prueba_media_truncada <- t1way(ingreso ~ tipo_transporte, 
                               data = datos_transporte,
                               tr = gamma,
                               alpha = alfa)


print(prueba_media_truncada)

# La prueba nos da un p-value = 0.00015. Dado que el valor es menor a nuestro nivel
# de significación, diremos que, con un 95% de confianza, existen diferencias
# en los ingresos de los usuarios de diferentes tipos de transportes.
# Como tenemos tres grupos, queremos realizar un análisis post-hoc para
# identificar en qué par de grupos se encuentra la diferencia significitiva,
# o bien, si todos los grupos tiene ingresos definitivamente distintos.

# Realizamos el análisis post_hoc con la función 'lincon', especialmente hecha
# para ser utilizada tras haber aplicado la función t1way:

post_hoc <- lincon(ingreso ~ tipo_transporte, 
                   data = datos_transporte, 
                   tr = gamma, 
                   alpha = alfa)

print(post_hoc)

# Conclusiones finales:

# Dados los resultados del análisis post-hoc, podemos ver que entre grupo de personas que utilizan 
# tipos de transporte motorizados (autos, motos) y aquellos que solo van 'a pie'
# existe una gran diferencia entre los ingresos (p = 0.00008, muy menor a alpha).
# Por otro lado, aquellos que van en transporte público y los mismos usuarios con 
# transporte motorizado propio, se diferencian en menor forma, sin embargo aún existe
# diferencia significativa estadísticamente (p = 0.003 < alpha).
# Finalmente, aquellos que se transportan 'a pie' y aquellas personas que utilizan
# transporte público no poseen diferencias significativas entre sus ingresos (p = 0.0621 > alfa)
# es decir, en promedio estos dos grupos tienen los mismos ingresos, sin embargo, 
# el margen para esta prueba es bastante estrecho (de 0.1), por lo que faltaría corroborar
# con otro estudio.
