library(ggpubr)
library(rcompanion)
library(WRS2)
library(tidyverse)

# 1. En el trabajo de título de un estudiante del DIINF se reportan los siguientes tiempos de ejecución (en milisegundos)
#medidos para dos versiones de un algoritmo genético para resolver instancias del problema del vendedor viajero disponibles en repositorios públicos.
#¿Es uno de los algoritmos más rápido que el otro? Aplique una transformación de datos adecuada para responder.



# Datos para preguntas '2 y '3

datos_anterior <- read.csv2("EP11 Datos.csv")


# Transformar tabla a data.frame para que pueda ser procesada en R.

instanciaA <- c(129, 109, 28, 178, 74, 16, 87, 108, 149, 78)
tiempoA <- c(1510394, 402929, 885722, 4428151,48667,834565, 70599, 783108,210041, 37449)
algoritmoA <- data.frame(instanciaA, tiempoA)

instanciaB <- c(134, 193, 10, 88, 142, 86, 36, 190, 163, 33)
tiempoB <- c(1252837, 2196277, 120276, 4629726, 5743260, 6701654, 6568968, 180141, 6684497, 35974)
algoritmoB <- data.frame(instanciaB, tiempoB)

algoritmos <- data.frame(tiempoA, tiempoB)

g <- gghistogram(algoritmoA, x = "instanciaA")
print(g)

# Transformar datos según Tukey
transformarDatos <- function(datos){
  lambda <- transformTukey(datos, start = -1, end = 1, 
                             int = 0.1, plotit = TRUE, 
                             returnLambda = TRUE)
  
  datosTukey <- transformTukey(datos, start = -1, end = 1, 
                                int = 0.1, plotit = TRUE, 
                                returnLambda = FALSE)
  
  return(list(lambda, datosTukey))
}

# Obtener datos transformados
transfA <- transformarDatos(algoritmoA$tiempoA)
lambda_A <- transfA[1]
datosTukeyA <- transfA[2]

transfB <- transformarDatos(algoritmoB$tiempoB)
lambda_B <- transfB[1]
datosTukeyB <- transfB[2]

# Unificar datos transformados en una sola tabla.
tiemposTukey <- data.frame(datosTukeyA, datosTukeyB)

# Realizar la prueba T de Student
# Se aplica con un nivel de significación de 0.05

# Formulación de hipótesis
# H0 = Los tiempos de ejecución del algoritmo A, son iguales a los tiempos de ejecución del algoritmo B (ninguno es mas rápido que otro)
# H1 = Los tiempos de ejecución del algoritmo A, son diferentes a los tiempos de ejecución del algoritmo B (Uno es más rápido que el otro)

# H0 = tA = tB
# h1 = tA != tB

# Se corrobora supuesto de normalidad mediante Shapiro Wilk
normalidadA = shapiro.test(datosTukeyA[[1]])
normalidadB = shapiro.test(datosTukeyB[[1]])
# Las observaciones provienen de una distribución cercana a la normal (p>alfa)

# Como las muestras son independientes y aleatorias, se puede proceder a aplicar la prueba.

alfa <- 0.05
prueba <- t.test(x = datosTukeyA[[1]], 
                 y = datosTukeyB[[1]],
                 paired = FALSE,
                 alternative = "greater",
                 mu = 0,
                 conf.level = 1 - alfa)

print(prueba$p.value)

# Como p = 0.9996, y p > alfa, se falla al rechazar la hipótesis nula, por lo que, con un 
# 95% de confianza, se puede asegurar que ningún algoritmo es más rapido que otro (los tiempos de ejecución son iguales)

#####################
####################

# 2.

# Contexto: Se desea realizar una aplicación de citas y se 
# requiere evaluar los grupos a los que irá enfocada la app.
# Por ello se desea estudiar el promedio de edad de hombres y mujeres 
# que están solteros(as) en el rango de edad entre 20 y 40 años.

# Hipótesis:

# H_0: En promedio, la edad de las mujeres y hombres solteros es la misma.
# H_A: La edad promedio de hombres y mujeres solteras difieren o son distintas.

# H_0: u_M = u_F, donde M : male, F: female
# H_A: u_M != u_F

adulto_joven <-  datos_anterior %>% filter(edad > 20 & edad < 40)

set.seed(198) # Semilla para repetibilidad de estudio
n <- 350

adulto_joven <- adulto_joven %>% sample_n(n) %>% select(sexo, ecivil, edad)
solteros <- adulto_joven %>% filter(ecivil == "Soltero(a)" & sexo == "Hombre")
solteros_edad <- solteros$edad
solteras <- adulto_joven %>% filter(ecivil == "Soltero(a)" & sexo == "Mujer")
solteras_edad <- solteras$edad

# Ajustamos datos para la prueba a realizar...

edad <- c(solteros_edad, solteras_edad)
genero <- c(rep("M", length(solteros_edad)), rep("F", length(solteras_edad)))
datos_df <- data.frame(edad, genero)

# Veamos la normalidad de los datos, para saber qué método debemos utilizar
# o si directamente podemos utilizar una prueba paramétrica típica:

# Comprobar normalidad
g_mf <- ggqqplot(datos_df, x = "edad", facet.by = "genero" ,
               palette = c("red", "blue") , color = "genero")
print(g_mf)

# Como en el gráfico Q-Q notamos que muchos puntos escapan
# del margen de normalidad (generado por la función) por lo
# que debemos trabajar los datos problemáticos, al no poder
# utilizar pruebas típicas.

# Como tenemos grupos independientes (mujeres y hombres) con muestras
# de datos problemáticos, utilizaremos la prueba
# de Yuen para dos muestras independientes.

alfa2 <- 0.05

# Vemos cómo son los gráficos si realizáramos una poda:

gamma <- 0.2
n_a <- length(solteros_edad)
n_b <- length (solteras_edad)
poda_a <- n_a*gamma
poda_b <- n_b*gamma
a_truncada <- solteros_edad[poda_a:(n_a-poda_a)]
b_truncada <- solteras_edad[poda_b:(n_b-poda_b)]
edad <- c(a_truncada, b_truncada)
genero <- c(rep("M", length(a_truncada)), rep("F", length(b_truncada)))
datos_truncados <- data.frame(edad, genero)
g <- ggqqplot(datos_truncados, x="edad",facet.by = "genero",
              palette = c("red", "blue") , color = "genero" )
print(g) 

# Vemos que al realizar la poda, el margen de normalidad para los puntos
# se hace más grande y también los datos calzan mejor a esta franja, lo que nos dice
# que un procedimiento con la prueba de Yuen entregará buenos resultados
# ya que internamente utiliza la media truncada y winsorizada.

# Como funciona la poda, utilizamos la prueba de Yuen, con más seguridad.
# Aplicar prueba de Yuen:
prueba_yuen <- yuen(edad ~ genero, data=datos_df, tr=gamma)
print(prueba_yuen)


# Tras realizar la prueba, tenemos que nuestro p = 0.089 y es mayor a alfa.
# Por ello, diremos, con un 95% de confianza, que la edad promedio de
# hombres y mujeres solteras es igual, o bien, fallamos en rechazar
# la hipótesis nula.

# Con esta información, claramente los desarrolladores de la supuesta
# aplicación de citas pueden saber correctamente a qué usuarios dirigir 
# su publicidad.

###############
###############

# 3

# Contexto e hipótesis:

# Dependiendo de cuánto ganan (personas de chile) influye en si van a pie, en vehiculo 
# o en transporte publico en su movilización en el día a día.

# H_0: El ingreso promedio de los hogares es igual para cada grupo que utiliza distintos 
# tipos de transporte para movilizarse.
# H_A: El ingreso promedio de los hogares es diferente para cada grupo que utiliza distintos 
# tipos de transporte para movilizarse.

# Obtención de datos para cada grupo - es el mismo código que para el EP 11.

set.seed(891)
n2 <- 450

datos2 <- datos_anterior %>% sample_n(n2) %>% select(region, ytotcorh, o25c)


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
