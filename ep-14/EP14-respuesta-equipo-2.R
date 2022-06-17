library(dplyr)
library(car)
datos <- read.csv2("EP13 Datos.csv")


# 1.|
set.seed(2231) 

# Como el rut es impar, se toma una muestra de 120 hombres.
tamano_muestra <- 120

# Obetnemos una muestra de todos los hombrees
datos_hombres <- datos %>% filter(Gender == 1)

# Crear variable IMC (Indice de Masa Corporal)
IMC <- datos_hombres$Weight / ( ((datos_hombres$Height)/100) ** 2)
datos_hombres['IMC'] <- IMC



# 2.
# Dividimos la muestra en 60 hombres sobrepeso y no sobrepeso
# utilizando el umbral de 25 unidades para el IMC

# Hombres considerados en la región de no sobrepeso
muestra1 <- datos_hombres %>% filter(IMC < 25 ) %>% sample_n(60)

# Hombres considerados en la región de sobrepeso
muestra2 <- datos_hombres %>% filter(IMC >= 25 ) %>% sample_n(60)


# Consideramos la muestra total
muestra <- bind_rows(muestra1, muestra2)


# 2.
# Creamos la variable EN (Estado Nutricional)

# En este caso consideramos:
# 1: Sobrepeso
# 0: Sin Sobrepeso
EN <- muestra$IMC > 25

# Técnica para convertir booleanos a valores numéricos (1 y 0)
muestra$EN <- EN * 1


# 3.
# Seleccionamos las 8 variables (como en el EP anterior)
# (se copian directamente, ya que la semilla utilizada con anterioridad fue una distinta)
variables <- list("Knees.diameter", "Calf.Maximum.Girth", "Bicep.Girth", "Chest.Girth", "Hip.Girth", "Biiliac.diameter",
               "Knee.Girth", "Biacromial.diameter")

datos_seleccionados <- data.frame(matrix(NA, ncol = 0, nrow = 120))
for(column_name in variables){
  datos_seleccionados[[column_name]] <- muestra[[column_name]]
}

# Añadimos la variable IMC ya que al crear un nuevo dataframe con las variables azarosas elegidas
# no lo terminamos por añadir

datos_seleccionados$IMC <- muestra$IMC

# Añadimos la variable nueva, dicotómica, creada a partir del IMC, EN:
datos_seleccionados$EN <- muestra$EN

# Este es el paso 4, que lo hacemos antes de separar los datos para mejor orden
# (seleccionar una variable que no están dentro de las seleccionadas al azar)
datos_seleccionados$Navel.Girth <- muestra$Navel.Girth

# Seleccionamos 80 personas para la construcción del modelo
# 40 deben tener sobrepeso

datos_modelo_nosobrepeso <- datos_seleccionados[1:40, ]
datos_modelo_sobrepeso <- datos_seleccionados[61:100, ]

datos_modelo <- bind_rows(datos_modelo_sobrepeso, datos_modelo_nosobrepeso)


# Seleccionamos 40 personas para el entrenamiento del modelo
# 20 deben tener sobrepeso

datos_evaluacion_nosobrepeso <- datos_seleccionados[41:60, ]
datos_evaluacion_sobrepeso <- datos_seleccionados[101:120, ]
datos_evaluacion <- bind_rows(datos_evaluacion_sobrepeso, datos_evaluacion_nosobrepeso)


# 4.
# En este caso, utilizaremos la variable Navel.Girth que representa el grosor a la
# altura del ombligo.
# Eso se hizo más arriba para tener todas las variables ya listas.

# Evaluamos la correlación entre EN y Navel.Girth:

cat("Corr(EN, Navel.Girth) = ", cor(datos_modelo$EN, datos_modelo$Navel.Girth))

# En este caso es 0.63, por lo que nos servirá, ya que es positiva y más cercana a 1 que de 0.

# Usando el entorno R y paquetes estándares:


###########################################################################################################
###########################################################################################################
# 5. construir un modelo de regresión logística con el predictor 
# seleccionado en el paso anterior y utilizando de la muestra obtenida.

modelo_inicial <- glm(EN ~ Navel.Girth,
              family = binomial(link = "logit"),
              data = datos_modelo)

print(summary(modelo_inicial))

# Notemos cómo el AIC es de 71.293 para el modelo de regresión logística con una variable.


# 6. Usando herramientas estándares para la exploración de modelos del entorno R, buscar entre dos y 
# cinco predictores de entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar 
# al modelo obtenido en el paso 5.


# Generamos el modelo completo para utilizar una función de selección
# dada por R.

# Sacamos la variable IMC ya que esta se conecta directamente con EN, no sirviendo
# para la predicción (EN se obtiene de IMC)

datos_temp <- datos_modelo
datos_temp$IMC <- NULL


# Se definen, como siempre, los modelos de inicio y el modelo completo
# para mostrar a la función step adónde dirigirse en cada iteración.

modelo_nulo <- glm(EN ~ 1,
                       family = binomial(link = "logit"),
                       data = datos_temp)

modelo_completo <- glm(EN ~ .,
                      family = binomial(link = "logit"),
                      data = datos_temp)


# Utilizamos la función 'step', fijándonos en que 'direction' está seteada en 'forward'
# lo que implica la utilización del algoritmo de selección hacia adelante.

mejor_modelo <- step(modelo_nulo, 
                     scope = list(lower = modelo_nulo, upper = modelo_completo),
                     direction = "forward",
                     trace = 1)

# Veamos el mejor modelo, revisando las iteraciones con el flag de trace en 1
# Seleccionaremos dos variables de entre las 8 que utilizó el algoritmo para encontrar
# el mejor modelo.

# En este caso, al ver cada iteración del algoritmo, notamos que luego de cinco iteraciones
# no puede agregar más predictores al objeto de regresión logística, y se detiene.
# En los últimos pasos solo baja el AIC de 58.78 a 58.32, por lo que nos quedamos con 
# solo dos variables, ya que no cambia mucho agregar al una más.

# Los predictores están dados en la fórmula abajo (Chest.Girth y Calf.Maximum.Girth).

formula <- EN ~  Navel.Girth + Chest.Girth + Calf.Maximum.Girth

modelo_elegido <- glm(formula,
                      family = binomial(link = "logit"),
                      data = datos_modelo)

print(summary(modelo_elegido))

# En este caso tenemos un AIC de 58.783 para el modelo elegido, en comparación
# a 71.293 obtenido solo con una variable (la elegida en el paso 4, Navel.Girth).

###########################################################################################################
###########################################################################################################
# 7. Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de ajuste y son generalizables) 
# y "arreglarlos" en caso de que tengan algún problema.

# Primero verificamos las siguientes condiciones existentes
# para realizar válidamente un modelo de regresión logística:

# 1. Debe existir una relación lineal entre los predictores y la respuesta transformada.

# Utilizando la función 'cor', se especifica la opción 'pearson', ya que existen otros métodos
# según R, como 'kendall' o 'spearman'.

correlaciones_mult_variables <- cor(x = datos_modelo, method = "pearson")
print(correlaciones_mult_variables)

# Recordando que las variables elegidas son 2:
# Para el par (EN, Chest.Girth) tenemos una correlación de 0.667.
# Luego, (EN, Calf.Maximum.Girth) = 0.49.

# Considerando que ambas correlaciones se alejan bastante de 0,
# diremos que se cumple la condición (1). 
# Además, notamos que se ve una relación lineal positiva, es decir, 
# a medida que aumentan ambas Chest.Girth y Calf.Maximum.Girth, es decir, 
# la circunferencia del pecho y máxima circunferencia de la pantorrilla,
# la variable asociada al IMC, 'EN', también crece.


# 2. Los residuos deben ser independientes entre sí.

# En este caso, debemos tener el paquete 'car', para utilizar el test de Durbin-Watson
# donde H_0 determina que no existe correlación entre los residuos.
prueba_residuos <- durbinWatsonTest(modelo_elegido)
print(prueba_residuos)

###########################################################################################################
###########################################################################################################
# 8. Usando código estándar, evaluar el poder predictivo de los modelos con los datos de las personas que
# no se incluyeron en su construcción en términos de sensibilidad y especificidad.
