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

# Hombres considerados en la regi�n de no sobrepeso
muestra1 <- datos_hombres %>% filter(IMC < 25 ) %>% sample_n(60)

# Hombres considerados en la regi�n de sobrepeso
muestra2 <- datos_hombres %>% filter(IMC >= 25 ) %>% sample_n(60)


# Consideramos la muestra total
muestra <- bind_rows(muestra1, muestra2)


# 2.
# Creamos la variable EN (Estado Nutricional)

# En este caso consideramos:
# 1: Sobrepeso
# 0: Sin Sobrepeso
EN <- muestra$IMC >= 25

# T�cnica para convertir booleanos a valores num�ricos (1 y 0)
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


# Vemos las estad�sticas descriptivas de cada variable (en columnas)
print(summary(datos_seleccionados))

# A�adimos la variable IMC ya que al crear un nuevo dataframe con las variables azarosas elegidas
# no lo terminamos por a�adir

datos_seleccionados$IMC <- muestra$IMC

# A�adimos la variable nueva, dicot�mica, creada a partir del IMC, EN:
datos_seleccionados$EN <- muestra$EN

# Este es el paso 4, que lo hacemos antes de separar los datos para mejor orden
# (seleccionar una variable que no est�n dentro de las seleccionadas al azar)
datos_seleccionados$Navel.Girth <- muestra$Navel.Girth

# Seleccionamos 80 personas para la construcci�n del modelo
# 40 deben tener sobrepeso


datos_modelo_sobrepeso <- datos_seleccionados %>% filter(EN == 1) %>% sample_n(40, replace = FALSE)
datos_modelo_restante <- datos_seleccionados %>% sample_n(40, replace = FALSE)
datos_modelo <- bind_rows(datos_modelo_sobrepeso, datos_modelo_restante)


# Seleccionamos 40 personas para el entrenamiento del modelo
# 20 deben tener sobrepeso

datos_evaluacion_sobrepeso <- datos_seleccionados %>% filter(EN == 1) %>% sample_n(20, replace = FALSE)
datos_evaluacion_restante <- datos_seleccionados %>% sample_n(20, replace = FALSE)
datos_evaluacion <- bind_rows(datos_evaluacion_sobrepeso, datos_evaluacion_restante)


# 4.
# En este caso, utilizaremos la variable Navel.Girth que representa el grosor a la
# altura del ombligo.
# Eso se hizo m�s arriba para tener todas las variables ya listas.

# Evaluamos la correlaci�n entre EN y Navel.Girth:

cat("Corr(EN, Navel.Girth) = ", cor(datos_modelo$EN, datos_modelo$Navel.Girth))

# En este caso es 0.63, por lo que nos servir�, ya que es positiva y m�s cercana a 1 que de 0.

# Usando el entorno R y paquetes est�ndares:


###########################################################################################################
###########################################################################################################
# 5. construir un modelo de regresi�n log�stica con el predictor 
# seleccionado en el paso anterior y utilizando de la muestra obtenida.

modelo_inicial <- glm(EN ~ Navel.Girth,
              family = binomial(link = "logit"),
              data = datos_modelo)

print(summary(modelo_inicial))

# Notemos c�mo el AIC es de 71.293 para el modelo de regresi�n log�stica con una variable.


# 6. Usando herramientas est�ndares para la exploraci�n de modelos del entorno R, buscar entre dos y 
# cinco predictores de entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar 
# al modelo obtenido en el paso 5.


# Generamos el modelo completo para utilizar una funci�n de selecci�n
# dada por R.

# Sacamos la variable IMC ya que esta se conecta directamente con EN, no sirviendo
# para la predicci�n (EN se obtiene de IMC)

datos_temp <- datos_modelo
datos_temp$IMC <- NULL


# Se definen, como siempre, los modelos de inicio y el modelo completo
# para mostrar a la funci�n step ad�nde dirigirse en cada iteraci�n.

modelo_nulo <- glm(EN ~ 1,
                       family = binomial(link = "logit"),
                       data = datos_temp)

modelo_completo <- glm(EN ~ .,
                      family = binomial(link = "logit"),
                      data = datos_temp)


# Utilizamos la funci�n 'step', fij�ndonos en que 'direction' est� seteada en 'forward'
# lo que implica la utilizaci�n del algoritmo de selecci�n hacia adelante.

mejor_modelo <- step(modelo_nulo, 
                     scope = list(lower = modelo_nulo, upper = modelo_completo),
                     direction = "forward",
                     trace = 1)

# Veamos el mejor modelo, revisando las iteraciones con el flag de trace en 1
# Seleccionaremos dos variables de entre las 8 que utiliz� el algoritmo para encontrar
# el mejor modelo.

# En este caso, al ver cada iteraci�n del algoritmo, notamos que luego de cinco iteraciones
# no puede agregar m�s predictores al objeto de regresi�n log�stica, y se detiene.
# En los �ltimos pasos solo baja el AIC de 58.78 a 58.32, por lo que nos quedamos con 
# solo dos variables, ya que no cambia mucho agregar al una m�s.

# Los predictores est�n dados en la f�rmula abajo (Chest.Girth y Calf.Maximum.Girth).

formula <- EN ~  Navel.Girth + Chest.Girth + Calf.Maximum.Girth

modelo_elegido <- glm(formula,
                      family = binomial(link = "logit"),
                      data = datos_modelo)

print(summary(modelo_elegido))

# En este caso tenemos un AIC de 58.783 para el modelo elegido, en comparaci�n
# a 71.293 obtenido solo con una variable (la elegida en el paso 4, Navel.Girth).

###########################################################################################################
###########################################################################################################
# 7. Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de ajuste y son generalizables) 
# y "arreglarlos" en caso de que tengan alg�n problema.

# Primero verificamos las siguientes condiciones existentes
# para realizar v�lidamente un modelo de regresi�n log�stica:

# 1. Debe existir una relaci�n lineal entre los predictores y la respuesta transformada.

# Utilizando la funci�n 'cor', se especifica la opci�n 'pearson', ya que existen otros m�todos
# seg�n R, como 'kendall' o 'spearman'.

correlaciones_mult_variables <- cor(x = datos_modelo, method = "pearson")
print(correlaciones_mult_variables)

# Recordando que las variables elegidas son 2:
# Para el par (EN, Chest.Girth) tenemos una correlaci�n de 0.667.
# Luego, (EN, Calf.Maximum.Girth) = 0.49.

# Considerando que ambas correlaciones se alejan bastante de 0,
# diremos que se cumple la condici�n (1). 
# Adem�s, notamos que se ve una relaci�n lineal positiva, es decir, 
# a medida que aumentan ambas Chest.Girth y Calf.Maximum.Girth, es decir, 
# la circunferencia del pecho y m�xima circunferencia de la pantorrilla,
# la variable asociada al IMC, 'EN', tambi�n crece.


# 2. Los residuos deben ser independientes entre s�.

# En este caso, debemos tener el paquete 'car', para utilizar el test de Durbin-Watson
# donde H_0 determina que no existe autocorrelaci�n entre los residuos.
prueba_residuos <- durbinWatsonTest(modelo_elegido)
cat("Prueba de independencia de residuos (D-W-Test)/P-value = ", prueba_residuos$p, "\n")

# Vemos que se falla en rechazar la hip�tesis nula, es decir, los residuos son independientes.

# Por �ltimo, verificamos la multicolinealidad entre los predictores, usando
# el factor de inflaci�n de la varianza (VIF) y las tolerancias, como en el EP anterior.

vifs <- vif(modelo_elegido)
cat("VIFs:\n", vifs, "\n")

cat("Tolerancias:\n", 1 / vifs, "n")
cat("mean(VIFs) = ", mean(vifs), "\n")

# De lo anterior, notamos que el promedio de los VIF para las variables
# es de 1.16, es decir, se aleja en una proporci�n m�nima de 1,
# mostrando que el modelo no se encuentra sesgado (preocuparse si est� muy lejano de 1).
# Por otro lado, las tolerancias son mayores a 0.2.
# Por ello, al cumplirse (1), (2) y la multicolinealidad no necesita ser corregido,
# concluimos que estamos ante un modelo, por lo menos, v�lido para ser utilizado formalmente.


# Aparte de la evaluaci�n de condiciones anterior, esto solo nos habla de la validez del modelo,
# es decir, sabemos que estamos tomando un modelo coherente. Sin embargo, podr�amos mejorar
# este mismo viendo los valores con sobreinfluencia y valores at�picos.


# Detectar valores at�picos:
plot(modelo_elegido)

# Notamos en el primer gr�fico entregado al utilizar plot() sobre el modelo log�stico m�ltiple
# solo dos valores que se ven at�picos dentro de los residuos.

# Residuos y estad�sticas:
output <- data.frame(predicted.probabilities = fitted(modelo_elegido))
output[["standardized.residuals"]] <- rstandard(modelo_elegido)
output[["studentized.residuals"]] <- rstudent(modelo_elegido)
output[["cooks.distance"]] <- cooks.distance(modelo_elegido)
output[["dfbeta"]] <- dfbeta(modelo_elegido)
output[["dffit"]] <- dffits(modelo_elegido)
output[["leverage"]] <- hatvalues(modelo_elegido)

# Evaluar residuos estandarizados que escapen a la normalidad 
# 95 % de los residuos estandarizados deber�an estar entre
# -1.96 y 1.96 , y 99 % entre -2.58 y 2.58 
sospechosos1 <- which(abs(output[["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort(sospechosos1) 
cat("\n")
cat(" Residuos estandarizados fuera del 95% esperado\n ")
print(rownames(datos_modelo[sospechosos1,]))

# Como ve�amos anteriormente, estos son los dos valores que observ�bamos en el gr�fico de plot(modelo_elegido).

# Revisar casos con distancia de Cook mayor a uno
sospechosos2 <- which(output[["cooks.distance"]] > 1)
# En este punto vemos que length(sospechosos2) = 0, por lo tanto, no existen casos de este tipo (D.C > 1)

# Revisar casos cuyo apalancamiento sea m�s del doble
# o triple del apalancamiento promedio
leverage.promedio <- ncol(datos_modelo)/nrow(datos_seleccionados)
sospechosos3 <- which(output[["leverage"]] > leverage.promedio)
sospechosos3 <- sort ( sospechosos3 )
cat("\n\n Residuales con levarage fuera de rango ( > ")
cat(round(leverage.promedio , 3), " ) " , " \n " , sep = "" )
print(rownames(datos_modelo[ sospechosos3 , ]))

# En este caso, nos podemos "preocupar", ya que ahora existen m�s valores
# con apalancamiento fuera de rango, sin embargo, hemos tenido 
# buenas evaluaciones en las secciones anteriores.

# Revisar casos con DFBeta >= 1
sospechosos4 <- which(apply(output[["dfbeta"]] >= 1, 1, any))
sospechosos4 <- sort(sospechosos4)
names(sospechosos4) <- NULL
cat("\n\n Residuales con DFBeta sobre 1 \n" )
print(rownames(datos_modelo[sospechosos4, ]))

# Ac� encontramos nuevos valores con DFBeta sobre 1, que podr�amos evaluar eliminar.


# Detalle de las observaciones posiblemente at�picas:
# En este caso, ser�a una especie de "summarise" para ver el panorama completo.

sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4)
sospechosos <- sort(unique(sospechosos))
cat("\n\n Casos sospechosos \n" )
print(datos_modelo[sospechosos , ])
cat("\n\n")
print(output[sospechosos, ])

# Como vimos que nuestro modelo cumpl�a bastante bien las condiciones (1), (2) y la multicolinealidad
# se decide no modificar ning�n dato, ni eliminar variables, ya que vimos que efectivamente disminuye
# el AIC con las que ya tenemos.


# Ahora, falta evaluar el poder predictivo en el punto 8.

###########################################################################################################
###########################################################################################################
# 8. Usando c�digo est�ndar, evaluar el poder predictivo de los modelos con los datos de las personas que
# no se incluyeron en su construcci�n en t�rminos de sensibilidad y especificidad.

# Se utilizar� el dataframe "datos_evaluacion" formado desde 
# la separacion de 40 datos desde una muestra de 120 hombres, en donde 60 ten�an sobrepeso y 60 no.
# Se asegura que al menos 20 hombres tienen sobrepeso en estos 40 datos.

# Evaluaci�n para modelo m�ltiple en RLog:

umbral <- 0.5
probs_p <- predict(modelo_elegido, datos_evaluacion,
                   type = "response")

preds_p <- sapply(probs_p, function(p) ifelse(p >= umbral, "1", "0"))
preds_p <- factor(preds_p, levels = levels(datos_seleccionados[["EN"]]))


# Obtenemos la curva de calibraci�n
# En este caso evaluamos la precisi�n del modelo
# Vemos que la curva  se aleja bastante de la diagonal, mostrando una precisi�n aceptable.
# (En este caso, nos referimos a 'curva' aunque se grafica como escalones)
ROC_p1 <- roc(datos_evaluacion[["EN"]], probs_p)
plot(ROC_p)

# Evaluaci�n para modelo de una variable (la elegida: Navel.Girth - grosor del ombligo)

probs_p <- predict(modelo_inicial, datos_evaluacion,
                   type = "response")

preds_p <- sapply(probs_p, function(p) ifelse(p >= umbral, "1", "0"))
preds_p <- factor(preds_p, levels = levels(datos_seleccionados[["EN"]]))

ROC_p2 <- roc(datos_evaluacion[["EN"]], probs_p)
plot(ROC_p)


# Tambi�n podemos sobreponer los gr�ficos para evaluar los modelos a la vez:
plot(ROC_p1, col = 1, lty = 2, main = "Gr�ficos curvas ROC")
plot(ROC_p2, col = 4, lty = 3, add = TRUE) 

# En este caso, la curva azul corresponde a la curva para el modelo de una variable.
# Tenemos indicios de haber mejorado correctamente el modelo al adicionar las dos variables
# nombradas en el paso 6.

# En consecuencia, diremos que el modelo con tres predictores ajusta mejor nuestros datos
# que el modelo que considera solamente el grosor del ombligo (en el objeto de modelo_inicial).



