library(dplyr)
library(ggpubr)
library(car)
library(caret)
# 1. Definir la semilla a utilizar, que corresponde a los �ltimos cuatro d�gitos 
# del RUN (sin considerar el d�gito verificador) del integrante de menor edad del equipo.

# 2. Seleccionar una muestra de 50 mujeres (si la semilla es un n�mero par) o 
# 50 hombres (si la semilla es impar).

# 0869
set.seed(869)

datos <- read.csv2("EP13 Datos.csv") %>% filter(Gender == 1)
n <- 50
muestra_hombres <- datos %>% sample_n(n)

# 3. Seleccionar de forma aleatoria ocho posibles variables predictoras.

variables <- data.frame(colnames(muestra_hombres)) %>% sample_n(8)

# sample_data <- data[sample(nrow(muestra_hombres), 8, replace = FALSE), ]

datos_seleccionados <- data.frame()
for(column in variables){
  datos_seleccionados <- data.frame(muestra_hombres[column])
}

datos_seleccionados[['Weight']] <- muestra_hombres[["Weight"]]
datos_seleccionados[['Thigh.Girth']] <- muestra_hombres[["Thigh.Girth"]]
 
# 4. Seleccionar, de las otras variables, una que el equipo considere que podr�a ser �til para predecir la variable Peso, 
# justificando bien esta selecci�n.

# Seleccionaremos la variable Thigh.Girth
# que es el grosor promedio de ambos muslos bajo el pliegue del gl�teo.
# Se selecciona esta variable ya que mientras m�s grande sea el grosor
# de los muslos puede haber m�s masa o m�sculo que finalmente 
# podr�a contribuir al peso total.


peso <- muestra_hombres$Weight
modelo1 <- lm(muestra_hombres$Weight ~ muestra_hombres$Thigh.Girth)

# 5. Usando el entorno R, construir un modelo de regresi�n lineal 
# simple con el predictor seleccionado en el paso anterior.

g <- ggplot(muestra_hombres, aes(x = Thigh.Girth, y = peso)) + 
     geom_point() + stat_smooth(method = "lm")

# 6. Usando herramientas para la exploraci�n de modelos del entorno R, buscar entre dos y 
# cinco predictores de entre las variables seleccionadas al azar en el punto 3, para agregar
# al modelo de regresi�n lineal simple obtenido en el paso 5.


# Comenzamos con el modelo nulo para llegar al completo (con las ocho variables aleatorias seleccionadas)

modelo_anterior <- lm(Weight ~ Thigh.Girth, data = datos_seleccionados)
completo <- lm(Weight ~ ., data = datos_seleccionados)

step(modelo_anterior, scope = list(upper = completo), direction = "forward", trace = 1)

# Al ver la traza de la funci�n step, notamos que agregando las variables Hip.Girth y Chest.Girth (grosor de la cadera y el pecho)
# el AIC disminuye, por lo que nos da indicios de que estas variables mejoran el modelo en cierta forma.

formula <- Weight ~ Thigh.Girth + Hip.Girth + Chest.Girth

modelo_nuevo <- lm(formula, data = datos_seleccionados)
plot(modelo_nuevo)

# Con el primer gr�fico de los residuos obtenido con la funci�n plot() notamos que estos no se
# desv�an tanto de la normal, aunque existen valores at�picos.

# 7. Evaluar los modelos y "arreglarlos" en caso de que tengan alg�n problema con las condiciones que deben cumplir.

# Primero evaluamos las condiciones m�s simples:

# 1. Las variables predictoras son num�ricas a nivel de intervalo, ya que se tratan de magnitudes f�sicas medibles.
# 2. Independencia: Las medidas son medidas corporales de distintas personas, las cuales se asumen escogidas aleatoreamente.
# 3. La variable dependiente es num�rica: la variable dependiente en este caso es el peso (columna 'Weight') y tambi�n
# se trata de una medida f�sica, que es num�rica a nivel de intervalo.

# Se eval�an las condiciones complejas:

# Definiendo a alpha como 0.05:

# 4. Independencia de los residuos.
# Para esta condici�n se utilizar� la prueba de Durbin Watson:
test_indep_residuos <- durbinWatsonTest(modelo_nuevo)

# La hip�tesis nula del test dice que los residuos no est�n autocorrelacionados, es decir,
# los residuos son independientes.
# Como obtenemos un valor p = 0.126, con un nivel de significancia de 0.05
# no rechazamos H_0 y se verifica la condici�n 4.

# 5. Los residuos siguen una distribuci�n normal.
# Como ya lo hemos hecho muchas veces anteriormente, para verificar la normalidad
# de los residuos utilizaremos la prueba de Shapiro.
residuos_normales <- shapiro.test(modelo_nuevo$residuals)
p_shapiro <- residuos_normales$p.value

# Nuevamente obtenemos un p-valor de 0.158 > alpha, por lo tanto, los residuos siguen
# una distribuci�n normal.

# 6. Homocedasticidad de los residuos.
# Para verificar la condicion se utilizar� la prueba Breusch-Pagan-Godfrey,
# cuya hip�tesis nula nos dice que las varianzas de los residuos son iguales.
test_homoced <- ncvTest(modelo_nuevo)
p_homoced <- test_homoced$p

# Nuevamente obtenemos un resultado favorable ya que p = 0.674 > alpha,
# fallando en rechazar la hip�tesis nula, es decir, las varianzas de los residuos
# son iguales.

# 7. Por �ltimo, comprobamos la multicolinealidad

# Esto se realiza a trav�s del factor de inflaci�n de varianza y el estad�stico de tolerancia.

vifs <- vif(modelo_nuevo)
print(vifs)
print(1/vifs)

# De los valores anteriores, notamos que los factores de inflaci�n de varianza no
# tienen niveles preocupantes ( todos < a 10),
# Sin embargo la tolerancia (denotada por 1/vifs) tiene un valor preocupante para
# la variable Hip.Girth, por lo que podr�amos desistir de utilizar esta variable
# en el modelo en caso de que necesitaramos un modelo muy riguroso.
# A�n as�, el modelo cumple todas las condiciones anteriores al punto (7)
# solo teniendo un conflicto con una variable, por lo que concluimos que construimos
# un modelo aceptable.

# 8. Evaluar el poder predictivo del modelo en datos no utilizados 
# para construirlo (o utilizando validaci�n cruzada).

# Utilizamos validaci�n cruzada:

n <- nrow(datos_seleccionados)

n_entrenamiento <- floor(0.75 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)

# Separar datos de entrenamiento y prueba 
entrenamiento <- datos_seleccionados[muestra, ]
prueba <- datos_seleccionados[-muestra, ]

# Creamos un modelo con la f�rmula elegida anteriormente tras ver las variables que mejoran el modelo
# utilizando los datos de entrenamiento

modelo_2 <- lm(formula, data = entrenamiento)
print(summary(modelo_2))

mse_entrenamiento <- mean(modelo_2$residuals ** 2)
cat("MSE para el conjunto de entrenamiento: ", mse_entrenamiento)

# Predecimos los valores utilizando las observaciones del modelo de prueba
predicciones <- predict(modelo_2, prueba)

error <- prueba$Weight - predicciones
mse_prueba <- mean(error ** 2)
cat("MSE para el conjunto de prueba: ", mse_prueba)

# Usando el segundo m�todo de la validaci�n cruzada de k-pliegues
# modelo3 <- train(formula, data = entrenamiento, method = "lm",
                 # trControl = trainControl(method = 'cv', number = 5))

# Predicciones para el conjunto de entrenamiento 

# predicciones_entrenamiento <- predict(modelo3, entrenamiento)
# 
# # Calcular mse para el conjunto de entrenamiento
# error_entrenamiento <- entrenamiento[["Weight"]] - predicciones_entrenamiento
# mse_entrenamiento <- mean(error_entrenamiento ** 2)
# cat("MSE para el conjunto de entrenamiento: ", mse_entrenamiento, "\n")
# 
# ###
# 
# 
# # Predicciones para el conjunto de prueba
# 
# 
# predicciones_prueba <- predict(modelo3, prueba)
# 
# # C�lculo MSE para el conjunto de prueba
# error_prueba <- prueba[["Weight"]]  - predicciones_prueba
# mse_prueba <- mean(error_prueba ** 2)
# cat("MSE para el conjunto de prueba: ", mse_prueba, "\n")

# Vemos en la secci�n comentada que los valores para el MSE son iguales, por lo que dejamos
# la opci�n m�s simple de validaci�n cruzada sin utilizar la funci�n trainControl.
# En este caso, el MSE para el conjunto de entrenamiento fue de 10.472 y
# 26.507 para el conjunto de prueba.
# Notando que la escala de la variable est� en [Kg], una diferencia de 10.472 [Kg] y 26.507 [Kg] 
# es bastante notoria, y por se concluye que el error es bastante grande, corroborando que
# el modelo no es muy generalizable, debido a este abrupto cambio en el MSE al predecir
# nuevos valores no vistos antes por el modelo.
