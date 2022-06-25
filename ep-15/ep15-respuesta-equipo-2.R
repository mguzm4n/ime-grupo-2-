library(dplyr)
library(caret)
library(leaps)
library(pROC)
datos <- read.csv2("EP13 Datos.csv")


# 1.
# Setear semilla con el rut, como se pide.

set.seed(2231) 

# Como el rut es impar, se toma una muestra de 120 hombres.
tamano_muestra <- 100

# Crear variable IMC (Indice de Masa Corporal)
IMC <- datos$Weight / ( ((datos$Height)/100) ** 2)
datos['IMC'] <- IMC

# En este caso consideramos:
# 1: Sobrepeso, 0: Sin sobrepeso
EN <- datos$IMC >= 25

# Técnica para convertir booleanos a valores numéricos (1 y 0)
datos$EN <- EN * 1


# 2.

muestra1 <- datos %>% filter(IMC < 25 ) %>% sample_n(50)
muestra2 <- datos %>% filter(IMC >= 25 ) %>% sample_n(50)

datos_total <- bind_rows(muestra1, muestra2)
 
################################################################################
################################################################################
# 3. Usando las herramientas del paquete leaps, realizar una búsqueda exhaustiva para seleccionar entre dos 
# y ocho predictores que ayuden a estimar la variable Peso (Weight), obviamente sin considerar las nuevas 
# variables IMC ni EN, y luego utilizar las funciones del paquete caret para construir un modelo de regresión 
# lineal múltiple con los predictores escogidos y evaluarlo usando bootstrapping.

datos_modelo <- datos_total
datos_modelo$EN <- NULL
datos_modelo$IMC <- NULL
modelos <- regsubsets(Weight ~ ., 
                     data = datos_modelo, 
                     method = 'exhaustive',
                     nbest = 3,
                     nvmax = 8)

plot(modelos)

# Del gráfico, mirando el eje 'y' (donde se especifica el BIC)
# seleccionamos las variables:
# Shoulder.Girth, Waist.Girth, Hip.Girth
# Calf.Maximum.Girth, Age y Height.

formula <- Weight ~ Shoulder.Girth + Waist.Girth + Hip.Girth + Calf.Maximum.Girth + Age + Height


# Configuramos el objeto para el método de evaluación, utilizando Bootstrapping con 1000 repeticiones.
control <- trainControl(method = "boot",
                        number = 1000)

# Evaluar el modelo
evaluacion <- train(formula, 
                    data = datos_modelo,
                    trControl = control,
                    method = "lm")

# Vemos que efectivamente la función train efectúa el método
# de remuestreo Bootstrapping utilizando 1000 repeticiones.
# En este caso, obtenemos un RMSE de 2.62 [Kg], lo cual nos da un 
# valor bastane pequeño considerando que hablamos de Kilogramos.

################################################################################
################################################################################
# 4.Haciendo un poco de investigación sobre el paquete caret, en particular cómo 
# hacer Recursive Feature Elimination (RFE), construir un modelo de regresión lineal
# múltiple para predecir la variable IMC que incluya entre 10 y 20 predictores,
# seleccionando el conjunto de variables que maximice R2 y que use cinco repeticiones
# de validación cruzada de cinco pliegues para evitar el sobreajuste (obviamente no se
# debe considerar las variables Peso,  Estatura ni estado nutricional -Weight, Height, EN respectivamente).

# Nuestra investigación se basa en la información del siguiente link:
# https://www.projectpro.io/recipes/use-rfe-r
# https://topepo.github.io/caret/recursive-feature-elimination.html


# Restauramos variable IMC eliminada en paso 3.
datos_modelo$IMC <- datos_total$IMC

# Particionamos el set de datos

# Particiones: 0.8 entrenamiento, 0.2 evaluación
particiones <- createDataPartition(datos_modelo$IMC, 
                                   p = 0.8, 
                                   list = FALSE)

entrenamiento <- datos_modelo[particiones, ]
evaluacion_1 <- datos_modelo[-particiones, ]


# Este será el dataframe de 'features'
x_entrenamiento <- entrenamiento[, -26] # selecciono todo menos IMC

# EN ya estaba fuera. Eliminamos altura y peso.
x_entrenamiento$Weight <- NULL
x_entrenamiento$Height <- NULL

# Dataframe de la variable de respuesta 
y_entrenamiento <- entrenamiento$IMC # seleccionar solo columna de IMC



# Implementar la "Recursive Feature Selection":


# Configuramos el objeto de control para función rfe.
# Se configura para utilizar una validación cruzada de 5 pliegues
# con 5 repeticiones.

control <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv", # Especificar que se repite la validación cruzada
                   number = 5, # 5-Fold Cross Validation
                   repeats = 5, # Simplemente las repeticiones de la validación cruzada
                   verbose = FALSE)


# Aplicamos RFE, especificando que queremos de entre 15 a 20 variables seleccionadas.

set.seed(2231) # Debemos poner la semilla inmediatamente antes - si no hacemos esto siempre se generará modelo nuevo
feature_select_model <- rfe(x = x_entrenamiento,
                            y = y_entrenamiento,
                            sizes = c(15:20),
                            rfeControl = control)

# La función rfe nos entrega por sí sola el top cinco de las variables.
# Para visualizar todos los que ha seleccionado, utilizamos la función predictors().
# Obtenemos los predictores que selecciona la función RFE
# obteniendo 15 en total:

print(predictors(feature_select_model))


################################################################################
################################################################################
# 5. Usando RFE, construir un modelo de regresión logística múltiple para la variable 
# EN que incluya el conjunto, de entre dos y seis, predictores que entregue la mejor 
# curva ROC y que utilice validación cruzada dejando uno fuera para evitar el sobreajuste 
# (obviamente no se debe considerar las variables Peso, Estatura -Weight y Height respectivamente- ni IMC).


# Agregamos variable EN ya que antes se había eliminado

datos_modelo$EN <- as.factor(datos_total$EN) # Arreglar la variable categórica, para que no se muestre el warning (aun funciona sin este)


# Particionamos el set de datos
# Particiones: 0.8 entrenamiento, 0.2 evaluación para el modelo RLog
particiones <- createDataPartition(datos_modelo$EN, 
                                   p = 0.8, 
                                   list = FALSE)

entrenamiento <- datos_modelo[particiones, ]
evaluacion_2 <- datos_modelo[-particiones, ]


# Este será el dataframe de 'features'
x_entrenamiento <- entrenamiento[, -27] # quitar EN (index 24)
# Eliminamos variables que obviamente predicen bien el EN, ya que fue creada a partir de estos.
x_entrenamiento$IMC <- NULL
x_entrenamiento$Weight <- NULL
x_entrenamiento$Height <- NULL


# Dataframe consistente solo de la var. EN, para introducir a la función RFE:
y_entrenamiento <- entrenamiento$EN # seleccionar solo columna de EN



# Configuramos el objeto de control para función rfe específicamente
# para que utilice regresión logística: por ello, se utilizan las funciones desde
# lrFuncs (logistic regression functions).


# Agregamos la función que permite utilizar la métrica ROC (investigado en Google)

lrFuncs$summary <- twoClassSummary
control <- rfeControl(functions = lrFuncs,
                      method = "LOOCV", # Especificar "leave one out" Cross Validation.
                      number = 5, # k = 5 pliegues,
                      verbose = FALSE)


# Aplicamos Recursive Feature Elim. - especificando entre 2 a 6 variables.

set.seed(2231) # Debemos poner la semilla inmediatamente antes - si no hacemos esto siempre se generará modelo nuevo
feature_select_model_2 <- rfe(x = x_entrenamiento,
                            y = y_entrenamiento,
                            sizes = c(2:6),
                            rfeControl = control,
                            metric = "ROC")

# Vemos que obtenemos warnings - se deben mayoritariamente a los diferentes
# modelos que la función rfe intenta crear al realizar la selección de variables.

# Finalmente, obtenemos los predictores:

print(predictors(feature_select_model_2))

# Ahora se enumeran, ya que son solo dos, no como en el caso anterior donde
# resultaba en 15.

# Waist.Girth y Calf.Maximum.Girth

# Además, haciendo una llamada al objeto feature_select_model_2, notamos que
# efectivamente este modelo entrega la mejor curva ROC (Area Under the Curve/ROC = 0.9231).
print(feature_select_model_2)
# Variables    ROC
# 2          0.9175 - notamos la mejor curva (AUC -> area bajo la curva ROC > más cercana a 1).
# 3          0.9144 - Notamos que con 3 o 4 variables tampoco existe mucha diferencia.
# 4          0.9125         
# 5          0.8994 - acá disminuye bastante   
# 6          0.9081         
# 23         0.8772 - este es el modelo completo que genera el modelo para hacer la eliminación de features

# Así, nos quedamos con las dos variables mencionadas antes, por simplicidad.

################################################################################
################################################################################
# 6. Pronunciarse sobre la confiabilidad y el poder predictivo de los modelos obtenidos.


# Evaluamos el poder predictivo, utilizando los datos de entrenamiento separados en los puntos
# 4 y 5, para estos dos modelos.

# Para el modelo de regresión lineal múltiple (pto. 4) tenemos:
predict_m1 <- predict(feature_select_model$fit, evaluacion_1)
error_m1 <- evaluacion_1$IMC - predict_m1 # Recordamos que la var. de respuesta era el IMC.
mse_m1 <- mean(error_m1 ** 2)
cat("MSE_mlr = ", mse_m1, "\n\n")

# Vemos que el MSE es de 1.62: un valor bastante bajo, mostrando que tiene un buen poder predictivo,
# de apenas +-1.62 para cada predicción.

# Ahora evaluamos el modelo de regresión logística obtenido con recursive feature elimination:
probs <- predict(feature_select_model_2$fit, 
                 evaluacion_2, 
                 type = "response")

# Seteamos el umbral de decisión
umbral <- 0.5

preds_e <- sapply(probs, function(p) ifelse(p >= umbral, "1", "0"))
preds_e <- factor(preds_e, levels = levels(entrenamiento[["EN"]]))
ROC_e <- roc(evaluacion_2[["EN"]], probs)
plot(ROC_e)

# Vemos que la curva se aleja bastante de la línea recta, lo que es un indicio favorable.
# Calculemos el AUC (área bajo ella) para medir qué tan buena es la curva numéricamente.

cat("AUC = ", auc(evaluacion_2[["EN"]], probs), "\n")

