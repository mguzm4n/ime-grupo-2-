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

# T�cnica para convertir booleanos a valores num�ricos (1 y 0)
datos$EN <- EN * 1


# 2.

muestra1 <- datos %>% filter(IMC < 25 ) %>% sample_n(50)
muestra2 <- datos %>% filter(IMC >= 25 ) %>% sample_n(50)

datos_total <- bind_rows(muestra1, muestra2)
 
################################################################################
################################################################################
# 3. Usando las herramientas del paquete leaps, realizar una b�squeda exhaustiva para seleccionar entre dos 
# y ocho predictores que ayuden a estimar la variable Peso (Weight), obviamente sin considerar las nuevas 
# variables IMC ni EN, y luego utilizar las funciones del paquete caret para construir un modelo de regresi�n 
# lineal m�ltiple con los predictores escogidos y evaluarlo usando bootstrapping.

datos_modelo <- datos_total
datos_modelo$EN <- NULL
datos_modelo$IMC <- NULL
modelos <- regsubsets(Weight ~ ., 
                     data = datos_modelo, 
                     method = 'exhaustive',
                     nbest = 3,
                     nvmax = 8)

plot(modelos)

# Del gr�fico, mirando el eje 'y' (donde se especifica el BIC)
# seleccionamos las variables:
# Shoulder.Girth, Waist.Girth, Hip.Girth
# Calf.Maximum.Girth, Age y Height.

formula <- Weight ~ Shoulder.Girth + Waist.Girth + Hip.Girth + Calf.Maximum.Girth + Age + Height


# Configuramos el objeto para el m�todo de evaluaci�n, utilizando Bootstrapping con 1000 repeticiones.
control <- trainControl(method = "boot",
                        number = 1000)

# Evaluar el modelo
evaluacion <- train(formula, 
                    data = datos_modelo,
                    trControl = control,
                    method = "lm")

# Vemos que efectivamente la funci�n train efect�a el m�todo
# de remuestreo Bootstrapping utilizando 1000 repeticiones.
# En este caso, obtenemos un RMSE de 2.62 [Kg], lo cual nos da un 
# valor bastane peque�o considerando que hablamos de Kilogramos.

################################################################################
################################################################################
# 4.Haciendo un poco de investigaci�n sobre el paquete caret, en particular c�mo 
# hacer Recursive Feature Elimination (RFE), construir un modelo de regresi�n lineal
# m�ltiple para predecir la variable IMC que incluya entre 10 y 20 predictores,
# seleccionando el conjunto de variables que maximice R2 y que use cinco repeticiones
# de validaci�n cruzada de cinco pliegues para evitar el sobreajuste (obviamente no se
# debe considerar las variables Peso,  Estatura ni estado nutricional -Weight, Height, EN respectivamente).

# Nuestra investigaci�n se basa en la informaci�n del siguiente link:
# https://www.projectpro.io/recipes/use-rfe-r
# https://topepo.github.io/caret/recursive-feature-elimination.html


# Restauramos variable IMC eliminada en paso 3.
datos_modelo$IMC <- datos_total$IMC

# Particionamos el set de datos

# Particiones: 0.8 entrenamiento, 0.2 evaluaci�n
particiones <- createDataPartition(datos_modelo$IMC, 
                                   p = 0.8, 
                                   list = FALSE)

entrenamiento <- datos_modelo[particiones, ]
evaluacion_1 <- datos_modelo[-particiones, ]


# Este ser� el dataframe de 'features'
x_entrenamiento <- entrenamiento[, -26] # selecciono todo menos IMC

# EN ya estaba fuera. Eliminamos altura y peso.
x_entrenamiento$Weight <- NULL
x_entrenamiento$Height <- NULL

# Dataframe de la variable de respuesta 
y_entrenamiento <- entrenamiento$IMC # seleccionar solo columna de IMC



# Implementar la "Recursive Feature Selection":


# Configuramos el objeto de control para funci�n rfe.
# Se configura para utilizar una validaci�n cruzada de 5 pliegues
# con 5 repeticiones.

control <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv", # Especificar que se repite la validaci�n cruzada
                   number = 5, # 5-Fold Cross Validation
                   repeats = 5, # Simplemente las repeticiones de la validaci�n cruzada
                   verbose = FALSE)


# Aplicamos RFE, especificando que queremos de entre 15 a 20 variables seleccionadas.

set.seed(2231) # Debemos poner la semilla inmediatamente antes - si no hacemos esto siempre se generar� modelo nuevo
feature_select_model <- rfe(x = x_entrenamiento,
                            y = y_entrenamiento,
                            sizes = c(15:20),
                            rfeControl = control)

# La funci�n rfe nos entrega por s� sola el top cinco de las variables.
# Para visualizar todos los que ha seleccionado, utilizamos la funci�n predictors().
# Obtenemos los predictores que selecciona la funci�n RFE
# obteniendo 15 en total:

print(predictors(feature_select_model))


################################################################################
################################################################################
# 5. Usando RFE, construir un modelo de regresi�n log�stica m�ltiple para la variable 
# EN que incluya el conjunto, de entre dos y seis, predictores que entregue la mejor 
# curva ROC y que utilice validaci�n cruzada dejando uno fuera para evitar el sobreajuste 
# (obviamente no se debe considerar las variables Peso, Estatura -Weight y Height respectivamente- ni IMC).


# Agregamos variable EN ya que antes se hab�a eliminado

datos_modelo$EN <- as.factor(datos_total$EN) # Arreglar la variable categ�rica, para que no se muestre el warning (aun funciona sin este)


# Particionamos el set de datos
# Particiones: 0.8 entrenamiento, 0.2 evaluaci�n para el modelo RLog
particiones <- createDataPartition(datos_modelo$EN, 
                                   p = 0.8, 
                                   list = FALSE)

entrenamiento <- datos_modelo[particiones, ]
evaluacion_2 <- datos_modelo[-particiones, ]


# Este ser� el dataframe de 'features'
x_entrenamiento <- entrenamiento[, -27] # quitar EN (index 24)
# Eliminamos variables que obviamente predicen bien el EN, ya que fue creada a partir de estos.
x_entrenamiento$IMC <- NULL
x_entrenamiento$Weight <- NULL
x_entrenamiento$Height <- NULL


# Dataframe consistente solo de la var. EN, para introducir a la funci�n RFE:
y_entrenamiento <- entrenamiento$EN # seleccionar solo columna de EN



# Configuramos el objeto de control para funci�n rfe espec�ficamente
# para que utilice regresi�n log�stica: por ello, se utilizan las funciones desde
# lrFuncs (logistic regression functions).


# Agregamos la funci�n que permite utilizar la m�trica ROC (investigado en Google)

lrFuncs$summary <- twoClassSummary
control <- rfeControl(functions = lrFuncs,
                      method = "LOOCV", # Especificar "leave one out" Cross Validation.
                      number = 5, # k = 5 pliegues,
                      verbose = FALSE)


# Aplicamos Recursive Feature Elim. - especificando entre 2 a 6 variables.

set.seed(2231) # Debemos poner la semilla inmediatamente antes - si no hacemos esto siempre se generar� modelo nuevo
feature_select_model_2 <- rfe(x = x_entrenamiento,
                            y = y_entrenamiento,
                            sizes = c(2:6),
                            rfeControl = control,
                            metric = "ROC")

# Vemos que obtenemos warnings - se deben mayoritariamente a los diferentes
# modelos que la funci�n rfe intenta crear al realizar la selecci�n de variables.

# Finalmente, obtenemos los predictores:

print(predictors(feature_select_model_2))

# Ahora se enumeran, ya que son solo dos, no como en el caso anterior donde
# resultaba en 15.

# Waist.Girth y Calf.Maximum.Girth

# Adem�s, haciendo una llamada al objeto feature_select_model_2, notamos que
# efectivamente este modelo entrega la mejor curva ROC (Area Under the Curve/ROC = 0.9231).
print(feature_select_model_2)
# Variables    ROC
# 2          0.9175 - notamos la mejor curva (AUC -> area bajo la curva ROC > m�s cercana a 1).
# 3          0.9144 - Notamos que con 3 o 4 variables tampoco existe mucha diferencia.
# 4          0.9125         
# 5          0.8994 - ac� disminuye bastante   
# 6          0.9081         
# 23         0.8772 - este es el modelo completo que genera el modelo para hacer la eliminaci�n de features

# As�, nos quedamos con las dos variables mencionadas antes, por simplicidad.

################################################################################
################################################################################
# 6. Pronunciarse sobre la confiabilidad y el poder predictivo de los modelos obtenidos.


# Evaluamos el poder predictivo, utilizando los datos de entrenamiento separados en los puntos
# 4 y 5, para estos dos modelos.

# Para el modelo de regresi�n lineal m�ltiple (pto. 4) tenemos:
predict_m1 <- predict(feature_select_model$fit, evaluacion_1)
error_m1 <- evaluacion_1$IMC - predict_m1 # Recordamos que la var. de respuesta era el IMC.
mse_m1 <- mean(error_m1 ** 2)
cat("MSE_mlr = ", mse_m1, "\n\n")

# Vemos que el MSE es de 1.62: un valor bastante bajo, mostrando que tiene un buen poder predictivo,
# de apenas +-1.62 para cada predicci�n.

# Ahora evaluamos el modelo de regresi�n log�stica obtenido con recursive feature elimination:
probs <- predict(feature_select_model_2$fit, 
                 evaluacion_2, 
                 type = "response")

# Seteamos el umbral de decisi�n
umbral <- 0.5

preds_e <- sapply(probs, function(p) ifelse(p >= umbral, "1", "0"))
preds_e <- factor(preds_e, levels = levels(entrenamiento[["EN"]]))
ROC_e <- roc(evaluacion_2[["EN"]], probs)
plot(ROC_e)

# Vemos que la curva se aleja bastante de la l�nea recta, lo que es un indicio favorable.
# Calculemos el AUC (�rea bajo ella) para medir qu� tan buena es la curva num�ricamente.

cat("AUC = ", auc(evaluacion_2[["EN"]], probs), "\n")

