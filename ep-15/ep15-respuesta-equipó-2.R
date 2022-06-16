library(dplyr)
library(caret)
library(leaps)

datos <- read.csv2("EP13 Datos.csv")


# 1.|
set.seed(2231) 

# Como el rut es impar, se toma una muestra de 120 hombres.
tamano_muestra <- 100

# Crear variable IMC (Indice de Masa Corporal)
IMC <- datos$Weight / ( ((datos$Height)/100) ** 2)
datos['IMC'] <- IMC

# En este caso consideramos:
# 1: Sin sobrepeso
# 0: Sobrepeso
EN <- datos$IMC < 25

# Técnica para convertir booleanos a valores numéricos (1 y 0)
datos$EN <- EN * 1

muestra1 <- datos %>% filter(IMC < 25 ) %>% sample_n(50)
muestra2 <- datos %>% filter(IMC >= 25 ) %>% sample_n(50)

datos_total <- bind_rows(muestra1, muestra2)

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
# seleccionamos las variables
# Shoulder.Girth, Waist.Girth, Hip.Girth
# Calf.Maximum.Girth, Age y Height

formula <- Weight ~ Shoulder.Girth + Waist.Girth + Hip.Girth + Calf.Maximum.Girth + Age + Height

control <- trainControl(method = "boot",
                        number = 1000)

evaluacion <- train(formula, 
                    data = datos_modelo,
                    trControl = control,
                    method = "lm")

# Vemos que efectivamente la función train efectúa el método
# de remuestreo Bootstrapping utilizando 1000 repeticiones.
# En este caso, obtenemos un RMSE de 2.62, mostrando que es un error
# bastante bajo en relación a los ejercicios prácticos anteriores.

