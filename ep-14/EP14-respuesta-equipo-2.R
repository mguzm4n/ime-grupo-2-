library(dplyr)
datos <- read.csv2("EP13 Datos.csv")

set.seed(2231) 

# Como el rut es impar, se toma una muestra de 120 hombres.
tamano_muestra <- 120

# Obetnemos una muestra de todos los hombrees
datos_hombres <- datos %>% filter(Gender == 1)

# Crear variable IMC (Indice de Masa Corporal)
IMC <- datos_hombres$Weight / ( ((datos_hombres$Height)/100) ** 2)
datos_hombres['IMC'] <- IMC


# Dividimos la muestra en 60 hombres sobrepeso y no sobrepeso
# utilizando el umbral de 25 unidades para el IMC

# Hombres considerados en la región de no sobrepeso
muestra1 <- datos_hombres %>% filter(IMC < 25 ) %>% sample_n(60)

# Hombres considerados en la región de sobrepeso
muestra2 <- datos_hombres %>% filter(IMC >= 25 ) %>% sample_n(60)

# Consideramos la muestra total
muestra <- bind_rows(muestra1, muestra2)


# Creamos la variable EN (Estado Nutricional)

# En este caso consideramos:
# 1: Sin sobrepeso
# 0: Sobrepeso
EN <- muestra$IMC < 25

# Técnica para convertir booleanos a valores numéricos (1 y 0)
muestra$EN <- EN * 1


# Seleccionamos 80 personas para la construcción del modelo
# 40 deben tener sobrepeso

datos_modelo_sobrepeso <- muestra[1:40, ]
datos_modelo_nosobrepeso <- muestra[61:100, ]
datos_modelo <- bind_rows(datos_modelo_sobrepeso, datos_modelo_nosobrepeso)

# Seleccionamos 40 personas para el entrenamiento del modelo
# 20 deben tener sobrepeso
datos_entrenamiento_sobrepeso <- muestra[41:60, ]
datos_entrenamiento_nosobrepeso <- muestra[101:120, ]
datos_entrenamiento <- bind_rows(datos_entrenamiento_sobrepeso, datos_entrenamiento_nosobrepeso)

#            Knees.diameter
#        Calf.Maximum.Girth
#               Bicep.Girth
#               Chest.Girth
#                 Hip.Girth
#          Biiliac.diameter
#                Knee.Girth
#       Biacromial.diameter
#               Thigh.Girth


# En este caso, utilizaremos la variable Nave.Girth que representa el grosor a la
# altura del ombligo.

# Usando el entorno R y paquetes estándares1, construir un modelo de regresión logística con el predictor 
# seleccionado en el paso anterior y utilizando de la muestra obtenida.

# Usando herramientas estándares1 para la exploración de modelos del entorno R, buscar entre dos y 
# cinco predictores de entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar 
# al modelo obtenido en el paso 5.

# Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de ajuste y son generalizables) 
# y "arreglarlos" en caso de que tengan algún problema.
# Usando código estándar1, evaluar el poder predictivo de los modelos con los datos de las 0 personas que
#no se incluyeron en su construcción en términos de sensibilidad y especificidad.