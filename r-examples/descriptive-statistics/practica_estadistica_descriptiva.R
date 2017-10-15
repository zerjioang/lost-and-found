# Nombre:       Sergio
# Entrega:      Estadistica descriptica
# Fecha:        14 octubre
#---------------------------------------------------------------------------
# ****************** AVISOS IMPORTANTES ************************************
# 1. Asegúrese de incluir, junto a esta plantilla, cualquier fichero necesario
#    para su ejecución, incluidos datasets
# 2. Si utiliza una función de un determinado paquete, no olvide incluir la
#    correspondiente llamada a la función "library()"
# 3. No olvide comentar el código, en especial aquellos comandos no-triviales
#    (recuerda que parte de la calificación depende de la limpieza del código)
# 4. Todos los procesos (salvo que se indique lo contrario) deberán ser 
#    lo más "automatizables" para cualquier conjunto de datos posible. 
#       Para acceder a la última columna de un dataset de 10 columnas:
#         Opción Automatizada    -> datos[,ncol(datos)]
#         Opción no Automatizada -> datos[,10]
#---------------------------------------------------------------------------
rm(list = ls());cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(ggplot2)
library(GGally)
# (incluya aquí cualquier librería adicional)
#---------------------------------------------------------------------------

# Paso 01: Lea los datos del fichero “crx.csv” y los guarde en un data.frame
dataset = read.csv("crx.csv")

# Paso 02: En este caso, los valores perdidos se denotan como ‘?’ -> Convertirlos todos a NA

#devolver una matriz booleana en funcion de la condicion dataset == '?'
#y a los valores true, asignarles el valor NA
summary(dataset)
dataset[ (dataset == '?') ] = NA
summary(dataset)
# Paso 03: Calcule la matriz de correlación y muestre los 5 pares de atributos más correlacionados
# Sin contar una variable consigo misma, claro. Y sin contar parejas [cor(a,b)==cor(b,a)]
# Tenga en cuenta medir las correlaciones en valor absoluto! [correlación de -0.99 es más fuerte que una de 0.9]
# * Hay varios caminos para hacer ésto, se puede programar con FORs, o hacer uso de algunas
# * funciones de R, como upper.tri(), melt() [paquete reshape2] 
# Mapa de correlación entre cada par de variables. El tamaño de la burbuja revela el significado de cada correlación,
#   mientras que el color representa la dirección (positivo o negativo)

#nota: la correlacion, solo se ejecuta sobre las variables numericas, no categoricas.
numeric_colums = sapply(dataset, is.numeric)
numeric_colums_index = which(numeric_colums)
#hacer un subset del dataset con las numeric columns only
#dataset_numeric_only = dataset %>% select(numeric_colums_index)
dataset_numeric_only = dataset %>% select(A4, A5)
M = cor(dataset_numeric_only)
corrplot(M, method="circle")
cor(dataset_numeric_only)

correlate(dataset)


# Paso 04: Automáticamente, para cada atributo, muestre por pantalla:
#     Si es numérico: la media, mediana, Q1, Q3, desviación estándar, valores mínimo y máximo, número de valores perdidos
#     Si no es numérico: número de valores únicos y porcentaje de apariciones de cada valor único




# Paso 05: Agregue los datos por los valores de los atributos A1 y A4 usando la función media
# * Tenga en cuenta que la agregación sólo se podrá hacer para las columnas numéricas




# Paso 06: Haga uso de la función ggplot [librería ggplot2] y dibuje:
#      El boxplot para cada variable numérica
#      El barplot para cada variable no-numérica
#      El scatterplot (función ggpairs) de todas las variables
#      Usando (en todas) la columna “Class” para definir el color




# Paso 07: Comente cuáles han sido las dificultades encontradas para afrontar esta práctica y cómo las ha resuelto
