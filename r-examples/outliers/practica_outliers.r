# Nombre:       Sergio
# Entrega:      Outliers
# Fecha:        13 octubre
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
rm(list = ls())
cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(DMwR)
library(ggplot2)
library(GGally)
#---------------------------------------------------------------------------
apply_to_rows = 1
apply_to_columns = 2

# Paso 01: Lea los datos del fichero “ozone.cvs” y los guarde en un data.frame
dataset = read.csv("ozone.csv")

# Paso 02: Elimine aquellas columnas que tengan más de un 10% de sus valores NA
# * Para recorrer las columnas, puede usar una función de la familia apply, 
# * una estructura for o un filtro que te marque las columnas a eliminar
threshold_value = 0.1 
#condicion de borrado de la columna. resultado boolean
columnas_a_borrar = colMeans(is.na(dataset)) >= threshold_value
#traducir de TRUE, FALSE, al indice de columna a borrar
columnas_idx_a_borrar = which(columnas_a_borrar)
filtered_dataset = dataset[, -columnas_idx_a_borrar]

# Paso 03: Elimine aquellas filas que tengan algún valor perdido
# * La opción más inmediata es hacer uso de la función complete.cases()
filtered_dataset = na.omit(filtered_dataset)

#recalcular las filas totales y columas
total_columnas_filtered = ncol(filtered_dataset)
total_filas_filtered = nrow(filtered_dataset)

# Paso 04: Cree una columna “outliermedia” (booleana) = TRUE cuando uno 
# de los valores de la fila está fuera del rango 
# (media+-3*desviacióntipica, para su columna) 
# * Ojo, necesitará calcular las medias y desviaciones de cada columna

# se calcula la media y la SD de cada columna una sola vez
temp = matrix(0, 3, total_columnas_filtered)
temp[2,]=apply(filtered_dataset, apply_to_columns, mean)
temp[3,]=apply(filtered_dataset, apply_to_columns, sd)

evaluateRow <- function(x) {
  x1 = x[1]
  media = x[2]
  sd = x[3]
  condicion_valida = x1 >= (media-3*sd) && x1 <= (media+3*sd)
  return(condicion_valida)
}

outliermedia <- function(x) {
  temp[1,] = x
  #evaluar todas las columnas de la fila actual aplicando la funcion evaluateRow
  #result contiene un array de TRUE o FALSE
  result = apply(temp, apply_to_columns, function(x) evaluateRow(x))
  return(
    length(which(result==FALSE)) > 0
  )
}

# Paso 05: Cree una columna “outliermediana” (booleana) = TRUE cuando uno 
# de los valores de la fila está fuera del rango 
# (mediana+-1.5*(Q3-Q1)), para su columna) 
# * Ojo, necesitará calcular las medianas, Q1 y Q3 de cada columna

# se calcula la mediana, q1 y q3 de cada columna una sola vez
temp2 = matrix(0, 4, total_columnas_filtered)
temp2[2,]=apply(filtered_dataset, apply_to_columns, median) #mediana de todas las columnas
temp2[3,]=apply(filtered_dataset, apply_to_columns, function(col) quantile(col, 0.25) ) #q1
temp2[4,]=apply(filtered_dataset, apply_to_columns, function(col) quantile(col, 0.75) ) #q3

evaluateRow2 <- function(x) {
  x1 = x[1]
  median = x[2]
  q1 = x[3]
  q3 = x[4]
  condicion_valida = x1 >= (median-1.5*(q3-q1)) && x1 <= (median+1.5*(q3-q1))
  return(condicion_valida)
}
outliermediana <- function(x) {
  temp2[1,] = x
  #evaluar todas las columnas de la fila actual aplicando la funcion evaluateRow
  #result contiene un array de TRUE o FALSE
  result = apply(temp2, apply_to_columns, function(x) evaluateRow2(x))
  return(
    length(which(result==FALSE)) > 0
  )
}

#aplicar transformaciones
outliermediaResult = apply(filtered_dataset, apply_to_rows, function(row) outliermedia(row))
outliermedianaResult = apply(filtered_dataset, apply_to_rows, function(row) outliermediana(row))

#juntar
filtered_dataset$outlier_media = outliermediaResult
filtered_dataset$outlier_mediana = outliermedianaResult

# Paso 06: Utilice la función lofactor (paquete: DMwR) con k=5
# y establezca el umbral que considere apropiado, razone por qué.
# (Lea la documentación y) Responda a lo siguiente: ¿Qué pasaría si k=1?, ¿y si k=100?

#k representa el numero de vecinos que se usaran para calcular si el dato observado, es un outlier.
# si se escoge un valor de k muy bajo, como 1, solo se mirara el vecino mas cercano, por lo que la precision del 
# modelo se ve reducida y el modelo seguiria teniendo outliers. En el caso contrario, con un valor muy alto como 100,
# el modelo, compara cada dato observado con sus 100 vecinos mas cercanos, que para este dataset, supone compararlo con 1/3 del dataset
# Lo que tampoco serviria para reducir o eliminar los outliers, por el hecho de calcular la densidad con demasiados elementos
# A la hora de calcular clusters, una de tantas medidas para predefinir cuantos grupos crear dado un dataset, es sqrt(length(dataset)), e decir
# la raiz cuadrada del numero total de elementos disponibles, lo que en este caso seria, k=18

k_value = 5
scores = lofactor(filtered_dataset, k_value)
lofactor_threshold = mean(scores)+3*sd(scores) #our threshold value. 99% confidence
plot(density(scores))

# Paso 07: Cree una columna "outlierlofactor" (booleana) = TRUE cuando
# el valor devuelto por la función lofactor es mayor que el umbral fijado

filtered_dataset$outlier_lofactor = ifelse( scores > lofactor_threshold, "TRUE", "FALSE")

# Paso 08: Cree 3 gráficos utilizando la función ggpairs (paquete GGally) sobre los datos
# Cada uno de ellos utilizando una de las columnas "outlierXXX" para darle color

#pintar todas las columnas salvo las boolenas
numero_col_bool = 3
ultima_col_pintable = (ncol(filtered_dataset)-numero_col_bool)

pm <- ggpairs(
  filtered_dataset,
  mapping = aes(color = outlier_lofactor),
  columns=1:ultima_col_pintable,
  title = "GG PAIRS: Outlier lofactor"
)
pm

filtered_dataset$outlier_medianaColor = ifelse( filtered_dataset$outlier_mediana, "TRUE", "FALSE")

pm2 <- ggpairs(
  filtered_dataset,
  mapping = aes(color = outlier_medianaColor),
  columns=1:ultima_col_pintable,
  title = "GG PAIRS: Outlier mediana"
)
pm2

filtered_dataset$outlier_mediaColor = ifelse( filtered_dataset$outlier_media, "TRUE", "FALSE")

pm3 <- ggpairs(
  filtered_dataset,
  mapping = aes(color = outlier_mediaColor),
  columns=1:ultima_col_pintable,
  title = "GG PAIRS: Outlier media"
)
pm3
# Paso 09: Comente cuáles han sido las dificultades encontradas para afrontar esta práctica y cómo las ha resuelto

# La mayor dificultad que he tenido ha sido a la hora de buscar una forma de iterar
# sobre las filas que a suvez tenian que iterar sobre las columnas para calcular los valores pedidos
