# Nombre:       Sergio
# Entrega:      Estadistica descriptica
# Fecha:        17 octubre
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
library(reshape)
library(corrplot)
apply_to_rows = 1
apply_to_columns = 2

# Paso 01: Lea los datos del fichero “crx.csv” y los guarde en un data.frame
dataset = read.csv("crx.csv")

# Paso 02: En este caso, los valores perdidos se denotan como ‘?’ -> Convertirlos todos a NA

#devolver una matriz booleana en funcion de la condicion dataset == '?'
#y a los valores true, asignarles el valor NA
dataset[ (dataset == '?') ] = NA
#dataset = na.omit(dataset)
summary(dataset)

# Paso 03: Calcule la matriz de correlación y muestre los 5 pares de atributos más correlacionados
# Sin contar una variable consigo misma, claro. Y sin contar parejas [cor(a,b)==cor(b,a)]
# Tenga en cuenta medir las correlaciones en valor absoluto! [correlación de -0.99 es más fuerte que una de 0.9]
# * Hay varios caminos para hacer ésto, se puede programar con FORs, o hacer uso de algunas
# * funciones de R, como upper.tri(), melt() [paquete reshape2] 
# Mapa de correlación entre cada par de variables. El tamaño de la burbuja revela el significado de cada correlación,
#   mientras que el color representa la dirección (positivo o negativo)

# nota: la correlacion, solo se ejecuta sobre las variables numericas, no categoricas. Se podria usar la funcion correlate(dataset)
# del paquete lsr pero la he descartado porque devuelve una lista de 5 elementos que es as dificil de procesar en los siguientes 
# pasos que una matriz

max_variables = 5

# #detecta columnas numericas. ojo: enteras y con decimales. para ello convierte las columnas al dato correcto
# convert_columns <- function(column){
#   #intentamos procesar la columna como double
#   column_data_as_double = as.double(as.character(column))
#   na_count = sum(is.na(column_data_as_double))
#   if(na_count > 0){
#     #la columna no es double. no se hace nada
#     return(column)
#   }
#   else{
#     return(column_data_as_double)
#   }
# }
# 
# #todo: automatizarlo
# dataset = apply(dataset, apply_to_columns, convert_columns)

numeric_colums = sapply(dataset, is.numeric)
no_numeric_colums = sapply(dataset, function(x)!is.numeric(x))
numeric_colums_index = which(numeric_colums)
no_numeric_colums_index = which(no_numeric_colums)

#hacer un subset del dataset con las numeric columns only
dataset_numeric_only = dataset[numeric_colums_index]
#convertir a numeros
dataset_numeric_only = sapply(dataset_numeric_only, function(x) as.numeric(as.character(x)))
dataset_no_numeric_only = dataset[no_numeric_colums_index]

#calcular la correlacion entre las variables numericas solamente
correlation_data = cor(dataset_numeric_only)

#mostrar resultados de la correlacion
correlation_data

#mostrar la grafica de correlacion
#order="hclust" nos miuestra la matriz ordenada para detectar posibles valores ocultos
corrplot(correlation_data, type="lower", method="circle" , order="hclust")

#usar reshape para convertir la matriz en una lista
#pero antes eliminar las correlaciones de variables consigo mismas
correlation_data[correlation_data == 1] = NA
#eliminar correlacion (a==b), (b==a)
items_a_borrar = upper.tri(correlation_data, TRUE)
correlation_data[ items_a_borrar ] = NA
#eliminar los NA generados en el anterior paso
correlation_data = na.omit(melt(correlation_data))
#convertir a valores absolutos
correlation_data$value = abs(correlation_data$value)
#ordenar de mayor a menor con la variable value (el menos hace que sea de mayor a menor)
correlation_data = correlation_data[order(-correlation_data$value),]

correlation_data

top_correlated = correlation_data[1:max_variables,]

print("Top correlated variables")
top_correlated

# Paso 04: Automáticamente, para cada atributo, muestre por pantalla:
#     Si es numérico: la media, mediana, Q1, Q3, desviación estándar, valores mínimo y máximo, número de valores perdidos
#     Si no es numérico: número de valores únicos y porcentaje de apariciones de cada valor único

#mostrar datos de los atributos.
showAttributeData = function(columnData) {
  if(is.numeric(columnData)){
    #columna de datos numericos
    print(
      paste(
        "Media: ",
        mean(columnData),
        "Mediana: ",
        median(columnData),
        "Q1: ",
        quantile(columnData, 0.25),
        "Q3: ", 
        quantile(columnData, 0.75),
        "sd: ",
        sd(columnData),
        "min: ",
        min(columnData),
        "max: ",
        max(columnData),
        "Valores perdidos: ",
        sum(
          is.na(columnData)
        )
      )
    )
  }
  else{
    #columna de datos numericos
    print (
      paste(
        "Numero de valores numericos: ",
        length(
          unique(columnData)
        )
      )
    )
  }
}

#procesar las columnas numericas
apply(dataset_numeric_only, apply_to_columns, showAttributeData)
#procesar las columnas no numericas
apply(dataset_no_numeric_only, apply_to_columns, showAttributeData)


# Paso 05: Agregue los datos por los valores de los atributos A1 y A4 usando la función media
# * Tenga en cuenta que la agregación sólo se podrá hacer para las columnas numéricas




# Paso 06: Haga uso de la función ggplot [librería ggplot2] y dibuje:
#      El boxplot para cada variable numérica
#      El barplot para cada variable no-numérica
#      El scatterplot (función ggpairs) de todas las variables
#      Usando (en todas) la columna “Class” para definir el color




# Paso 07: Comente cuáles han sido las dificultades encontradas para afrontar esta práctica y cómo las ha resuelto
