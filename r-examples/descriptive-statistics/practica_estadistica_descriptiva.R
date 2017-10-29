# Nombre:       Sergio
# Entrega:      Estadistica descriptica
# Fecha:        17 octubre
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

#conocer el tipo de dato de cada columna.
#el que R detecta es:
sapply(dataset, class)

#autoasignar el tipo de dato correcto a cada columna.
#las columnas A2 Y A 14 son numericas pero no son detectadas como tal.

#detectar las columnas que son factores
bool_columnas_factor = sapply(dataset, is.factor)

#obtener las columnas numericas, es decir, todas las que no son factores
real_numeric_columns = dataset[!bool_columnas_factor]

#1 Convertir a numericas todas las columnas posibles. Las que no sean numericas, se ponen a NA automaticamente
real_numeric_but_factor = apply(dataset[bool_columnas_factor], apply_to_columns, function(x) as.numeric(as.character(x)))

#obtener los ids de las columnas que sean numericas y no sean NA
numeric_cols_index = which(sapply(as.data.frame(real_numeric_but_factor), function(x) is.numeric(x) && !is.na(x) ))

#eliminar columnas que sean NAs
#se compara el numero total de NA en la columna y se compara con el numero total de elementos en la columna. si es igual, se borra a columna
real_numeric_but_factor = real_numeric_but_factor[, colSums(is.na(real_numeric_but_factor)) != nrow(real_numeric_but_factor)]
#convertir el resultado de matrix a data.frame
real_numeric_but_factor = as.data.frame(real_numeric_but_factor)

#juntar todas las columnas numericas en un mismo dataset
all_numeric_columns = data.frame(real_numeric_but_factor, real_numeric_columns)

#se separan las columnas categoricas. es decir, al dataset original se le eliminan las columnas numericas detectadas

#obtener el nombre de las columnas numericas
numeric_columns_names = colnames(all_numeric_columns)
#se ponen a TRUE a borrar las columnas que coincidan en nombre con las numericas detectadas.
condicion_borrado_columnas= colnames(dataset) %in% numeric_columns_names
#se separa en un data.frame diferente
all_categorical_columns = dataset[, -which(condicion_borrado_columnas)]

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

#calcular la correlacion entre las variables numericas solamente
correlation_data = cor(na.omit(all_numeric_columns))

#mostrar resultados de la correlacion
print("Matriz de correlación")
correlation_data

#mostrar la grafica de correlacion
#order="hclust" nos miuestra la matriz ordenada para detectar posibles valores ocultos
corrplot(correlation_data, type="lower", method="circle" , order="hclust")

#Pasos para mostrar el top 5 de atributos mas correlacionados

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

print("Top 5 correlated variables")
top_correlated

# Paso 04: Automáticamente, para cada atributo, muestre por pantalla:
#     Si es numérico: la media, mediana, Q1, Q3, desviación estándar, valores mínimo y máximo, número de valores perdidos
#     Si no es numérico: número de valores únicos y porcentaje de apariciones de cada valor único

#mostrar datos de los atributos.
showNumericalData = function(columnData) {
  fixedData = na.omit(columnData)
  print(
    paste(
      "Media:   ",
      mean(fixedData),
      "Mediana:   ",
      median(fixedData),
      "Q1:    ",
      quantile(fixedData, 0.25),
      "Q3:    ", 
      quantile(fixedData, 0.75),
      "sd:    ",
      sd(fixedData),
      "min:   ",
      min(fixedData),
      "max:   ",
      max(fixedData),
      "Valores perdidos:    ",
      sum(
        is.na(columnData)
      )
    )
  )
}

#procesar las columnas numericas
temp = lapply(all_numeric_columns, showNumericalData)
temp = NULL

showCategoricalData = function(columnDataAsFactor){
  #convertir d factor a matrix, para poder procesarlo bien
  columnData = as.matrix(columnDataAsFactor)
  print("Numero de valores unicos")
  table = table(columnData)
  print(table)
  total = nrow(columnData)
  print("Porcentaje de apariciones")
  perc = table/total*100
  print(perc)
}

#procesar las columnas categoricas
temp = sapply(all_categorical_columns, showCategoricalData)
temp =NULL

# Paso 05: Agregue los datos por los valores de los atributos A1 y A4 usando la función media
# * Tenga en cuenta que la agregación sólo se podrá hacer para las columnas numéricas

#se seleccionan los atributos especificados
selected_columns = list(all_categorical_columns$A1, all_categorical_columns$A4)

#se agregan
mean = aggregate( x = all_numeric_columns, by = selected_columns, FUN = "mean")

print("Agregacion de datos en funcion de A1 y A4")
print(mean)
# Paso 06: Haga uso de la función ggplot [librería ggplot2] y dibuje:
#      El boxplot para cada variable numérica
#      El barplot para cada variable no-numérica
#      El scatterplot (función ggpairs) de todas las variables
#      Usando (en todas) la columna “Class” para definir el color

color = as.matrix(all_categorical_columns$Class)
colordef = ifelse(color == "positive", "red", "darkblue")

generateBoxplots = function(x){
  boxplot(
    x, 
    data=all_numeric_columns,
    main="Boxplot variable numerica", 
    xlab="Variable",
    ylab="Descripción",
    col=colordef
  )
}
temp = apply(all_numeric_columns, apply_to_columns, generateBoxplots)

color = as.matrix(all_categorical_columns$Class)
colordef = ifelse(color == "positive", "red", "darkblue")

#se procesa cada columna de forma individual y se crea el grafico
createMultipleBarPlots = function(x){
  #data = dataset_no_numeric_only[name]
  x = table(x)
  barplot(
    x,
    main="Barplot variables categoricas",
    xlab="name",
    col=colordef
  )
  
}
temp=apply(all_categorical_columns, apply_to_columns, createMultipleBarPlots)

#crear scatterplot

#definir la formula en funcion de las columnas que queremos usar
#como queremos usar todas se pone ~.
formula = as.formula(dataset$Class ~.)

#definir la columna que decide que color pintar
color = as.matrix(dataset$Class)
#crear la lista de colores en funcion de la columna anterior
colordef = ifelse(color == "positive", "red", "darkblue")

'''
ggpairs(
  data=dataset,
  columns=1:ncol(dataset),
  title="ggpairs scatterplot",
  colour = "sex",
  cardinality_threshold = NULL
)
'''

#generar el grafico scatterplot con las 16 variables
pairs(
  formula,
  data=dataset, 
  main="Scatterplot",
  col=colordef
)

# Paso 07: Comente cuáles han sido las dificultades encontradas para afrontar esta práctica y cómo las ha resuelto

#Pues no se por donde empezar porque ha sido la practica que mas dificultades he tenido. En primer lugar, ha sido el hecho
#de tener que hacer experimentos para poder convertir las columnas que R detectaba como factor cuando realmente eran numericas, pero double.

#al final ya has visto la solucion que he elegido. antes de decidirme por esto, habia probado a leer el csv directamente diciendole que 
#no me convirtiera en factores las columnas con strings, pero no consegui avanzar por ese lado. asi que he optado por hacer lo que has visto

# el segundo de los problemas ha sido que las graficas que he intentado crear con ggally tardaban muchisimo en generarse. Se generan si, pero tardan una eternidad
#asi que te dejo adjuntadas las dos opciones, el scatterplot con ggpairs y con pairs. 
