# Nombre: Sergio
# Entrega: Particionado y overfitting
# Fecha: 2/11/2017
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
library(caret)
library(ggplot2)
# (incluya aquí cualquier librería adicional)
#---------------------------------------------------------------------------
library(rpart)

# Paso 01: Lea los datos del fichero “churn.csv” y los guarde en un data.frame
dataset = read.csv("churn.csv")

# Paso 02: Elimine aquellas columnas no numéricas que tengan más de 1000 
# niveles diferentes

#nos quedamos con las columnas que si cumplen la condicion
filtered_dataset = dataset[ !sapply(dataset, function(x) !is.numeric(x) && nlevels(x) > 1000) ]

# Paso 03: Divida los datos para hacer 10-fold cross validation, utilizando
# la función apropiada del paquete caret
fold_number = 10

#divide el dataset en 10 folds, con 90% train y 10% test. 
#la siguiente configuracion devuelve el 90% de las filas para entrenar por cada fold
folds = createFolds(filtered_dataset$Churn, k = fold_number, list = TRUE, returnTrain = TRUE)

# Paso 04: Para cada uno de los folds:
#  - Utilice la función “rpart” para construir un árbol de decisión con los datos training
#  - Utilice la función “predict” para probar el árbol construido en el paso anterior sobre los datos de training y test
#  - Almacene en un vector los valores:
#  - minsplit, maxdepth, Porcentaje de fallos promedio (de las 10 ejecuciones) tanto en training y test

calculate_error = function(real, prediction){
  #calculate confusion matrix
  confMat = table(real, prediction)
  #calculate accuracy
  accuracy = sum(diag(confMat))/sum(confMat)
  #return error
  return(1-accuracy)
}

processFold = function(fold, min, maxd){
  print("Procesando FOLD")
  #crear la formula para construir el arbol
  model_formula = as.formula(Churn~.)
  print("Se usará la siguiente formula para generar el modelo")
  print(model_formula)
  
  #construir el modelo usando la formula anterior y todos definidos el fold actual
  training = filtered_dataset[unlist(fold),]
  test = filtered_dataset[-unlist(fold),]
  
  #entrenar el modelo con los datos training del fold actual
  control = rpart.control(minsplit=min, maxdepth=maxd)
  model = rpart(Churn~., method="class", data=training, control = control)
  
  #comprobar el resultado del modelo con los datos del fold
  prediction_training = predict(model, training, type = "class")
  prediction_test = predict(model, test, type = "class")
  
  print("Los datos de la predicción sobre TRAINING son:")
  print(summary(prediction_training))
  print("Los datos de la predicción sobre TEST son:")
  print(summary(prediction_test))
  
  #calcular el error de la predicion
  train_error = calculate_error(training$Churn, prediction_training)
  test_error = calculate_error(test$Churn, prediction_test)
  #guardar los valores actuales en la matriz
  return( c(min, maxd, train_error, test_error) )
}

#comprobar con el primer fold que funciona correctamente
#fold=folds[1]
#processFold(folds[1], 20, 30)

#procesar todos los folds creados
results = sapply(folds, function(x) processFold(x, 20, 30))
print(results)
# se genera la matriz transpuesta
results = t(results)
#le ponemos los nombres a cada columna para saber lo que contiene
colnames(results) = c("min_split", "max_depth", "avg_train_error", "avg_test_error")

#se calcula el promedio de fallos
print("Promedio de fallos en TRAINING:")
print( paste( mean(results[,3])*100, "% ") )

print("Promedio de fallos en TEST:")
print( paste( mean(results[,4])*100, "% ") )

# Paso 05: Construya y pruebe árboles de decisión
# Itere sobre maxdepth = {1...30} y minsplit = {1,10,20,...500}
# Almacene en minsplit, maxdepth, error promedio en training y test
# Obtenga los valores “óptimos” de los parámetros para este problema
# (Hay otros parámetros que se pueden optimizar, pero sólo usaremos estos 2)

# se genera el listado de valores solicitados
maxdepth_vals = c(1:30)
minsplit_vals = seq(0, 500, 10)

# training (80%) y test (20%)
splitted_data = createDataPartition(filtered_dataset$Churn, list = FALSE, p = 0.8, times = 1)
#splitted_data contiene indices a los datos unicamente.
training<-filtered_dataset[splitted_data,]
testing<-filtered_dataset[-splitted_data,]

generateAllPossibles =  function(x, min, depth){
  #crear la formula para construir el arbol
  model_formula = as.formula(Churn~.)

  #entrenar el modelo con los datos training del fold actual
  control = rpart.control(minsplit=min, maxdepth=depth)
  model = rpart(Churn~., method="class", data=training, control=control)
  
  #comprobar el resultado del modelo con los datos del fold
  prediction_test = predict(model, testing, type = "class")
  
  #calcular el error de la predicion
  test_error = calculate_error(testing$Churn, prediction_test)
  #guardar los valores actuales en la matriz
  return( c(depth, min, test_error) )
}

#funcion para iterar sobre los pares de valores depth y minsplit
# length(maxdepth_vals) * length(minsplit_vals) --> 30 * 51 --> 1530 iteraciones
iterateOverMinVal = function(depth, min){
  print( paste("Iteracion actual: depth: ", depth, " minsplit: ", min) )
  return( generateAllPossibles(training, min, depth) )
}

#funcion parar iterar sobre los valores de depth. 30 iteraciones en total
iterateOverMaxdepth = function(depth){
  tmp=sapply(minsplit_vals, function(minval) iterateOverMinVal(depth, minval))
  tmp=t(tmp)
  print(tmp)
  return(tmp)
}
tmp=lapply(maxdepth_vals, iterateOverMaxdepth)
#buscar la mejor configuracion
global_min_val = 1
best_config = 0
plot_data_source = matrix(NA, 0, 4)
for(i in 1:length(tmp)){
  print( paste("Procesando resultado: ", i))
  current=tmp[i]
  current=as.data.frame(current)
  plot_data_source=rbind(plot_data_source, current)
  min_val = min(current[3])
  if(min_val < global_min_val)
    best_config = i
}

if(best_config > 0){
  print( paste("Best configuration is at index: ", best_config) )
  print("Best configuration details:")
  frame = as.data.frame(tmp[best_config])
  colnames(frame) = c("min_split", "max_depth", "error");
  #mostrar solo la parte del resultado que hace minimo el error
  best_idx = which( frame$error == min(frame$error))
  print( paste("Number of optimal configurations: ", length(best_idx) ) )
  print(frame[best_idx,])
} else {
  print("No optimal configuration found")
}

#
# Paso 06: Utilice la librería ggplot2 para hacer para hacer un gráfico de "azulejos":
#  - Eje X -> Valor de maxdepth
#  - Eje Y -> Valor de minsplit
#  - Color -> Porcentaje de error (promedio) en test
# * Puede utilizar geom_tile() o geom_raster()

#(hjust = 0, vjust = 0) es para ajustar la grafica al centro
ggplot(
  plot_data_source, 
  aes(plot_data_source$X1, plot_data_source$X2, fill = plot_data_source$X3)
) + geom_raster(hjust = 0, vjust = 0) + ggtitle("Grafico de azulejos")+ xlab("Depth") + ylab("Min_split") + labs(fill="Error rate")

# Paso 07: Comente cuáles han sido las dificultades encontradas para afrontar esta práctica y cómo las ha resuelto

#El mayor problema que he tenido al hacer esta entrega ha sido el de encontrar el resultado optimo para el arbol
# dado que R me devolvia los datos en formato de lista y yo necesitaba un data.frame para encontrar el error minimo

# Al final lo he resuelto iterando sobre la lista que se devolvia como resultado y contruyendo un data.frame por cada elemento de dentro
