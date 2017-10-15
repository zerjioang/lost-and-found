# Nombre:       Sergio
# Entrega:      Arboles decision
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
# (incluya aquí cualquier librería adicional)

# Classification Tree with rpart
library(caret)
library(rpart)
library(rpart.plot)

#---------------------------------------------------------------------------
apply_to_rows = 1
apply_to_columns = 2

# Paso 01: Lea los datos del fichero “churn.csv” y los guarde en un data.frame  [hecho]
original_dataset = read.csv("churn.csv")
dataset = original_dataset
# Paso 02: Elimine aquellas columnas no numéricas que tengan más de 1000 
# niveles diferentes

# saber que columnas no son numericas y cuales si
numeric_colums = sapply(dataset, is.numeric)
#obtener la longitud del vector de valores unicos
unique_data = sapply(dataset, function(x) length(unique(x)))
threshold_value = 1000; 
#condicion de borrado de las columnas
erase_columns = (unique_data >= threshold_value) & !numeric_colums
#traducir de TRUE, FALSE, al indice de columna a borrar
colum_index_to_be_deleted = which(erase_columns)
#eliminar las columnas del dataset original
filtered_dataset = dataset[, -colum_index_to_be_deleted]

# Paso 03: Utilice la función createDataPartition (paquete: caret) para dividir los datos en
# training (80%) y test (20%)
splitted_data = createDataPartition(filtered_dataset$Churn, list = FALSE, p = 0.8, times = 1)
#splitted_data contiene indices a los datos unicamente.
training<-filtered_dataset[splitted_data,]
testing<-filtered_dataset[-splitted_data,]

#plot(density(filtered_dataset$Churn[splitted_data]))

# (Lea la documentación y) Responda a lo siguiente: ¿createDataPartition particiona los datos
# de manera estratificada o no?

#Por lo que he podido leer, haciendo uso de la funcion createDataPartition, solo se seleccionan elementos
#de dicho dataset, de forma aleatoria, sin tener en cuenta el resto de variables ni la proporcion de tipo
#de datos, por lo que a priori, los datos no se particionan de forma estratificada

# Paso 04: Utilice la función rpart() para construir un modelo de árbol con los datos de training
# y la función predict() para probar el árbol sobre los datos de test
# --> modelo = rpart(...datostra...)
# --> prediccion = predict(...modelo,...datostst,...)
#eliminar la ultima columna que representa la clase a precedir para 
#obtener los parametros de la formula de forma dinamica
#formula con todas las columnas excepto clase, que tiene que colocarse como primera columna

#mover la columna tipo class como primera columna
formula_data = filtered_dataset
last = ncol(formula_data)
formula_data = formula_data[,c(last,1:(last-1))]
formula = as.formula(formula_data)

#modelo entrenado con datos de training
#anova = regression
#class = classification
rpart_model = rpart(formula, method="class", data=training)
#testeo con datos de testing
prediction = predict(rpart_model, testing, type = "class")

# Paso 05: Utilice la función rpart.plot() para dibujar y la función summary() para ver un resúmen del árbol obtenido
rpart.plot(rpart_model)
summary(rpart_model)
#plotcp(rpart_model)
post(rpart_model, file = "churn_tree.pdf", 
     title = "Classification Tree for Churn")

#calculate confusion matrix
confMat = table(testing$Churn, prediction)
#calculate accuracy
accuracy = sum(diag(confMat))/sum(confMat)
print("Accuracy: ")
accuracy

# Paso 06: (Lea la documentación y) explique con sus palabras el significado de los parámetros: minsplit, maxdepth y cp

#minsplit:
#maxdepth: define el numero maximo de niveles en profundidad que el arbol de decision va a tener.
#          llegado a ese limite se pararia la construccion del arbol
#cp:      basicamente representa el nivel de complejidad y es usado como parametro de parada. Ayuda a mejorar 
#         las busquedas de 'splits' y se usa tambien para hacer 'prune' de aquellos 'splits'
#         que no cumplen con el 'criteria'

# Paso 07: Pruebe 3 configuraciones diferentes de los parámetros minsplit, maxdepth y cp de la función rpart() y 
# repita los pasos 4 y 5. Comente los resultados.

values_cp = c(0.01, 0.01, 0.01)
values_minsplit = c(15, 25, 30)
values_max_depth = c (5, 10, 20)

for(i in 1:3){
  rpart_model = rpart(
    formula,
    method="class",
    data=training,
    minsplit = values_minsplit[i],
    cp = values_cp[i],
    maxdepth = values_max_depth[i]
  )
  prediction = predict(rpart_model, testing, type = "class")
  #calculate confusion matrix
  confMat = table(testing$Churn, prediction)
  #calculate accuracy
  accuracy = sum(diag(confMat))/sum(confMat)
  print("Accuracy: ")
  accuracy
}

# Paso 08: Comente cuáles han sido las dificultades encontradas para afrontar esta práctica y cómo las ha resuelto

# la mayo dificultad que he tenido ha sido a la hora de limpiar el dataset con la condicion establecida de
#coluumnas no numericas mayores de 1000 elementos unicos. Esa en cuanto al limpiado de datos,
# Ademsa, por otro lado, tambien me ha costado el hecho de llegar a la conclusio, que a la hora de generar la formula para rpart,
# la columna de tipo clase, es decir, la cual se va a predecir, tenia que estar en primer lugar.
# de lo contrario no se generaba correctamente el arbol de decision.