# Nombre: Enrique Onieva Caracuel
# Documento: Primera toma de contacto con R y Rstudio


# R es un lenguaje de scripting donde podemos ejecutar el código línea a línea
# Para avanzar por el código basta con ponernos en la línea (o marcar el pedazo)
# de código a ejecutar y pulsar Control+Enter
# (También podemos usar el botón arriba del editor <<Run>>)
#---------------------------------------------------------------------------
## Preparación del entorno de trabajo

# Con los siguientes dos comandos limpiamos el entorno de trabajo y la consola
rm(list = ls())
cat("\014")

# Otro punto es establecer el working directory, con este comando hacemos que 
# éste sea igual a aquél en el que se encuentra el script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Este comando puede dar error si hay acentos u otros carácteres "extraños" en 
# la ruta. Si es así, puedes escribir directamente la ruta entrecomillada y 
# cuidado con la dirección de las barras
# setwd("c:/Users/Adm...")
# O puede usar la opción del menú "Sessions->Set Working Directory->To Source File Location"

# Comprobamos que está correcto
getwd()
#---------------------------------------------------------------------------

#READ CSV
datos = read.csv("ozone.csv")

# En R, no hay que reservar memoria para las variables, ni "preocuparnos" por sus tipos
a = 2
b = "dos"
a = "tres"
b = 3.4

# Si miráis código por internet, podéis encontrar la asignación así: "a <- 2" (es similar)

# Para declarar vectores, basta con asignarlos, y basta llamar a una variable para mostrar su 
# contenido por la consola
vector = c(1,2,3,4,5,6)
vector = c("uno","dos","tres")

# Existen multitud de comandos para crear e inicializar vectores (y matrices)
vector = 1:10
vector = seq(1,10,2)
vector = seq(10,1,-2)
vector = runif(10,50,100)
vector = rep(c(1,2),10)

matriz = matrix(0,nrow=10,ncol=3)
matriz

# Accedemos a las componentes con corchetes
vector[2]
matriz[1,3]

# Para las matrices, podemos acceder a toda una fila/columna
matriz[1,]
matriz[,1]

# Y hacer operaciones múltiples
matriz[1,] = 10
matriz

matriz[,2] = matriz[,2]+10
matriz

vector = vector^2# Nombre: Enrique Onieva Caracuel
# Documento: Primera toma de contacto con R y Rstudio


# R es un lenguaje de scripting donde podemos ejecutar el código línea a línea
# Para avanzar por el código basta con ponernos en la línea (o marcar el pedazo)
# de código a ejecutar y pulsar Control+Enter
# (También podemos usar el botón arriba del editor <<Run>>)
#---------------------------------------------------------------------------
## Preparación del entorno de trabajo

# Con los siguientes dos comandos limpiamos el entorno de trabajo y la consola
rm(list = ls())
cat("\014")

# Otro punto es establecer el working directory, con este comando hacemos que 
# éste sea igual a aquél en el que se encuentra el script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Este comando puede dar error si hay acentos u otros carácteres "extraños" en 
# la ruta. Si es así, puedes escribir directamente la ruta entrecomillada y 
# cuidado con la dirección de las barras
# setwd("c:/Users/Adm...")
# O puede usar la opción del menú "Sessions->Set Working Directory->To Source File Location"

# Comprobamos que está correcto
getwd()
#---------------------------------------------------------------------------

# En R, no hay que reservar memoria para las variables, ni "preocuparnos" por sus tipos
a = 2
b = "dos"
a = "tres"
b = 3.4

# Si miráis código por internet, podéis encontrar la asignación así: "a <- 2" (es similar)

# Para declarar vectores, basta con asignarlos, y basta llamar a una variable para mostrar su 
# contenido por la consola
vector = c(1,2,3,4,5,6)
vector

vector = c("uno","dos","tres")

# Existen multitud de comandos para crear e inicializar vectores (y matrices)
vector = 1:10
vector = seq(1,10,2)
vector = seq(10,1,-2)
vector = runif(10,50,100)
vector = rep(c(1,2),10)

matriz = matrix(0,nrow=10,ncol=3)
matriz

# Accedemos a las componentes con corchetes
vector[2]
matriz[1,3]

# Para las matrices, podemos acceder a toda una fila/columna
matriz[1,]
matriz[,1]

# Y hacer operaciones múltiples
matriz[1,] = 10
matriz

matriz[,2] = matriz[,2]+10
matriz

vector = vector^2
vector

# Se pueden utilizar estructuras FOR (aunque no son recomendables, porque son más ineficientes)
for (i in 1:nrow(matriz)){
  matriz[i,3] = matriz[i,3]+1
}
matriz

# Otra opción, elegante, eficiente, y necesaria en algunos casos es usar funciones de la familia apply
# Podemos definir nuestra propia función
prueba = function(x){
  valor = x[2]^2-x[3]
  return(valor)
}
matriz[,1] = apply(matriz,1,function(x) prueba(x))
matriz

# Aunque la estructura "más potente" es el data.frame, que permite tener columnas
# de diferentes tipos
datos = read.csv("basketball.csv")

# En ella, podemos acceder a columnas por índice o nombre
datos[,1]
datos$assists_per_minute

# Y crear una nueva columna que calculemos simplemente dándole un nombre
datos$masde2metros = datos$height>190
datos$masde2metros

# Y hacer operaciones más complejas
datos$alto = ifelse(datos$height>190,"Alto","Bajo")




# La librería ggplot2 es la más utilizada por científicos de datos para hacer gráficas
# Ojo: Deberéis instalar el paquete (R tiene ya más de 10.000 paquetes distintos)
# Podéis usar el comando install.packages("ggplot2"), o descargarlo desde la interfaz de RStudio
library(ggplot2)
ggplot(datos,aes(x=assists_per_minute,y=time_played))+geom_point()

ggplot(datos,aes(x=assists_per_minute,y=time_played,col=masde2metros))+
  geom_point()

# Y hacerlo todo lo complejo que queráis
ggplot(datos,aes(x=assists_per_minute,y=time_played,col=masde2metros))+
  geom_point()+
  labs(x = "Asistencias por Minuto (min)", 
       y="Tiempo de Juego (%)", 
       colour = "Alto?",
       title = "Jugadores de baloncesto",
       subtitle = "(Asistencias/Tiempo de juego)",
       caption = "Universidad de deusto")


# Y hacerlo todo lo complejo que queráis
ggplot(datos,aes(x=height,y=as.numeric(masde2metros),col=as.numeric(masde2metros)))+
  geom_point()+
  labs(x = "Altura (cm)", 
       y="Pertenencia al conjunto <Alto>", 
       colour = "Alto?",
       title = "Jugadores de baloncesto",
       subtitle = "(Grado de pertenencia al conjunto <Alto> de una manera no difusa)",
       caption = "Universidad de deusto")
vector

# Se pueden utilizar estructuras FOR (aunque no son recomendables, porque son más ineficientes)
for (i in 1:nrow(matriz)){
  matriz[i,3] = matriz[i,3]+1
}
matriz

# Otra opción, elegante, eficiente, y necesaria en algunos casos es usar funciones de la familia apply
# Podemos definir nuestra propia función
prueba = function(x){
  valor = x[2]^2-x[3]
  return(valor)
}
matriz[,1] = apply(matriz,1,function(x) prueba(x))
matriz

# Aunque la estructura "más potente" es el data.frame, que permite tener columnas
# de diferentes tipos
datos = read.csv("basketball.csv")

# En ella, podemos acceder a columnas por índice o nombre
datos[,1]
datos$assists_per_minute

# Y crear una nueva columna que calculemos simplemente dándole un nombre
datos$masde2metros = datos$height>190
datos$masde2metros

# Y hacer operaciones más complejas
datos$alto = ifelse(datos$height>190,"Alto","Bajo")




# La librería ggplot2 es la más utilizada por científicos de datos para hacer gráficas
# Ojo: Deberéis instalar el paquete (R tiene ya más de 10.000 paquetes distintos)
# Podéis usar el comando install.packages("ggplot2"), o descargarlo desde la interfaz de RStudio
library(ggplot2)
ggplot(datos,aes(x=assists_per_minute,y=time_played))+geom_point()

ggplot(datos,aes(x=assists_per_minute,y=time_played,col=masde2metros))+
  geom_point()

# Y hacerlo todo lo complejo que queráis
ggplot(datos,aes(x=assists_per_minute,y=time_played,col=masde2metros))+
  geom_point()+
  labs(x = "Asistencias por Minuto (min)", 
       y="Tiempo de Juego (%)", 
       colour = "Alto?",
       title = "Jugadores de baloncesto",
       subtitle = "(Asistencias/Tiempo de juego)",
       caption = "Universidad de deusto")


# Y hacerlo todo lo complejo que queráis
ggplot(datos,aes(x=height,y=as.numeric(masde2metros),col=as.numeric(masde2metros)))+
  geom_point()+
  labs(x = "Altura (cm)", 
       y="Pertenencia al conjunto <Alto>", 
       colour = "Alto?",
       title = "Jugadores de baloncesto",
       subtitle = "(Grado de pertenencia al conjunto <Alto> de una manera no difusa)",
       caption = "Universidad de deusto")
