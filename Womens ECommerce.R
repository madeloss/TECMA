##################################################################################
####################      TECMA - Obligatorio     ################################  
##################################################################################

###### Librerias requeridas
# importar la librería ISLR
if (!require(ISLR))
  install.packages("ISLR")
library(ISLR)

# importar la librería readr, si se utiliza read_csv
if (!require(readr))
  install.packages("readr")
library(readr)

# Se agrego para poder realizar %<%
if (!require(dplyr))
  install.packages("dplyr")
library(dplyr)
# Se agrego para poder realizar corrplot
if (!require(corrplot))
  install.packages("corrplot")
library(corrplot)


# Carga de los datos en un data set llamado "reviews"
# Si cargamos los datos con esta sentencia "Text" y "Review Text", " Division Name", 
# "Department Name" y "Class Name" estan en tipo Factor con la cantidad de niveles 
reviews <- read.csv2('Womens Clothing E-Commerce Reviews.csv', sep = ",")
# Si cargamos de esta forma "Title" y "Review Text", " Division Name", 
# "Department Name" y "Class Name" ya estan en tipo chars
reviews <- read_csv('Womens Clothing E-Commerce Reviews.csv')
# Tenemos que ver por el tema del análisis descriptivo, capaz que nos sirve la primera

##################################################################################
#######################          Parte 1      ####################################
##################################################################################

############### Descripción de Observaciones, atributos y tipos
# Reviews es un data set con 23.486 observaciones y 11 variables.
# X1: (int) Indica el numero de comentario en el data set
# CLothing ID: (int) Referencia la prenda a la cual se le esta realizando el comentario
# Age: (int) Edad de la persona que realiza el comentario
# Title: (chars-Factor) Titulo del comentario
# Review Text: (chars-Factor) Texto con todo el comentario
# Rating: (int) Calificacion por parte del comprador acerca de la prenda comprada
# [1 - mala, 5 - excelente]
####### Esta "Rating" se puede convertir en "Factor"
# Recommended IND: (int) Opinion del comprador si recomienda que compren los demás
# la prenda o no [1 - Recomienda, 0 - No recomienda] 
####### Esta "Recommended.IND" se puede convertir en "Factor"
# Positive Feedback Count: (int) Esta variable es la devolucion por parte de otros 
# clientes afirmando que el comentario les sirvio
# Division Name: (chars-Factor) Categoría de las prendas segun la division a un alto nivel
# [General, General Petite, Initmates, ""] (con levels() no se ve que haya los 4 niveles, se utilizan solo los 3 con nombres)
# Department Name: (chars-Factor) Categoria de las prendas pero dentro del departamento que les corresponde
# [Bottoms, Dresses, Intimate, Jackets, Tops, Trend, ""] pasa lo mismo que con la variable anterior
# Class Name: (chars-Factor) Clase a la cual pertenece la prenda
# [Blouses, Casual bottoms, Chemises, Dresses, Fine gauge, Intimates, Jackets, Jeans
# Knits, Layering, Legwear, Lounge, Outerwear, Pants, Shorts, Skirts, Sleep, Sweaters
# Swim, Trend, ""] pasa lo mismo que con la variable anterior

############ Tratamiento de NA
# Como la variable esta en Factor entonces la transformamos en character para poder trabajar con ella
reviews$Title <- as.character(reviews$Title)
reviews$Review.Text <- as.character(reviews$Review.Text)
# En el dataset no dice NA, solo esta el campo en blanco entonces transformamos
# las variables para que cuando haya un blanco sea NA
i <- 1
for (i in 1:length(reviews$X)) {
  if (reviews$Title[i] == ""){
    reviews$Title[i] <- NA
  }
  if (reviews$Review.Text[i] == ""){
    reviews$Review.Text[i] <- NA
  }
}

# Tenemos 3810 elementos dentro de reviews$Title que son NA
sum(is.na(reviews$Title))
# Tenemos 845 elementos dentro de reviews$Review.Text que son NA
sum(is.na(reviews$Review.Text))
# Ver que se puede hacer en estos casos
# Para cuando no haya Title, podemos usar el Review Text pero 
# cuando no haya ninguna de las dos ver si se elimina o se toma en cuenta el rating
# para el caso que el Rating sea [0-2] - no recomienda
# [4-5] - recomienda pero [3]... es indeciso

############ Tratamiento de Factores

# Construimos la variable Recommended IND en un factor de:
# [0 - no recomienda, 1 - recomienda]
reviews$Recommended.IND <- as.factor(reviews$Recommended.IND)

# Para la variable Rating, tenemos que categorizar, sabemos que:
# [0-2] - no recomienda
# [4-5] - recomienda
# ver [3] segun la media de si con [3] recomienda o no

i <- 1
cuantosRecomiendan <- 0
for (i in 1:length(reviews$X)) {
  if(reviews$Rating[i] == 3){
    cuantosRecomiendan <- cuantosRecomiendan + reviews$Recommended.IND[i]
  }
}
Rating3 <- sum(reviews$Rating == 3)

# tenemos que de 2871 observaciones con rating 3, 1189 recomiendan el producto
# menos de la mitad.... ver que tomamos tiran mas a no recomendar pero esta
# ajustado

reviews$recomienda[reviews$Rating <= 2] <- 0
# ver como se determina el valor 3 por ahora esta para "recomendar"
reviews$recomienda[reviews$Rating >= 3] <- 1

##################################################################################
#######################          Parte 2      ####################################
##################################################################################
############## Estadistica Descriptiva
summary(reviews)

factores <- reviews %>%  select_if(is.factor)
names(factores)
noFactores <- reviews[,!names(reviews) %in% names(factores)]
noFactores$X <- NULL
noFactores$Clothing.ID <- NULL
noFactores$Title <- NULL
noFactores$Review.Text <- NULL
noFactores$recomiend <- NULL
names(noFactores)

reviews$Recommended.IND <- as.numeric(reviews$Recommended.IND)
cor(reviews[, names(reviews) %in% names(noFactores)],reviews$Recommended.IND)

# No se ven los nombres de los departamentos o clases
# los transforma como numero
reviews$Department.Name <- as.numeric(reviews$Department.Name)
boxplot(reviews$Department.Name ~ reviews$Recommended.IND, data = reviews, xlab = "Recomienda", ylab ="Nombres de Secciones")

reviews$Class.Name <- as.numeric(reviews$Class.Name)
boxplot(reviews$Class.Name ~ reviews$Recommended.IND, data = reviews, xlab = "Recomienda", ylab ="Clases de las prendas")

# Descriptivos
mean <- sapply(noFactores, mean)
sd <- sapply(noFactores, sd)
median <- sapply(noFactores, median)
max <- sapply(noFactores, max)
Nas <- colSums(is.na(noFactores))
q1 <- t(sapply(noFactores, quantile)) #Cuantil Q1
estadisticosNoFactores <- round(cbind(mean, median, sd, max, Nas, q1),digits=1)
estadisticosNoFactores

# Histogramas
summary(noFactores[,1])
hist(noFactores[,1])
hist(noFactores[,1], 
     main="Histograma de la edad de los comentarios", 
     xlab="Edad", 
     border="blue", 
     col="green",
     xlim=c(17,99), # en un entorno de 17 a 99
     las=1, 
     breaks=25)
# Este ploteo abarca de 17 a 99 que es la edad minima y maxima 
