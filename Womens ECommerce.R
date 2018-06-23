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
# ver [3] segun la media de si con[3] recomienda o no

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

##################################################################################
#######################          Parte 2      ####################################
##################################################################################
############## Estadistica Descriptiva
summary(reviews)
