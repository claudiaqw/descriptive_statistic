library("ISLR")
library('ggplot2')

▲#El dataset Wage de la biblioteca ISLR reúne información sobre el salario y 
# otros datos de 3000 trabajadores hombres en la región del Atlántico Medio.

#El dataset contiene 11 variables. A continuación una pequeña muestra:
head(Wage)


#Análisis descriptivo
#Para comprobar que a cada variable en el dataset le fue asignado el tipo correcto
str(Wage)

#Frequency tables and bar charts
table(Wage$year)
prop.table(table(Wage$year))
hist(Wage$year)

table(Wage$age)
hist(Wage$age)

table(Wage$maritl)
barplot(table(Wage$maritl))


table(Wage$race)
barplot(table(Wage$race))


table(Wage$education)
barplot(table(Wage$education))


table(Wage$region)


table(Wage$jobclass)


table(Wage$health)

table(Wage$education)
barplot(table(Wage$education))


hist(Wage$wage)


hist(Wage$logwage)


# Descriptive statistics
summary(Wage)


#Estadísticos de posición
numeric_cols = sapply(Wage, is.numeric)

#Media
apply(X = Wage[, numeric_cols], MARGIN = 2, FUN = mean, na.rm = T)

#Mediana
apply(X = Wage[, numeric_cols], MARGIN = 2, FUN = median, na.rm = T)

#Cuantiles
apply(X = Wage[, numeric_cols], MARGIN = 2, FUN = quantile(), na.rm = T)


#Estadísticos de dispersión
 
#rango intercuartilico muetsral RI
apply(X = Wage[, numeric_cols], MARGIN = 2, FUN = IQR, na.rm = T)

#varianza
apply(X = Wage[, numeric_cols], MARGIN = 2, FUN = var, na.rm = T)

#desviación típica
apply(X = Wage[, numeric_cols], MARGIN = 2, FUN = sd, na.rm = T)


#Estadisticos de forma
library(e1071)
apply(X = Wage[, numeric_cols], MARGIN = 2, FUN = skewness, na.rm = T)

apply(X = Wage[, numeric_cols], MARGIN = 2, FUN = kurtosis, na.rm = T)




  