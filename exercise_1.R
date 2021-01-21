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
apply(X = Wage[, numeric_cols], MARGIN = 2, FUN = quantile, na.rm = T)


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


#Ajusta un modelo de regresión lineal para predecir logwage usando 
#age, jobclass, race y health_ins
library(dplyr)

unique(Wage$region) #como existe una sola región lo quitamos porque no nos va a dar mucha información

pairs(Wage[-6], col = 'red')

lm_logwage <- lm(logwage ~ age + jobclass + race + health_ins, data=Wage)
lm_logwage

#model
#logwage = 0.0048*age + 0.1045*jobclass2.Information - 0.1056*race2. Black + 0.0624*race3. Asian - -0.1419*race4. Other - 0.2461*health_ins2. No + 4.4798

summary(lm_logwage)

confint(lm_logwage, level = 0.95) #intervalos de confianza

#plot(Wage$logwage, Wage$Sales, type = 'p', col = 'red', xlab = 'TV', ylab = 'Sales')
#abline(lm_logwage, col = 'blue')

plot(Wage$logwage, lm_logwage$residuals, type = 'p', col = 'red', xlab = 'TV', ylab = 'Sales')


  