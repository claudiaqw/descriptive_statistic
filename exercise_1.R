library(ISLR)
library(ggplot2)
library(dplyr)

▲#El dataset Wage de la biblioteca ISLR reúne información sobre el salario y 
# otros datos de 3000 trabajadores hombres en la región del Atlántico Medio.

#El dataset contiene 11 variables. A continuación una pequeña muestra:
head(Wage)


#Análisis descriptivo
#Para comprobar que a cada variable en el dataset le fue asignado el tipo correcto
str(Wage)

glimpse(Wage)


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
barplot(table(Wage$jobclass))

table(Wage$health)
barplot(table(Wage$jobclass))

#Workers raw wage
hist(Wage$wage)

hist(Wage$logwage)


# Descriptive statistics
summary(Wage)


#Estadísticos de posición
numeric_cols = sapply(Wage, is.numeric)

#ESto no es necesario porque ya se ve en el summary
#Media
apply(X = Wage[, numeric_cols], MARGIN = 2, FUN = mean, na.rm = T)

#Mediana
apply(X = Wage[, numeric_cols], MARGIN = 2, FUN = median, na.rm = T)

#Cuantiles
apply(X = Wage[, numeric_cols], MARGIN = 2, FUN = quantile, na.rm = T)


#Estadísticos de dispersión
 
#rango intercuartilico muestral RI
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

unique(Wage$region) #como existe una sola región lo quitamos porque no nos va a dar mucha información

pairs(Wage, col = 'blue')

pairs(Wage[, numeric_cols], col = 'blue')

lm_logwage <- lm(logwage ~ age + jobclass + race + health_ins, data=Wage)
summary(lm_logwage)


confint(lm_logwage, level = 0.95) #intervalos de confianza

#plot(Wage$logwage, Wage$Sales, type = 'p', col = 'red', xlab = 'TV', ylab = 'Sales')
#abline(lm_logwage, col = 'blue')

plot(Wage$logwage, lm_logwage$residuals, type = 'p', col = 'red', ylab = 'Wage')


Wage$is_black <- ifelse(Wage$race == "2. Black",1, 0)


lm_logwage_1 <- lm(logwage ~ age + jobclass + is_black + health_ins, data=Wage)
summary(lm_logwage_1)


lm_logwage_2 <- lm(logwage ~ jobclass + race + health_ins, data=Wage)
summary(lm_logwage_2)


lm_logwage_3 <- lm(logwage ~ jobclass + is_black + health_ins, data=Wage)
summary(lm_logwage_3)


plot(hatvalues(lm_logwage_2), col = 'blue')

plot(hatvalues(lm_logwage), col = 'blue')


library(leaps)
exhaustive_model <- regsubsets(logwage ~ age + jobclass + race + health_ins - wage, data = Wage)
summary(exhaustive_model)

plot(summary(exhaustive_model)$rsq, type = 'l')

summary(exhaustive_model)$adjr2


lm_logwage_4 <- lm(logwage ~ age + jobclass + is_black + health_ins, data = Wage)
lm_logwage_4
summary(lm_logwage_4)


plot(Wage$logwage, lm_logwage_4$residuals, col = 'blue', xlab = 'logwage', ylab = 'residuals')


plot(hatvalues(lm_logwage_4), col = 'blue')

cooksd <- cooks.distance(lm_logwage_4)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm=T), col="red")

