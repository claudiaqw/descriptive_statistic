library(mlbench)
library(dplyr)
library(leaps)
library(mlbench) 

head(BostonHousing2)


summary(BostonHousing)


for(i in 1:13){
  plot( BostonHousing[,i], BostonHousing$medv, type='p', col='blue', xlab = names(BostonHousing)[i], ylab = 'medv' )
}


exhaustive_model <- regsubsets(medv ~ ., data = BostonHousing)
summary(exhaustive_model)

plot(summary(exhaustive_model)$rsq, type = 'l')

summary(exhaustive_model)$adjr2


lm_6 <- lm(medv ~ chas + nox + rm + dis + ptratio + lstat, data = BostonHousing)
summary(lm_6)
