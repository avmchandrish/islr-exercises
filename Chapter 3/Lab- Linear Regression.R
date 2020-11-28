library(MASS)
library(ISLR)

fix(Boston)
names(Boston)

attach(Boston)
lm.fit=lm(medv~lstat)

summary(lm.fit)
lm.fit

names(lm.fit)
coef(lm.fit)
confint(lm.fit)

predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval = "confidence")
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval = "prediction")

plot(lstat,medv,col="red",pch=20)
plot(lstat,medv,col="red",pch="+")
plot(1:20, 1:20, pch=1:20)

abline(lm.fit, lwd=3, col= "red")

#plotting
par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


#Multiple Linear Regression
lm.fit=lm(medv~lstat+age, data=Boston)
lm.fit
summary(lm.fit)

#for using all variables
lm.fit=lm(medv~., data=Boston)
summary(lm.fit)

#accessing individual componentsof a summary object by name
?summary.lm

#R2
summary(lm.fit)$r.sq

#RSE
summary(lm.fit)$sigma

#VIF
library(car)
vif(lm.fit)

#removing one variable
lm.fit1=lm(medv~.-age, data=Boston)
summary(lm.fit1)

#interaction terms
summary(lm(medv~lstat*age, data=Boston))

#non-linear transformation
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
plot(lm.fit2)

lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)

summary(lm(medv~log(rm), data = Boston))


##Qualitative Predictors
fix(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age, data= Carseats)
summary(lm.fit)

#what codes R used for dummy variables
contrasts(ShelveLoc)
attach(Carseats)

#functions
LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded")
}

LoadLibraries()
