library(ISLR)
library(MASS)

rm(list=ls())

#8
attach(Auto)
lm.fit=lm(mpg~horsepower)
#8 a)
summary(lm.fit)
predict(lm.fit, data.frame(horsepower=c(98)), interval = "prediction")
predict(lm.fit, data.frame(horsepower=c(98)), interval = "confidence")
#8 b)
abline(lm.fit)
#8 c)
par(mfrow=c(2,2))
plot(lm.fit)


#9
#9 a)
pairs(Auto)
#9 b)
cor(Auto[-9],Auto[-9])
#9 c)
lm.fit=lm(mpg~.-name, data = Auto)
summary(lm.fit)
#9 d)
par(mfrow=c(2,2))
plot(lm.fit)
#9 e)
lm.fit2=lm(mpg~cylinders*displacement+displacement*weight)
summary(lm.fit2)


#10
attach(Carseats)
#10 a)
lm.fit = lm(Sales~Price + Urban + US)
summary(lm.fit)

#10 e)
lm.fit1 = lm(Sales~Price + US)
summary(lm.fit1)

#10 g)
names(lm.fit)
confint(lm.fit)

#10 h)
par(mfrow=c(2,2))
plot(lm.fit1)

par(mfrow=c(1,1))
plot(predict(lm.fit1), rstudent(lm.fit1))


#11
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)
#11 a)
lm.fit=lm(y~x+0)
summary(lm.fit)
#11 b)
lm.fit1=lm(x~y+0)
summary(lm.fit1)
#11 f) with intercept
lm.fit=lm(y~x)
lm.fit1=lm(x~y)
summary(lm.fit)
summary(lm.fit1)

#13
#a)
x<-rnorm(100, mean = 0, sd=1)
#b)
eps<-rnorm(100, mean = 0, sd=0.25)
#c)
y=-1+0.5*x+eps
length(y)
#d)
plot(x,y)
#e)
lm.fit = lm(y~x)
summary(lm.fit)
#f)
plot(x,y)
abline(-1, 0.5, col="blue", lwd=2, lty="dashed")
abline(lm.fit, col="red", lwd=2)
legend(x="bottomright",legend = c("population", "model fit"),
       col = c("blue", "red"), lwd = 2)
#g)
lm.fit2=lm(y~x+I(x^2))
summary(lm.fit2)
anova(lm.fit, lm.fit2)
#h) redo with less noisier data
x1=rnorm(100, mean=0, sd=1)
eps1=rnorm(100, mean=0, sd=0.05)
y1=-1+0.5*x1+eps1
plot(x1,y1)
lm.fit_1 = lm(y1~x1)
summary(lm.fit_1)
plot(x1,y1)
abline(-1, 0.5, col="blue", lwd=2, lty="dashed")
abline(lm.fit_1, col="red", lwd=2)
legend(x="bottomright",legend = c("population", "model fit"),
       col = c("blue", "red"), lwd = 2)
lm.fit2_1=lm(y1~x1+I(x1^2))
summary(lm.fit2_1)
anova(lm.fit_1, lm.fit2_1)


#14
#a)
set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2+2*x1+0.3*x2+rnorm(100)
#b)
cor(x1, x2)
plot(x1, x2)
#c)
lm.fit = lm(y~x1 + x2)
summary(lm.fit)
#d)
lm.fit1 = lm(y~x1)
summary(lm.fit1)
#e)
lm.fit2 = lm(y~x2)
summary(lm.fit2)
#g)
x1=c(x1,0.1)
x2=c(x2,0.8)
y=c(y,6)

par(mfrow=c(2,2))
plot(lm.fit)
