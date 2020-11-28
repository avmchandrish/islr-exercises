library(ISLR)
library(MASS)
names(Smarket)
help(Smarket)
dim(Smarket)
summary(Smarket)

#pair plot
pairs(Smarket)

#correlation
cor(Smarket[, -9])

attach(Smarket)
plot(Volume)


# Logistic Regression to predict the direction using Lag1 through Lag5 and VOlume
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
              data=Smarket, family= binomial)
summary(glm.fit)


coef(glm.fit)
#for p values from the summary 
summary(glm.fit)$coef[,4]

#predict the values from the model
glm.probs = predict(glm.fit, type="response")
glm.probs[1:10]
#to understand how the model has tagged the qualitative variables
contrasts(Direction)
#using the probablities an threshold
glm.pred=rep("Down", 1250)
glm.pred[glm.probs>0.5]="Up"

#confusion matrix
table(glm.pred, Direction)
#percentage of times the movement of market is correctly predicted
(507+145)/1250

#the above estimates the training error which can underestimate the testing 
#error rate. hence breaking the data 
train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

#fitting the subset train data
glm.fit = glm(Direction~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket,
              family=binomial, subset=train)
glm.probs=predict(glm.fit, Smarket.2005, type="response")

#checking the test error rate
glm.pred=rep("Down", 252)
glm.pred[glm.probs>0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)


###Fitting a similar model as above but with only two predictors, Lag1 and Lag2
glm.fit1 = glm(Direction~Lag1 + Lag2, data= Smarket, family=binomial
               , subset=train)
summary(glm.fit1)
glm.probs1 = predict(glm.fit1, Smarket.2005, type="response")
glm.pred1 = rep("Down", 252)
glm.pred1[glm.probs1>0.5]="Up"
table(glm.pred1, Direction.2005)
mean(glm.pred1==Direction.2005)

#predicting on a particular dataset
predict(glm.fit1, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)),
        type="response")
