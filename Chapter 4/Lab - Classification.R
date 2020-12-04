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



#----------------------------------------------------
#Linear Discriminant Analysis (LDA)
lda.fit= lda(Direction~Lag1 + Lag2, data=Smarket, subset=train)
lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
#output: class, posterior, x
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)

table(lda.class)

##  go over the posterior probability part again


#--------------------------------------------------------------
#Quadratic Discriminant Analysis (QDA)
qda.fit = qda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
qda.fit

qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)


#---------------------------------------------------------------
#k-nearest neighbors
library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

set.seed(1)
# I don't fully understand the reasoning behind the above statement, revisit this
knn.pred= knn(train.X, test.X, train.Direction,k=1)
table(knn.pred, Direction.2005)
(43+83)/252 #0.5

#trying the above with k=3
knn.pred= knn(train.X, test.X, train.Direction,k=3)
table(knn.pred, Direction.2005)
(48+85)/252 #0.53, that's the max it can reach with this approach

#So, QDA gives the best results with 60% accuracy



#------------ Caravan Insurance Data -----------------------
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

# for KNN classifiers the scale of the variables matters a lot, 
# because the distance between observations would be affected
# scale function helps in standardizing the data
standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])

var(standardized.X[,1])
var(standardized.X[,1])

test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=1)

mean(test.Y!=knn.pred)
mean(test.Y!="No")

table(knn.pred, test.Y)
9/(68+9)

#trying out with k=3
knn.pred = knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
5/26

#trying with k=5
knn.pred = knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)
4/(11+4)

## let's try a logistic regression on this data with cut off at 0.25
glm.fit = glm(Purchase~., data= Caravan, family=binomial, subset=-test)
glm.probs = predict(glm.fit, Caravan[test,], type="response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs>0.5] = "Yes"
table(glm.pred, test.Y)

#this gives very less number of positive results, let's try and decrease the threshold
glm.pred = rep("No", 1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred, test.Y)
11/(22+11)
