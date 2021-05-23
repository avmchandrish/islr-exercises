#Lab 1: Subset Selection Methods
#6.5.1. Best Subset Selection

library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))


library(leaps)
regfit.full = regsubsets(Salary~., Hitters)
summary(regfit.full)

#By default the function regsubsets only reports results of best 8 variable model
#However this can be changed by using the nvmax function

regfit.full = regsubsets(Salary~., data = Hitters, nvmax=19)
regfit.summary = summary(regfit.full)

names(regfit.summary)
#getting the r squared for each number variable model
regfit.summary$rsq

#Plotting rss, rsquared
par(mfrow=c(2,2))
plot(regfit.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(regfit.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

which.max(regfit.summary$adjr2)
points(11, regfit.summary$adjr2[11], col="red", cex=2, pch=20)

#similarly plotting cp and bic stats understand which is the smallest
plot(regfit.summary$cp, xlab = "Number of Variables", ylab = "Cp", type="l")
which.min(regfit.summary$cp)
points(10, regfit.summary$cp[10], col="red", cex=2, pch=20)

plot(regfit.summary$bic, xlab = "Number of Variables", ylab = "BIC", type="l")
which.min(regfit.summary$bic)
points(6, regfit.summary$bic[6], col="red", cex=2, pch=20)

#using the plot function regsubsets()
par(mfrow=c(1,1))

plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

#The sweet number of variables where the minimum occurs for each of the metrics above
#is shown in these plots. Need to understand which value has to reduce and which has to 
#increase


#-----------------------------------------------------------------
##FORWARD AND BACKWARD STEPWISE SELECTION
regfit.fwd = regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)

regfit.bwd = regsubsets(Salary~.,data=Hitters, nvmax=19, method="backward")
summary(regfit.bwd)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

#CHOOSING AMONG THE MODELS USING THE VALIDATION SET APPROACH AND CROSS VALIDATION
#creating a random test and training set split
set.seed(1)
train = sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test = (!train)

#now training on the training dataset
regfit.best = regsubsets(Salary~., data= Hitters[train,], nvmax=19)

#creating model matrix
test.mat = model.matrix(Salary~., data=Hitters[test,])

#the concept of model matrix is to build an X matrix for the model data, so that
#the coefficients from the best model will multiply with corresponding variables 
val.errors = rep(NA, 19)
for (i in 1:19){
  coefi = coef(regfit.best, id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

val.errors
which.min(val.errors)
coef(regfit.best, 7)

#creating your own predict function
predict.regsubsets = function(object, newdata, id){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

#because my train and test split is different am getting a 7 variable model
#I will use the entire dataset to find the best 7 variables
#Please note: for validation set I used only part of the data by splitting into 
#training and test. That approach is just for finding the best number of 
#variables in the model.

regfit.best=regsubsets(Salary~., data = Hitters, nvmax=19)
coef(regfit.best, 7)


#----------------------------------------------
#Now doing the same approach above using cross validation, k-fold cross 
#validation approach
k=10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors=matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

for(j in 1:k){
  best.fit = regsubsets(Salary~., data = Hitters[folds!=j,], nvmax=19)
  for(i in 1:19){
    pred = predict(best.fit, Hitters[folds==j,], id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}


mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type = 'b')

#plot for me gives a 10 variable model as the best
#now training the model non the whole dataset
reg.best = regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best, 10)



#----------------------------------------------
#-------------Lab2:Ridge Regression and The Lasso 

x = model.matrix(Salary~., Hitters)[,-1]
y = Hitters$Salary

#model.matrix function, produces matrix corresponding to the 19 predictors and
#automatically transforms any qualitative variables into dummy variables


#----- Ridge Regression
library(glmnet)
grid = 10^seq(10, -2, length=100)
ridge.mod = glmnet(x, y, alpha=0, lambda=grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
#l2 norm
sqrt(sum(coef(ridge.mod)[-1,50]^2))


ridge.mod$lambda[60]
coef(ridge.mod)[,60]
#l2 norm
sqrt(sum(coef(ridge.mod)[-1,60]^2))


#we can use the predict function to obtain ridge regression for a given value of lambda
predict(ridge.mod, s=50, type="coefficients")[1:20,]

#splitting

#continue the ridge regression when you get time



######## THE LASSO
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)

































  
  


