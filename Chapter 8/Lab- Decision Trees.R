##Fitting classification trees
library(tree)
library(ISLR)
attach(Carseats)
High = ifelse(Sales<=8, "No", "Yes")

Carseats = data.frame(Carseats, High)

tree.carseats = tree(High~.-Sales, Carseats)
summary(tree.carseats)
#the training error rate is 9%

#plotting the decision tree
plot(tree.carseats)
text(tree.carseats, pretty=0)

tree.carseats

#spltting the data into train and test sets
set.seed(2)
train = sample(1:nrow(Carseats),200)
Carseats.test = Carseats[-train,]
High.test = High[-train]
tree.carseats = tree(High~.-Sales, Carseats, subset=train)
tree.pred = predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
#prediction accuracy- sum of diagonals/200
(104+57)/200
# getting an 80% accuracy, in book's random split it was 71%. Need to investigate
#and think about this more. Because the model accuracy is dependent on the random
#split of test and train. Does it really affect the actual model? Is the model 
#I just created a bit better than the model in the book?


#Tree pruning
##FUN = prune.misclass implies we want to use classification error rate in
#cross validation and pruning process
set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats

## We find that the 8 variable model gives the lowest cross validation error, 75
## Again a point to ponder, in book, it's a 9 variable model with cv error of 50
## Now we plot error rate as a function of size and k

par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

#apply prune.misclass() in order to prune tree at 8 node tree 
prune.carseats = prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats)

#checking the test error rate for this 8 node model
tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
#prediction accuracy
(97+58)/200
#77.5%


##Let's try a 15 node pruned model and check it's accuracy
#apply prune.misclass() in order to prune tree at 15 node tree 
prune.carseats = prune.misclass(tree.carseats, best=15)
plot(prune.carseats)
text(prune.carseats)

#checking the test error rate for this 15 node model
tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
#prediction accuracy
(102+53)/200
#77.5%
#Crazy enough that we are seeing the same prediction accuracy


####8.3.2 Fitting Regression Trees
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~.,Boston, subset=train)
summary(tree.boston)
## for regression trees, deviance is simply squared of errors for the tree

plot(tree.boston)
text(tree.boston, pretty=0)

#now we prune the tree
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')
cv.boston

#tree pruning for the sake of it
prune.boston = prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

#making predictions
yhat = predict(tree.boston, newdata = Boston[-train,])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(1,0)
mean((yhat-boston.test)^2)


##8.3.3 Bagging and Random Forests
library(randomForest)
set.seed(1)
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE)
bag.boston

#test error
yhat.bag = predict(bag.boston, newdata = Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
##this model unlike the book, where the error sort of halved, doesn't 
#improve much, need to revist and see if a different split of test and train 
#would do the trick

##anyways, moving on, I can change the ntree argument to control the number of 
#trees that are constructed
bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)
yhat.bag = predict(bag.boston, newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)


##For random forest we use a smaller value of mtry
set.seed(1)
rf.boston = randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf = predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf-boston.test)^2)
#there is a slight improvement from 23.45 to 20.04

##importance function gives the imp of each variable
importance(rf.boston)
varImpPlot(rf.boston)
#the top 2 in the graphs are the most important variables



##8.3.4 Boosting
##We will be using the gbm function in the gbm package
## distribution = "gaussian" for regression problems
## distribution = "bernoulli" for binary classification problem

library(gbm)
set.seed(1)
boost.boston = gbm(medv~., data=Boston[train,], distribution = "gaussian", 
                   n.trees=5000, interaction.depth=4)
summary(boost.boston)

##partial dependence plots: illustrate the marginal effect of selected variables
##on the response after integrating out the other variables
par(mfrow=c(2,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

##use the boosted model to predict the medv on the test set
yhat.boost=predict(boost.boston, newdata = Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)

##we can change the shrinkage parameter if required
boost.boston = gbm(medv~., data = Boston[train,], distribution = "gaussian",
                 n.trees=5000, interaction.depth = 4, shrinkage = 0.2,
                 verbose = F)
yhat.boost=predict(boost.boston, newdata = Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)
##slight increase from lambda =0.2 from 0.001\









