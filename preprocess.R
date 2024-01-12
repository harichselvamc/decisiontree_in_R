
#import dataset
movie <- read.csv("Movie_regression.csv")
View(movie)

#Data Preprocessing
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = TRUE)









# Test-Train Split
#install.packages('caTools')
library(caTools)
set.seed(0)
split =sample.split(movie,SplitRatio = 0.8)
train = subset(movie,split == TRUE)
test = subset(movie,split == FALSE)












#install required packages
#install.packages('rpart')
#install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

#Run regression tree model on train set
regtree <- rpart(formula = Collection~., data = train, control = rpart.control(maxdepth = 3))
#press F1 on rpart for help on this function

#Plot the decision Tree
rpart.plot(regtree, box.palette="RdBu", digits = -3)

#Predict value at any point
test$pred <- predict(regtree, test, type = "vector")

MSE2 <- mean((test$pred - test$Collection)^2)







#Tree Pruning
fulltree <- rpart(formula = Collection~., data = train, control = rpart.control( cp = 0))
rpart.plot(fulltree, box.palette="RdBu", digits = -3)
printcp(fulltree)
plotcp(regtree)

mincp <- regtree$cptable[which.min(regtree$cptable[,"xerror"]),"CP"]

prunedtree <- prune(fulltree, cp = mincp)
rpart.plot(prunedtree, box.palette="RdBu", digits = -3)

test$fulltree <- predict(fulltree, test, type = "vector")
MSE2full <- mean((test$fulltree - test$Collection)^2)

test$pruned <- predict(prunedtree, test, type = "vector")
MSE2pruned <- mean((test$pruned - test$Collection)^2)



printcp(regtree)
fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
plotcp(regtree)

#Bagging
library (randomForest)
set.seed (1)
bagging =randomForest(Collection~Budget+Trailer_views, data = train ,mtry=2, importance =TRUE)
test$bagging <- predict(bagging, test)
MSE2bagging <- mean((test$bagging - test$Collection)^2)

#Random forest
#install.packages('randomForest')
library(randomForest)

fit <- randomForest(Collection~Budget+Trailer_views, data = train,ntree=500)
summary(fit)
#Predict Output 
test$random <- predict(fit, test)
MSE2random <- mean((test$random - test$Collection)^2)

