#######################################
# HW#6
# Yiling Chen
# Dec 1, 2020
#######################################
# Q1, Q2
#######################################
rm(list=ls())

#set working directory
setwd("/Users/elenachen/Desktop/EAS506/Wk12")
load("pima.RData")
#install.packages("gbm")
library(randomForest)
library(rpart)
library(MASS)
library(class)
library(gbm)

pima <- pima[1:8]
summary(pima)
head(pima)
View(pima)
sum(is.na(pima))
nrow(pima) #532
ncol(pima) #8

#split into train and testing
sample.pima <- floor(0.80 * nrow(pima))
set.seed(123)
train_ind <- sample(seq_len(nrow(pima)),size =sample.pima)
p.train <- pima[train_ind,]
p.test <- pima[-train_ind,]
y_true <- as.numeric(p.test$classdigit)-1
##CART model
model.control <- rpart.control(minsplit = 5, xval = 7, cp=0)
fit <- rpart(classdigit~., data=p.train, method = "class", control=model.control)
#Full Tree
x11()
plot(fit, branch = .5, compress=T,main = "Full Tree")
text(fit, cex = .8)
#prune the tree
min_cp = which.min(fit$cptable[,4])
min_cp #4
pruned_fit <- prune(fit, cp=fit$cptable[min_cp,1]) 
#pruned tree
x11()
plot(pruned_fit, branch = .5, compress=T,main = "Pruned Tree")
text(pruned_fit, cex = 1)

# Compute test error for a single tree
my_pred <- predict(pruned_fit, newdata = p.test, type = "class")
y_hat <- as.numeric(my_pred)-1
misclass_tree <- sum(abs(y_true - y_hat))/length(y_hat)
misclass_tree #23.36%


##Random Forest
pima.rf.model <- randomForest(classdigit~., data = p.train, ntree = 1000)
pima.rf.model 

#Compute test error for RF
y_hat <- predict(pima.rf.model, newdata = p.test, type = "response")
y_hat <- as.numeric(y_hat)-1
misclass_rf <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_rf # 23.36%

#variable importance
x11()
varImpPlot(pima.rf.model)
importance(pima.rf.model) #glucose,age,bmi,pedigree,etc

#partial dependence plot for RF
partialPlot(pima.rf.model, pred.data = p.train, x.var="glucose")
partialPlot(pima.rf.model, pred.data = p.train, x.var="age")
partialPlot(pima.rf.model, pred.data = p.train, x.var="pedigree")

##bagging
bag.fit <- randomForest(classdigit~., data = p.train, n.tree = 1000, mtry = 3)

#variable importance
x11()
varImpPlot(bag.fit)

importance(bag.fit)

y_hat <- predict(bag.fit, newdata = p.test, type = "response")
y_hat <- as.numeric(y_hat)-1
misclass_bag <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_bag # 22.42%


##KNN
pred.knn <- knn(Train <- data.frame(p.train$glucose),
                Test <- data.frame(p.test$glucose),
                cl = p.train$classdigit, k =20)
table(pred.knn, p.test$classdigit)
mean(pred.knn == p.test$classdigit) #74.77%

##boosting
boost.train <-p.train;
boost.train$classdigit <- as.numeric(p.train$classdigit)-1
boost.test <- p.test;
boost.test$classdigit <-as.numeric(p.test$classdigit)-1

boost.fit <- gbm(classdigit~., data = boost.train, n.trees = 1000, shrinkage = .1, 
                 interaction.depth = 3, distribution = "adaboost")
boost.fit2 <- gbm(classdigit~., data = boost.train, n.trees = 1000, shrinkage = .6, 
                  interaction.depth = 3, distribution = "adaboost")

summary(boost.fit)
summary(boost.fit2)

# Look at the error for shrinkage = .1
y_hat <- predict(boost.fit, newdata = boost.test, n.trees = 1000, type = "response")
misclass_boost.1 <- sum(abs(y_hat - y_true))/ length(y_true)
misclass_boost.1 # 24.11%

# Look at the error for shrinkage = .6
y_hat <- predict(boost.fit2, newdata = boost.test, n.trees = 1000, type = "response")
misclass_boost.6 <- sum(abs(y_hat - y_true))/ length(y_true)
misclass_boost.6 # 28.75%


shrink <- c(.1, .4, .6, .8)
max_iter <- 3000
store_error <- c()
for (i in 1:length(shrink)){
  boost.fit <- gbm(classdigit~., data = boost.train, n.trees = max_iter, shrinkage = shrink[i], interaction.depth = 3, distribution = "adaboost")
  temp <- c()
  for (j in 1:max_iter){
    y_hat <- predict(boost.fit, newdat = boost.test, n.trees = j, type ="response")
    misclass_boost <- sum(abs(y_true - y_hat))/length(y_hat)
    temp <- c(temp, misclass_boost)
  }
  store_error <- cbind(store_error, temp) # max_iter x length(shrink)
}

colnames(store_error) <- paste("shrinkage", shrink, sep = ":")

x11()
plot(store_error[,1], type = "l", main = "Error Profiles", ylab = "error", xlab = "boosting iterations", ylim = c(.07, .5))
lines(store_error[,2], col = "red")
lines(store_error[,3], col = "blue")
lines(store_error[,4], col = "green")


#######################################
# Q3
#######################################
#set working directory
setwd("/Users/elenachen/Desktop/EAS506/Wk12")
spam = read.table("spam.txt")
library(randomForest)
library(MASS)
library(class)

summary(spam)
head(spam)
View(spam)
sum(is.na(spam))
nrow(spam) #4601
ncol(spam) #58

#split into train and testing
sample.spam <- floor(0.80 * nrow(spam))
set.seed(128)
train_ind <- sample(seq_len(nrow(spam)),size =sample.spam)
s.train <- spam[train_ind,]
s.test <- spam[-train_ind,]

s.train$V58 <- as.character(s.train$V58)
s.train$V58 <- as.factor(s.train$V58)

#3 variables at each split
spam3.rf.model <- randomForest(V58~., data=s.train,mtry=3,ntree=1000)
spam3.rf.model #OOB 5%
spam3.rf.pred <- predict(spam3.rf.model,s.test, type = "class")
mean(spam3.rf.pred != s.test$V58) #test error 5.2%

#5 variables at each split
spam5.rf.model <- randomForest(V58~., data=s.train,mtry=54,ntree=1000)
spam5.rf.model #OOB 5.33%
spam5.rf.pred <- predict(spam5.rf.model,s.test, type = "class")
mean(spam5.rf.pred != s.test$V58) #test error 5.7%

#10 variables at each split
spam10.rf.model <- randomForest(V58~., data=s.train,mtry=10,ntree=1000)
spam10.rf.model #OOB 4.65%
spam10.rf.pred <- predict(spam10.rf.model,s.test, type = "class")
mean(spam10.rf.pred != s.test$V58) #test error 5.4%

#30 variables at each split
spam30.rf.model <- randomForest(V58~., data=s.train,mtry=30,ntree=1000)
spam30.rf.model #OOB :5.2%
spam30.rf.pred <- predict(spam30.rf.model,s.test, type = "class")
mean(spam30.rf.pred != s.test$V58) #test error 5.4%


