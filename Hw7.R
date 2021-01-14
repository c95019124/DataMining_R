## Yiling Chen
## Created: December 11, 2020 
## HW#7
#####################################################################
#1
#####################################################################
rm(list = ls())

#set working directory
setwd("/Users/elenachen/Desktop/EAS506/Wk14")
load("cleveland.RData")
library(randomForest)
library(rpart)
#install.packages("neuralnet")
library(neuralnet)
library(nnet)
#install.packages("caret")
library(caret)
library(lattice)
library(ggplot2)
#preprocess
cleveland <- cleveland[1:14]
summary(cleveland)
head(cleveland)
View(cleveland)
sum(is.na(cleveland))
nrow(cleveland) #296
ncol(cleveland) #14

dummy <- dummyVars(" ~.", fullRank = TRUE, data=cleveland)
#effectively does what model matrix does
cleveland.new <- data.frame(predict(dummy, newdata = cleveland))
#preprocess
head(cleveland.new)
View(cleveland.new)
sum(is.na(cleveland.new))
nrow(cleveland.new) #296
ncol(cleveland.new) #19

#neural network
#split into train and testing
set.seed(5520)
sample.cleveland_new <- floor(0.80 * nrow(cleveland.new))
train_ind_new <- sample(seq_len(nrow(cleveland.new)),size =sample.cleveland_new)
c.train_new <- cleveland.new[train_ind_new,]
c.test_new <- cleveland.new[-train_ind_new,]
y_true_new <- as.numeric(c.test_new$diag1.sick)

# train a neural network
nn3 <- neuralnet(diag1.sick ~ age + gender.male + cp.angina + cp.asympt + cp.notang
                 + trestbps + chol + fbs.true + restecg.hyp + restecg.norm + thalach
                 + exang.true + oldpeak + slope.flat + slope.up + ca + thal.norm +
                   thal.rev , data = c.train_new, 
                 hidden = 3, err.fct = "ce", linear.output = FALSE)
plot(nn3)

#predict neural network
pred <- predict(nn3, newdata = c.train_new)
#pred
y_hat_train <- round(pred)
train_err <- length(which(c.train_new$diag1.sick != y_hat_train))/length(y_hat_train)
train_err #11%

pred <- predict(nn3, newdata = c.test_new)
y_hat_test <- round(pred)
test_err <- length(which(c.test_new$diag1.sick != y_hat_test))/length(y_hat_test)
test_err #20%

#tuning
train_err_store <- c()
test_err_store <- c()
for (i in 1:18){
  nn3 <- neuralnet(diag1.sick ~ age + gender.male + cp.angina + cp.asympt + cp.notang +
                     trestbps + chol + fbs.true + restecg.hyp + restecg.norm + thalach+ 
                     exang.true + oldpeak + slope.flat + slope.up + ca + thal.norm +
                     thal.rev , data = c.train_new, hidden = i, stepmax = 10^9, 
                   err.fct = "ce", linear.output = FALSE)
  
  #calculate the train error
  pred <- predict(nn3, newdata = c.train_new)
  y_hat_train <- round(pred)
  train_err <- length(which(c.train_new$diag1.sick != y_hat_train))/length(y_hat_train)
  train_err
  train_err_store <- c(train_err_store, train_err)
  #calculate the test error
  pred <- predict(nn3, newdata = c.test_new)
  y_hat_test <- round(pred)
  test_err <- length(which(c.test_new$diag1.sick != y_hat_test))/length(y_hat_test)
  test_err 
  test_err_store <- c(test_err_store, test_err)
}
train_err_store
test_err_store
#####################################################################
#split into train and testing
sample.cleveland <- floor(0.80 * nrow(cleveland))
set.seed(155)
train_ind <- sample(seq_len(nrow(cleveland)),size =sample.cleveland)
c.train <- cleveland[train_ind,]
c.test <- cleveland[-train_ind,]
y_true <- as.numeric(c.test$diag1)-1

#CART
model.control <- rpart.control(minsplit = 5, xval = 10, cp=0)
fit <- rpart(diag1~., data=c.train, method = "class", control=model.control)
#pruned tree
x11()
plot(fit, branch = .5, compress=T,main = " Tree")
text(fit, cex = 1)
#prune the tree
min_cp = which.min(fit$cptable[,4])
min_cp #2
pruned_fit <- prune(fit, cp=fit$cptable[min_cp,1]) 
#pruned tree
x11()
plot(pruned_fit, branch = .5, compress=T,main = "Pruned Tree")
text(pruned_fit, cex = 1)

# Compute test error for a single tree
my_pred <- predict(pruned_fit, newdata = c.test, type = "class")
y_hat <- as.numeric(my_pred)-1
misclass_tree <- sum(abs(y_true - y_hat))/length(y_hat)
misclass_tree #26.67%

#random forest
c.rf.model <- randomForest(diag1~., data = c.train, ntree = 1000)
c.rf.model #OOB 19.92%

#Compute test error for RF
y_hat <- predict(c.rf.model, newdata = c.test, type = "response")
y_hat <- as.numeric(y_hat)-1
misclass_rf <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_rf # 13.33%
