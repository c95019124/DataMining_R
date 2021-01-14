#######################################
# HW#5
# Yiling Chen
# November 9, 2020
#######################################
# Q1
#######################################
rm(list=ls())

#set working directory
setwd("/Users/elenachen/Desktop/EAS506/Wk11")
load("vehicle.RData")

library("rpart")
library(MASS)

vehicle <- as.data.frame(vehicle)
sum(is.na(vehicle))
summary(vehicle)
head(vehicle)
nrow(vehicle)
ncol(vehicle)
n.vehicle <- vehicle[1:19]
ncol(n.vehicle)
##set parameter
model.control <- rpart.control(minsplit = 5,xval = 10, cp = 0)
fit.vehicle <- rpart(classdigit~., data = n.vehicle, method = "class", control = model.control)
names(fit.vehicle)

##find points to prune
fit.vehicle$splits
fit.vehicle$cptable

x11()
plot(fit.vehicle$cptable[,4], main = "Cp for model selection", ylab = "Cp")
min_cp = which.min(fit.vehicle$cptable[,4])
#min_cp #11
pruned_fit_vehicle <- prune(fit.vehicle, cp = fit.vehicle$cptable[min_cp,1])

##pruned tree
x11()
plot(pruned_fit_vehicle, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_vehicle, cex = 1.3)

##full tree
x11()
plot(fit.vehicle, branch = .8, compress=T, main = "Full Tree")
text(fit.vehicle, cex = .8)

##########################################
Q2
##########################################
setwd("/Users/elenachen/Desktop/EAS506/Wk11")
load("prostate.RData")
library(glmnet)
library(leaps)
library(ISLR)
prostate <- as.data.frame(prostate)
prostate <- prostate[1:9]
sum(is.na(prostate))
summary(prostate)
head(prostate)
nrow(prostate) #97
ncol(prostate) #9
View(prostate)

##split data into train and test
sample <-floor(0.80 *nrow(prostate))
set.seed(111)
train_ind <- sample(seq_len(nrow(prostate)),size = sample)
train <- prostate[train_ind,]
test <- prostate[-train_ind,]
y.test <- test$lpsa
y.train <- train$lpsa

##Compute AIC and BIC
fit <- regsubsets(lpsa~.,data=train,nbest=1,nvmax=8,method="exhaustive")
reg_summary <-summary(fit)
names(reg_summary)
reg_summary$cp
reg_summary$bic
which.min(reg_summary$cp) #3 is the best
which.min(reg_summary$bic) #3 is the best

quartz()
plot(reg_summary$cp,type="o",lty=2,col = "dark blue",xlab = "num of variables",ylab="cp",main="AIC")

quartz()
plot(reg_summary$bic,type="o",lty=2,col = "dark red",xlab = "num of variables",ylab="BIC",main="BIC")

select <- reg_summary$outmat
hold <- which(select[8,]=="*")
training.a <-train[,c(0,hold)]

train.store.error <- c()
test.store.error <- c()
for(i in 1:8){
  hold<-which(select[i,]=="*")
  training.a<-train[,c(1,hold)]
  testing.a<-test[,c(1,hold)]
  fit.a<-lm(y.train~.,data=training.a)
  predict.train<-predict(fit.a,newdata=training.a)
  predict.test<-predict(fit.a,newdata=testing.a)
  train.error<-sum((predict.train-y.train)^2)*(1/length(y.train))
  test.error<-sum((predict.test-y.test)^2)*(1/length(y.test))
  
  train.store.error<-c(train.store.error,train.error)
  test.store.error<-c(test.store.error,test.error)
  
}
train.store.error
test.store.error
upper <- max(train.store.error, test.store.error)
lower <- min(train.store.error, test.store.error)

holding <- which(select[3,]=="*")
ab.train <- train[,c(0,holding)]
ab.test <- test[,c(0,holding)]
ab.fit <-lm(y.train~., data=ab.train)
aic <-AIC(ab.fit,k=3) #172.39
bic <-BIC(ab.fit) #179.11

quartz()
plot(train.store.error, type = "o", lty = 2, col = "blue", ylim = c(lower, upper) , xlab = "k", ylab = "error", main = "Error")
lines(test.store.error, type = "o", lty = 1, col = "red")
legend("topright", c("train", "test"), lty = c(2,1), col = c("blue", "red"))

##cross-validation where k=5, k=10
k5.store <- c()
k10.store <- c()
for(i in 1:8){
  hold<-which(select[i,]=="*")
  k.data<-prostate[,c(9,hold)]
  fitting<-glm(lpsa~.,data =k.data)
  k5_cv<-cv.glm(k.data, fitting,K=5)$delta[2]
  k10_cv<-cv.glm(k.data, fitting,K=10)$delta[2]
  k5.store<-c(k5.store,k5_cv)
  k10.store<-c(k10.store,k10_cv)
}
k5.store
k10.store

which.min(k5.store) #8 50.57%
which.min(k10.store) #3 51.21%
##graph K error
quartz()
plot(k5.store,type="o",lty=2,col = "dark blue",ylim = c(.4,.7),xlab = "k",ylab="error",main="K_Error")
lines(k10.store,type="o",lty=1,col="dark green")
legend("topright",c("k5.error", "k10.error"),lty=c(2,1),col=c("dark blue","dark green"))

## bootstrap.632
library(boot)
#install.packages("bootstrap")
library(bootstrap)

#create functions that feed into "bootpred"
boot_fit <-function(X,Y){lsfit(X,Y)}
boot_predict <- function(fit, X){cbind(1,X)%*%fit$coef}
sq_error <- function(Y,Yhat){(Y-Yhat)^2}

#create X and Y
X <- prostate[,1:8]
Y <- prostate[,9]

#generalize and search over the best possible subsets of size"k"

bs.store.error <-c()
for (i in 1:8){
  hold <- which(select[i,] =="*")
  res <- bootpred(X[,hold],Y, nboot = 50, theta.fit = boot_fit, theta.predict = boot_predict, err.meas = sq_error)
  bs.store.error <-c(bs.store.error,res[[3]])
}
bs.store.error #0.517 #3

quartz()
plot(bs.store.error, type="o",lty=3,col="dark red",main="bootstrap_error",xlab = "k",ylab="error")
legend("topright",c("bootstrap"),lty=1,col=c("dark red"))
###########################################
Q3
###########################################
#set working directory
setwd("/Users/elenachen/Desktop/EAS506/Wk11")
wine <- read.csv("conv_wine.csv")
library("rpart")
library(MASS)
sum(is.na(wine))
summary(wine)
head(wine)
View(wine)
nrow(wine) #178
ncol(wine) #11

##test and training
sample.wine <-floor(0.80 *nrow(wine))
set.seed(222)
train_ind_wine <- sample(seq_len(nrow(wine)),size = sample.wine)
train.wine <- wine[train_ind_wine,]
test.wine <- wine[-train_ind_wine,]
y_true <- as.numeric(test.wine$Y)-1
##grow and prune tree
model3.control <- rpart.control(minsplit = 5,xval = 10, cp = 0)
fit.wine <- rpart(Y~., data = train.wine, method = "class", control = model3.control)



names(fit.wine)
fit.wine$splits
fit.wine$cptable

#finding the best point to prune the tree
x11()
plot(fit.wine$cptable[,4], main = "Cp for model selection", ylab = "Cp")
min_cp = which.min(fit.wine$cptable[,4])
#min_cp #6
pruned_fit_wine <- prune(fit.wine, cp = fit.wine$cptable[min_cp,1])

##compute test error for this tree
my_pred <- predict(pruned_fit_wine, newdata = test.wine, type = "class")
y_hat <- as.numeric(my_pred)-1
misclass_tree <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_tree #.08333

# tree for wine dataset
x11()
plot(pruned_fit_wine, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_wine, cex = 1.3)

##Applying random forest/boosting
#install.packages("randomForest")
library(randomForest)
random.forest.fit <- randomForest(Y~., data = train.wine, n.tree = 10000)

x11()
varImpPlot(random.forest.fit)
importance(random.forest.fit)

y_hat <- predict(random.forest.fit, newdata = test.wine, type = "response")
y_hat <- as.numeric(y_hat)-1
misclass_rf <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_rf # 9.71%

##construct LDA model
reg.wine <- regsubsets(Y~.,data=train.wine,nbest=1,nvmax=8,method="exhaustive")
summary.wine <-summary(reg.wine)
lda_wine <- lda(Y~OD, data = train.wine)
lda_pred <- predict(lda_wine, newdata = test.wine)
lda.pred <- lda_pred$class
table(lda.pred, test.wine$Y)
mean(lda.pred == test.wine$Y) #58.33%
###########################################
Q4
##########################################
setwd("/Users/elenachen/Desktop/EAS506/Wk11")
load("covertype.RData")
covertype <- as.data.frame(covertype)
covertype <- covertype[,-c(11:54)]
sum(is.na(covertype)) #0
summary(covertype)
head(covertype)
View(covertype)
nrow(covertype) #581012
ncol(covertype) #12
names(covertype)

##divide into test and train
sample.covertype <-floor(0.80 *nrow(covertype))
set.seed(333)
train_ind_covertype <- sample(seq_len(nrow(covertype)),size = sample.covertype)
train.covertype <- covertype[train_ind_covertype,]
test.covertype <- covertype[-train_ind_covertype,]
y_true <- as.numeric(test.covertype$V55)-1
##grow and prune tree
model4.control <- rpart.control(minsplit = 5,xval = 10, cp = 0)
fit.covertype <- rpart(V55~., data = train.covertype, method = "class", control = model4.control)
names(fit.covertype)
fit.covertype$splits
fit.covertype$cptable

x11()
plot(fit.covertype$cptable[,4], main = "Cp for model selection", ylab = "Cp")
min_cp = which.min(fit.covertype$cptable[,4])
#min_cp #4
pruned_fit_covertype <- prune(fit.covertype, cp = fit.covertype$cptable[min_cp,1])

##pruned tree
x11()
plot(pruned_fit_covertype, branch = .2, compress=T, main = "Pruned Tree")
text(pruned_fit_covertype, cex = 1.1)

##compute test error for this tree
my_pred <- predict(pruned_fit_covertype, newdata = test.covertype, type = "class")
y_hat <- as.numeric(my_pred)-1
misclass_tree <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_tree #15%
