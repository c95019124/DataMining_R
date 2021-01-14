#####################
## HW2
## Yiling Chen
## Sept.26 2020
#####################
## install some packages
install.packages("caTools")
require(caTools)

##getting data information
setwd("/Users/elenachen/Desktop/EAS506/Week4/")
cereal <- read.csv("cereal.csv")
library(leaps)
summary(cereal)
nrow(cereal)
ncol(cereal)
head(cereal)
sum(is.na(cereal))
cereal$name <- NULL
##splitting the dataset for train and test
set.seed(123)
sample = sample.split(cereal,SplitRatio = 0.80)

train1 = subset(cereal, sample ==TRUE)
test1 = subset(cereal, sample == FALSE)
nrow(train1)
nrow(test1)
head(cereal)
##fit linear model, multiple regression
full_model <- lm(rating ~ ., data = train1)
summary(full_model)

##report MSE
y_pred = predict(full_model, newdata = test1)
y_true = test1$rating
y_pred
y_true
test1_error <- mean((y_pred - y_true)^2) 
test1_error
##forward subset selection
full_model.fwd <- regsubsets(rating ~ ., data = train1, nbest = 1, nvmax = ncol(train1) - 1, method = "forward")
summary(full_model.fwd)

my_sum <- summary(full_model.fwd)

par(mfrow = c(2,2))
names(my_sum)
plot(my_sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(my_sum$bic,  xlab = "Number of Variables", ylab = "BIC", type = "l")

plot(my_sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(my_sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")
##Finding the best model
which(my_sum$cp == min(my_sum$cp))
which(my_sum$bic == min(my_sum$bic))

##exhaustive subset selection
full_model.exhaustive <- regsubsets(rating ~ ., data = train1, nbest = 1, nvmax = ncol(train1) - 1, method = "exhaustive")
summary(full_model.exhaustive)

my_sum1 <- summary(full_model.exhaustive)

par(mfrow = c(2,2))
names(my_sum1)
plot(my_sum1$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(my_sum1$bic,  xlab = "Number of Variables", ylab = "BIC", type = "l")

plot(my_sum1$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(my_sum1$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

##Finding the best model
which(my_sum1$cp == min(my_sum1$cp))
which(my_sum1$bic == min(my_sum1$bic))


#####################################
##Question 2
##loading and processing data
load("zip.test.RData")
load("zip.train.RData")
install.packages("class")
install.packages("dplyr")
library("dplyr")
library("class")
library("ggplot2")
test <- as.data.frame(zip.test)
train <-as.data.frame(zip.train)
summary(train)
head(train)
im <- matrix(as.numeric(train[2,2:257]), nrow = 16, ncol = 16)
image(t(apply(-im,1,rev)),col=gray((0:32)/32))
nrow(train)
nrow(test)


#Consider only the 2s and 3s for both train and test dataset
train_new = filter(train, (V1 == 2) | (V1 == 3 ))
table(train_new$V1)

test_new = filter(test, (V1 == 2) | (V1 == 3 ))
table(test_new$V1)

View(train)

train_lr <- train_new
test_lr <- test_new
train_lr$V1 <- ifelse(train_lr$V1 == 3, 1, 0)
test_lr$V1 <- ifelse(test_lr$V1 == 3, 1, 0)

#Classification by linear regression
library(base)
lr <- lm(V1 ~ ., data = train_lr)
summary(lr)
y_pred_train <- predict(lr, newdata = train_lr)
y_pred_train <- ifelse(y_pred_train >0.5,1, 0)
mean(y_pred_train != train_lr$V1)

y_pred_test <- predict(lr, newdata = test_lr)
y_pred_test <- ifelse(y_pred_test >0.5,1, 0)
mean(y_pred_test != test_lr$V1)


#Repeat this for different values of K as in the question
require(class)
#Separating V1 from V2-V257
train_number <- train_new$V1
test_number <- test_new$V1
train_new$V1 <- NULL
test_new$V1 <- NULL
#KNN to get training error for k = 1
knn.1.train =  knn (train=train_new, test=train_new, cl=train_number, k=1)
#Training error for k = 1
100 * sum(train_number != knn.1.train)/nrow(train_new)
#KNN to get testing error for k = 1
knn.1.test =  knn(train=train_new, test=test_new, cl=train_number, k=1)
#Testing error for k = 1
100 * sum(test_number != knn.1.test)/nrow(test_new)

#KNN to get training error for k = 3
knn.3.train =  knn(train=train_new, test=train_new, cl=train_number, k=3)
#Training error for k = 3
100 * sum(train_number != knn.3.train)/nrow(train_new)
#KNN to get testing error for k = 3
knn.3.test =  knn(train=train_new, test=test_new, cl=train_number, k=3)
#Testing error for k = 3
100 * sum(test_number != knn.3.test)/nrow(test_new)

#KNN to get training error for k = 5
knn.5.train =  knn(train=train_new, test=train_new, cl=train_number, k=5)
#Training error for k = 5
100 * sum(train_number != knn.5.train)/nrow(train_new)
#KNN to get testing error for k = 5
knn.5.test =  knn(train=train_new, test=test_new, cl=train_number, k=5)
#Testing error for k = 5
100 * sum(test_number != knn.5.test)/nrow(test_new)

#KNN to get training error for k = 7
knn.7.train =  knn(train=train_new, test=train_new, cl=train_number, k=7)
#Training error for k = 7
100 * sum(train_number != knn.7.train)/nrow(train_new)
#KNN to get testing error for k = 7
knn.7.test =  knn(train=train_new, test=test_new, cl=train_number, k=7)
#Testing error for k = 7
100 * sum(test_number != knn.7.test)/nrow(test_new)

#KNN to get training error for k = 9
knn.9.train =  knn(train=train_new, test=train_new, cl=train_number, k=9)
#Training error for k = 9
100 * sum(train_number != knn.9.train)/nrow(train_new)
#KNN to get testing error for k = 9
knn.9.test =  knn(train=train_new, test=test_new, cl=train_number, k=9)
#Testing error for k = 9
100 * sum(test_number != knn.9.test)/nrow(test_new)

#KNN to get training error for k = 11
knn.11.train =  knn(train=train_new, test=train_new, cl=train_number, k=11)
#Training error for k = 11
100 * sum(train_number != knn.11.train)/nrow(train_new)
#KNN to get testing error for k = 11
knn.11.test =  knn(train=train_new, test=test_new, cl=train_number, k=11)
#Testing error for k = 11
100 * sum(test_number != knn.11.test)/nrow(test_new)

#KNN to get training error for k = 13
knn.13.train =  knn(train=train_new, test=train_new, cl=train_number, k=13)
#Training error for k = 13
100 * sum(train_number != knn.13.train)/nrow(train_new)
#KNN to get testing error for k = 13
knn.13.test =  knn(train=train_new, test=test_new, cl=train_number, k=13)
#Testing error for k = 13
100 * sum(test_number != knn.13.test)/nrow(test_new)

#KNN to get training error for k = 15
knn.15.train =  knn(train=train_new, test=train_new, cl=train_number, k=15)
#Training error for k = 15
100 * sum(train_number != knn.15.train)/nrow(train_new)
#KNN to get testing error for k = 15
knn.15.test =  knn(train=train_new, test=test_new, cl=train_number, k=15)
#Testing error for k = 15
100 * sum(test_number != knn.15.test)/nrow(test_new)


#################################
##Question3
#################################
library("ISLR")
data(College)
college <- College
summary(college)
head(college)

##splitting the dataset for train and test
set.seed(456)
sample = sample.split(college,SplitRatio = 0.80)

train_college <- subset(college, sample ==TRUE)
test_college <- subset(college, sample == FALSE)
nrow(train2)
nrow(test2)
View(college)

##using least squares on training set and report the test error
college.fit <- lm( Apps ~., data=train_college)
college.pred <- predict(college.fit, test_college)
mean((test_college[,"Apps"] - college.pred)^2)


