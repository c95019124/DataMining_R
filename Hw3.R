####################################
## Hw4
## Yiling Chen
## Oct.20 2020
###################################
##Q1
###################################
rm(list = ls())

#set working directory
setwd("/Users/elenachen/Desktop/EAS506/Wk8")
load("Diabetes.RData")

#library(pysch)
diabetes <-as.data.frame(Diabetes)
sum(is.na(diabetes))
summary(diabetes)
head(diabetes)
nrow(diabetes)
ncol(diabetes)
## pairwise scatterplots for all five variables
col <- c("dark red","light blue","dark green")[diabetes$group]
plot(diabetes,col = col)
cov(diabetes[,1:5])

normal <-subset(diabetes, diabetes$group == 'Normal' )
cov(normal[,1:5])
chem_diabetic <-subset(diabetes, diabetes$group == 'Chemical_Diabetic' )
cov(chem_diabetic[,1:5])
overt_diabetic <-subset(diabetes, diabetes$group == 'Overt_Diabetic' )
cov(overt_diabetic[,1:5])
##1b set train and test
library(caTools)
sample <- floor(.80 * nrow(diabetes))
set.seed(321)
trainset <- sample(seq_len(nrow(diabetes)), size = sample)
train <- diabetes[trainset,]
test <- diabetes[-trainset,]
dim(test)
dim(train)
##LDA
library(MASS)
library(ISLR)
group<-diabetes[,6]

lda.fit <-lda(group~.,data=train)
lda.pred.test <- predict(lda.fit, newdata = test)
y_hat_test <- as.numeric(lda.pred.test$class)
y_true_test <- as.numeric(test$group)
lda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)
lda_test_error #0.2069

##QDA
qda.fit <- qda(group~., data=train)
qda.pred.test <- predict(qda.fit, newdata = test)
y_hat_test_q <- as.numeric(qda.pred.test$class)
y_true_test_q <- as.numeric(test$group)
qda_test_error <- sum(abs(y_true_test_q - y_hat_test_q))/length(y_true_test_q)
qda_test_error #0.1379

##c) glucose68,insulin122,SSPG544,weight1.86,fasting plasma glucose184
##which class does LDA assign this person? How about QDA?
prediction<-data.frame(1.86,184,68,122,544,NA)
colnames(prediction)<-c("relwt","glufast","glutest","instest","sspg","group")

lda.pred.new<-predict(lda.fit,newdata = prediction)
lda.pred.new

qda.pred.new<-predict(qda.fit,newdata = prediction)
qda.pred.new

###################################
##Q2
###################################
library(ISLR)
require(corrplot)
library(corrplot)
data('Weekly')
weekly <- Weekly
sum(is.na(weekly)) #0
head(weekly)
nrow(weekly) #1089
ncol(weekly) #9
View(weekly)

##a) produce numeric and graphic plot, check to see if there is any pattern
a <-cor(weekly[,1:8])
corrplot (a, method = "color")
corrplot( a, method = "number")
summary(weekly)

##b) perform logistic regression and use summary to print out result
weekly.regression <- glm(Direction ~. - Year - Today, data = weekly, family = binomial)
summary(weekly.regression)

##c) confusion matrix
weekly.pred <-predict(weekly.regression, type ='response')
weekly_pred <-rep("Down", length(weekly.pred))
weekly_pred[weekly.pred > 0.5] <- "Up"
table(weekly_pred, weekly$Direction)
mean(weekly_pred == weekly$Direction) #0.561

##d) fit model using train data from 90 to 08 with Lag2 as predictor, confusion matrix
training <- weekly[weekly$Year <2009,]
testing <- weekly[weekly$Year >=2009,]
weekly.fit <- glm(Direction~Lag2, data=training, family=binomial)
pred.weekly <- predict(weekly.fit, newdata = testing, type = 'response')
pred_weekly <- rep("Down", length(pred.weekly))
pred_weekly[pred.weekly >0.5] <- "Up"
table(pred_weekly, testing$Direction)
mean(pred_weekly == testing$Direction) #0.625

##e)Use LDA
library(MASS)
lda_weekly <- lda(Direction ~ Lag2, data = training)
lda_pred <- predict(lda_weekly, newdata = testing)
lda.pred <- lda_pred$class
table(lda.pred, testing$Direction)
mean(lda.pred == testing$Direction) #0.625

##f) use KNN with K = 1
library(class)
pred.knn <- knn(Train <- data.frame(training$Lag2),
                Test <- data.frame(testing$Lag2),
                cl = training$Direction, k =1)
table(pred.knn, testing$Direction)
mean(pred.knn == testing$Direction) #0.5
