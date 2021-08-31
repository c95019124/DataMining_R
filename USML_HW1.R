## Yiling Chen
## Created: February 4, 2021 
## HW#1
#####################################################################
#1
#####################################################################
rm(list = ls())

#set working directory
##1a~b
setwd("/Users/elenachen/Desktop/DataMiningII/Hw1")
library(ISLR)
summary(College)
View(College)
pairs(College[2:18])

##1c~e
Elite = rep("No",nrow(College))
Elite[College$Top10perc > 50] ="Yes"
Elite = as.factor(Elite)
College = data.frame(College, Elite)
View(College)
table(College$Elite)
#78Elite schools
table(College$Elite,College$Private)
#67/78 Elite schools are private

##1f
EliteYes <- subset(College, College$Elite == 'Yes')
View(EliteYes)
result.meanYes <- mean(EliteYes$Grad.Rate)
print(result.meanYes)
#83.38
EliteNo <- subset(College, College$Elite == "No")
result.meanNo <- mean(EliteNo$Grad.Rate)
print(result.meanNo)
#63.46

#####################################################################
#2
#####################################################################
library(ISLR)
library(corrplot)
library(dplyr)
View(Auto)
#2a~b
y <- c(Auto)
is.na(y)
sapply(Auto, class)
summary(Auto)
continous <- sapply(Auto, is.numeric)

#2c
twoC<-sapply(Auto[,continous], function(x) signif(c(mean(x),sd(x)),2))
rownames(twoC) <- c("Mean","SD")
twoC

#2d
newset <-sapply(Auto[-5:-55,continous], function(x) signif(c(range(x),mean(x),sd(x)),2))
rownames(newset) <- c("Min","Max","Mean","SD")
newset

#2e
df <-Auto
df$origin <- as.character(df$origin)
df[df == "1"] <-"American"
df[df == "2"] <-"European"
df[df == "3"] <-"Japanese"
df = subset(df, select=-c(name))
#View(df)
head(df)

#2f
pairs(Auto[1:7])

auto <- as.matrix(Auto[1:7])
corralation <-cor(auto)
corrplot(corralation,method = "number")

#2g
twog <-sapply(Auto[,continous], function(x) signif(c(range(x),mean(x),sd(x)),2))
rownames(twog) <- c("Min","Max","Mean","SD")
twog
#22low,35med,48high
my_mpg <- rep("low",nrow(Auto))
my_mpg[Auto$mpg >22] = "med"
my_mpg[Auto$mpg >35] = "High"
my_mpg <- as.factor(my_mpg)
new_Auto <-data.frame(Auto,my_mpg)
#View(new_Auto)
#saveRDS(new_Auto,"new_Auto.rds")
