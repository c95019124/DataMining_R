## Yiling Chen
## Created: February 26, 2021 
## HW#2
#####################################################################
#1
#####################################################################
rm(list = ls())
setwd("/Users/elenachen/Desktop/DataMiningII/Hw2")
load("marketing.RData")

sum(is.na(marketing))
summary(marketing)
head(marketing)
View(marketing)
#remove missing value
marketing <- na.omit(marketing)
##generate a reference set
set.seed(1234)
n <- dim(marketing)[1]
ref_store <- c()
for (i in 1:14){
  variable <- marketing[ ,i]
  uni <- na.omit(unique(variable))
  temp <- sample(uni, n, replace = TRUE)
  ref_store <- cbind(ref_store, temp)
}
colnames(ref_store) <- colnames(marketing)[1:14]

##combine data and reference set
combo <- rbind(marketing[,1:14], ref_store)
dim(combo)

## create a response variable
Y_dats <- rep(1, n)
Y_ref <- rep(0,n)
YY <- c(Y_dats, Y_ref)

##fit a classification tree
library(rpart)
model <- rpart(YY~., combo)
summary(model)

x11()
plot(model, branch = .5, compress=T,main = "Full Tree")
text(model, cex = .8)

predicted <- predict(model, combo)
predicted

#####################################################################
#2
#####################################################################
library(MASS)
data(Boston)
sum(is.na(Boston))
summary(Boston)
head(Boston)
View(Boston)

##2a visualize data using histograms
par(mfrow =c(3,5))
hist(Boston$medv, main = "Median Home value", col = "light grey")
hist(Boston$crim, main = "Crime", col = "light grey")
hist(Boston$zn, main = "Zone", col = "light grey")
hist(Boston$indus, main = "Industry", col = "light grey")
hist(Boston$chas, main = "Charles Indicator", col = "light grey")
hist(Boston$nox, main = "Notros Oxide", col = "light grey")
hist(Boston$rm, main = "Room", col = "light grey")
hist(Boston$age, main = "Age", col = "light grey")
hist(Boston$dis, main = "Dis", col = "light grey")
hist(Boston$rad, main = "Rad", col = "light grey")
hist(Boston$tax, main = "Tax", col = "light grey")
hist(Boston$ptratio, main = "PtRadio", col = "light grey")
hist(Boston$black, main = "Black", col = "light grey")
hist(Boston$lstat, main = "lstat", col = "light grey")

Boston[["medv"]]<-ordered(cut(Boston$medv,c(0,15,25,55)),labels=c("Low-medv","Middle-medv","High-medv"))
Boston[["crim"]]<-ordered(cut(Boston$crim,c(0,1.73,10.76,90)),labels=c("Low-crim","Mid-crim","High-crim"))
Boston[["zn"]]<-ordered(cut(Boston$zn,c(-0.1,20,70,101)),labels=c("low_zn","med_zn","high_zn"))
Boston[["indus"]]<-ordered(cut(Boston$indus,c(0,10,19.58,28)),labels=c("Low-indus","Mid-indus","High-indus"))
Boston[["chas"]]<-ordered(Boston$chas,labels=c("off-river","near-river"))
Boston[["nox"]]<-ordered(cut(Boston$nox,c(0,0.5,0.7,0.9)),labels=c("Low-nox","Mid-nox","High-nox"))
Boston[["rm"]]<-ordered(cut(Boston$rm,c(3,5,7,9)),labels=c("Small-house","Mid-house","Large-house"))
Boston[["age"]]<-ordered(cut(Boston$age,c(0, 45, 65, 100)),labels=c("Low_age", "Mid_age", "High_age"))
Boston[["dis"]]<-ordered(cut(Boston$dis,c(1, 4, 7,13)),labels=c("Low-dis","Mid-dis","High-dis"))
Boston[["rad"]]<-ordered(cut(Boston$rad,c(0, 3 , 8, 24)),labels=c("Low-index","Mid-index","High-index"))
Boston[["tax"]]<-ordered(cut(Boston$tax,c(180,300,500,720)),labels=c("Low-tax","Mid-tax","High-tax"))
Boston[["ptratio"]]<-ordered(cut(Boston$ptratio,c(12,17,20,23)),labels=c("Low-ptratio","Mid-ptratio","High-ptratio"))
Boston[["black"]]<-ordered(cut(Boston$black,c(0,100,300,400)),labels=c("Low-black","Mid-black","High-black"))
Boston[["lstat"]]<-ordered(cut(Boston$lstat,c(1,10,20,40)),labels=c("Low-lstat","Mid-lstat","High-lstat"))


#Convert to a binary incidence matrix
#install.packages("lmreg")
library(lmreg)
boston <- as(Boston, "transactions")
summary(boston)

quartz()
itemFrequencyPlot(boston, support = 0.08, cex.names = 0.8)

# Apply the apriori algorithm
rules  <- apriori(Boston, parameter = list(support = 0.01, confidence = 0.6))
summary(rules)
#2c
rules2c<-subset(rules,subset = lhs %in% "dis=Low-dis" & rhs %in% "crim=Low-crim" & lift>1.2)
rules2c #25917
inspect(head(sort(rules2c,by ="confidence"), n = 10))


#2d
rules2d<-subset(rules,subset = lhs %in% "ptratio=Low-ptratio" & lift>1.2)
rules2d #178721
inspect(head(sort(rules2d,by ="confidence"), n = 10))
##extra credit
linearmodel <- lm(ptratio ~ ., data = Boston)
linearmodel.sum <- summary(linearmodel)
linearmodel.sum
