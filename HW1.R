#################################################################################
##Yiling Chen
##Sep.12,2020
##EAS506
##HW1
###############################################################################
# Question 1, pre-process data
# install some packages
install.packages("ISLR")
library("ISLR")
names(Auto)
print(Auto)

#check missing data
sum(is.na(Auto))

#get a brief idea
summary(Auto)
plot(Auto)
#?Auto

#check to see if there is an outlier
boxplot(Auto$cylinders)

par(mfrow = c(2,4))

#graphs with mpg as Y
plot(mpg ~ year , data = Auto, main = "mpg vs year")
abline(lm(mpg ~ year , data = Auto), col = "red") 

plot(mpg ~ cylinders , data = Auto, main = "mpg vs cylinders")
abline(lm(mpg ~ cylinders , data = Auto), col="red")

plot(mpg ~ displacement , data = Auto, main = "mpg vs displacement")
abline(lm(mpg ~ displacement , data = Auto), col="red")

plot(mpg ~ horsepower , data = Auto, main = "mpg vs horsepower")
abline(lm(mpg ~ horsepower , data = Auto), col="red")

plot(mpg ~ weight , data = Auto, main = "mpg vs weight")
abline(lm(mpg ~ weight , data = Auto), col="red")

plot(mpg ~ acceleration , data = Auto, main = "mpg vs acceleraton")
abline(lm(mpg ~ acceleration , data = Auto), col="red")

plot(mpg ~ origin , data = Auto, main = "mpg vs origin")
abline(lm(mpg ~ origin , data = Auto), col="red") 
#write up: I started with download the dataset, check to see if there is any missing data and getting a breif idea of what the dataset contains. Then, I picked a variable (cylinder) to see if there is an outlier. I acheive this by using a boxplot. Fortunatly, the variable cylinder doesn't have any outlier. Finally, I preprocess the data Auto by using abline graphs and use par(mfrow) to put the graphs together as a 2x4 picture that contains all seven graphs.

#################################################################################
# Question2a
#identify the most significant coefficient
lm.relation <- lm(mpg ~ weight + cylinders + year + displacement + origin + acceleration, data = Auto)
summary(lm.relation)

#write up: The most significant coeefficients  that affect the mpg are "weight", "year", and "origin". This is indeed the case when we look at the plot as well. In most cases(except for some outliers), as the weight of the car increase, the mpg decreases. And for origin of the car, Japanese has a higher mpg than Germany and American cars. This can be related to the weight as well because usually Japanese cars are lighter than those of Germany and American. Finally, it seems like if the car is newer model, it also tend to have a higher mpg than previous years cars. And that is not suprising because as technology level increases, the auto companies can make faster accelerated cars.
#################################################################################
#Question2b
#write up: The coefficient variable for "year" suggests that as the newer the car is, the higher the mpg will become. And the increasement of the mpg is .76 as the year increase by one year.

#Question2c
interaction = lm(mpg ~ .- name + weight:origin , data = Auto)
summary(interaction)

#write-up:  After joining the weight and origin, it seems like the weight and origin are statistically significant in compare with the mpg. It is not a surprised finding because the origin of the car determines the average weight of the cars (due to use of different materials), and the weight of the car is directly correlated with the mpg(speed of the car) base on our previous findings.
#################################################################################
#Question3
library(MASS)
data(Boston)

#check missing data
sum(is.na(Boston))

# breif idea of the data
names(Boston)
summary(Boston)
#?Boston
#################################################################################
#Question3a

par(mfrow = c(2,7))

#graphs with crim as Y
plot(crim ~  . , data = Boston)
abline(lm(crim ~ . , data = Boston), col = "red")

#pairwise scatterplot
pairs(Boston)

#write up: There are alot of information in the scatterplot. For example, the crime rate is very high in suburbs that has low zn(small land space), not bound to river, proportion of owner-occupied units built prior to 40s, property-tax is high.
#################################################################################
#Question3b, finding relation with per capita crime rate
lm.associated <- lm(crim ~zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data = Boston)
summary(lm.associated)
#write up: base on the the significant codes in the lm. The most important factors that affect the crime rate are dis(weighted mean of distances to five Boston employment centres) and rad(index of accessibility to radial highways).
#################################################################################
#Question3c
summary(Boston$crim)
summary(Boston$tax)
summary(Boston$ptratio)

#graph
library(ggplot2)
qplot(Boston$crim, binwidth = 5, xlab = "Crime Rate", ylab = "Suburbs")

library(ggplot2)
qplot(Boston$tax, binwidth = 5, xlab = "Tax", ylab = "Suburbs")

library(ggplot2)
qplot(Boston$ptratio, binwidth = 5, xlab = "pupil-teacher ratio by town", ylab = "Suburbs")

crimerate <- subset(Boston, crim > 20)
nrow(crimerate)/ nrow(Boston)
crimerate <- subset(Boston, crim > 80)
nrow(crimerate)/ nrow(Boston)

#Boston suburbs full-value property-tax rate per \$10,000
taxrate <- subset(Boston, tax > 650)
nrow(taxrate) / nrow(Boston)
taxrate <- subset(Boston, tax < 650)
nrow(taxrate) / nrow(Boston)

#Boston suburbs pupil-teacher ratio by town
tpratio <- subset(Boston,ptratio > 17.5)
nrow(tpratio) / nrow(Boston)
tpratio <- subset(Boston, ptratio < 17.5)
nrow(tpratio) / nrow(Boston)

# crime rate: Most of the suburbs in Boston doesn't seem like they have a high crime rate because only 4% of the neighborhood has a crime rate above 20%, however there is 0.2% of the suburbs has an alarming high crime rate higher or equal to 80%.
# tax: Base on the graph and calculation, 27% of the people in Boston suburbs pay more than $650 for tax, and the rest (the majority) pays less than $650.
# pupil-teacher ratio: For about more than 100 but less than 150 suburbs, pt-ratio is 17.5:1, and the rest(more than 300 suburbs) has a pt-ratio of 22.5:1. Therefore, the majority of the suburbs has a higher pt-ratio.
#################################################################################
# Question3d
# average more than 7rooms in dwelling
morethan7 <- subset(Boston, rm >7)
nrow(morethan7)

# average more than 8rooms in dwelling
morethan8 <- subset(Boston, rm >8)
nrow(morethan8)

# compare suburbs with more than 8rooms in dwelling with others
summary(morethan8)
summary(Boston)

# write-up: 64suburbs has average more than 7 rooms/dwelling and 13 suburbs has average more than 8 rooms/dwelling. 
# In compareson with the data as a whole,the suburbs that has average more than 8 rooms/dwelling generally has a bigger land, more closer to the river, the houses are built prior to 1940, and the resident there pays lower tax in general compare with other Boston suburbs.
#################################################################################
