#############################################
## Yiling Chen
## HW#4
## 4/18/21

#############################################
#Q2
#############################################
rm(list = ls())
setwd("/Users/elenachen/Desktop/DataMiningII/Hw4")
#install.packages("dplyr")
library("dplyr")
library("knitr")
library("tidyverse")
library("vcd")
titanic <-read.csv("titanic.csv")

sum(is.na(titanic))
head(titanic)
nrow(titanic)
ncol(titanic)
View(titanic)
plot(titanic)
table(titanic$Sex) #female314 #male573
table(titanic$Sex, titanic$Survived)

hist(titanic$Age, main = "Distribution of Age on Titanic", col = "light blue")
new_titanic <- cbind(titanic, new_col = titanic$Age) 
new_titanic$Age[titanic$Age<18] <- "Child"
new_titanic$Age[titanic$Age>=18] <- "Adult"
new_titanic$Age[titanic$Age>60] <- "Elder"
table(new_titanic$Survived, new_titanic$Age,new_titanic$Pclass, new_titanic$Sex)
round(prop.table(table(new_titanic$Survived)),digits =2)
table(new_titanic$Age)
new_titanic <- new_titanic %>%
  mutate(Survived = case_when(Survived==1 ~ "Yes",
                              Survived==0 ~ "No"))
summary <- new_titanic %>%
  select(Name,Survived) %>%
  group_by(Survived) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))

survrate <- summary$freq[summary$Survived=="Yes"]
kable(summary)

prop.table(table(new_titanic$Survived,new_titanic$Pclass,new_titanic$Sex))


ggplot(new_titanic,aes(Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = 2) +
  ylab("Survival Rate") +
  ggtitle("Survival Rate by Class") +
  theme_dark()
  
ggplot(new_titanic,aes(Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = 2) +
  ylab("Survival Rate") +
  ggtitle("Survival Rate by Sex") +
  theme_dark()

ggplot(new_titanic,aes(Age, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = 2) +
  ylab("Survival Rate") +
  ggtitle("Survival Rate by Age") +
  theme_dark()
##mosaic plot
mosaic_total <- new_titanic %>%
  select(Survived, Pclass, Sex, Age) %>%
  mutate_all(as.factor)
mosaic(~Pclass+Sex+Survived+Age, data=mosaic_total, shade= TRUE, legend=TRUE)

##result of movie "Titanic",Rose was 17, Jack was 20

#############################################
#Q4
#############################################
rm(list = ls())
setwd("/Users/elenachen/Desktop/DataMiningII/Hw4")
#install.packages("recommenderlab")
library(recommenderlab)
data(MovieLense)
head(MovieLenseMeta)
nrow(MovieLenseMeta)
ncol(MovieLenseMeta)
head(MovieLenseUser)
nrow(MovieLenseUser)
ncol(MovieLenseUser)
sum(is.na(MovieLenseUser)) #0
sum(is.na(MovieLenseMeta)) #1


hist(rowCounts(MovieLense)) #num of ratings per user
hist(colCounts(MovieLense)) #num of rating per movie
mean(rowMeans(MovieLense)) #avg rating for all users

dim(MovieLense)
dim(getRatingMatrix(MovieLense))
normalizeML <- normalize(MovieLense)
normalizeML

image(normalizeML[1:50,1:50],main = "Normalized ratings")
image(MovieLense[1:50, 1:50], main = "Raw Ratings")
getRatingMatrix(normalizeML)[1:10, 1:10]
#de-normalize
R_denormalize <- denormalize(normalizeML)
getRatingMatrix(R_denormalize)[1:10, 1:10]

######################################
## Create a recommender system
######################################
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_ubcf <- Recommender(MovieLense, method = "UBCF")

# Create top 5 recommendations for 3 users
recom <- predict(recommender_ubcf, MovieLense[500:502], n=5)
recom
as(recom, "list")

# predict rating for three users
predict_ratings <- predict(recommender_ubcf, MovieLense[500:502], type = "ratingMatrix")
predict_ratings
as(predict_ratings, "matrix")[,60:65]

####################################
#Q5
######################
set.seed(188)
scheme <-evaluationScheme(MovieLense,method = 'cross',train=.75,
                          given = 5, goodRating = 4, k = 5)

model1 <-Recommender(getData(scheme,"train"),"UBCF")
model1_pred <- predict(model1, getData(scheme,"known"), type = "ratings")
ERROR <- rbind(UBCF = calcPredictionAccuracy(model1_pred, getData(scheme,"unknown")))
ERROR


result_1 <- evaluate(scheme,method = "UBCF", type="topNList", n=seq(10,200,20))
plot(result_1, annotate=TRUE)
plot(result_1, "prec/rec", annotate = TRUE)


