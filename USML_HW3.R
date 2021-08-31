## Yiling Chen
## Created: March 22, 2021 
## HW#3
#####################################################################
#2
#####################################################################
rm(list = ls())
#2a sketch the dendrogram
observation <- c(0, 0.3, 0.4, 0.7,
                 0.3, 0, 0.5, 0.8,
                 0.4, 0.5, 0, 0.45,
                 0.7, 0.8, 0.45, 0)
dis_matrix <- as.dist(matrix(observation,nrow = 4))
plot(hclust(dis_matrix))

#2b repeat but use simple linkage clustering
plot(hclust(dis_matrix, method = "single"))

#2e dendrogram two or more of the leaves are repositioned
plot(hclust(dis_matrix), labels = c(2,1,4,3))

#####################################################################
#3
#####################################################################
#install.packages("fossil")
#install.packages("cluster")
library(cluster)
library(fossil)
#3a. Generate a simulated dataset
set.seed(1234)
data <- rbind(matrix(rnorm(20*50, mean = 0), nrow = 20),
              matrix(rnorm(20*50, mean=3), nrow = 20),
              matrix(rnorm(20*50, mean=5), nrow = 20))
classes <- c(rep(1,20),rep(2,20),rep(3,20))
plot(data)

#3b. Perform k-means clustering of the observations with k=3, assess performance
kmean <- kmeans(data, 3, nstart = 20)
table(classes,kmean$cluster)

pr.out <-prcomp(data)
plot(pr.out$x[,c(1,2)],col=kmean$cluster)

rand.index(kmean$cluster,classes)
adj.rand.index(kmean$cluster,classes)

#3c. Use silhouette plots select the optimal number of clusters
idx <- sample(c(1:length(data[,1])), 60)
dat_red <- data[idx, ]
d <- dist(dat_red)
hc <- hclust(d, method = "ave")
ct <- cutree(hc, k = 3)
si <- silhouette(ct, dist = d)
plot(si)

#3d. Use the gap statistics select the optimal number of clusters

gap_kmeans <- clusGap(data, kmeans, nstart = 20, K.max = 10, B = 100)
plot(gap_kmeans, main = "Gap Statistic: kmeans")

#gap_kmed <- clusGap(data, pam, K.max = 10, B = 100)
#plot(gap_kmed, main = "Gap Statistic: kmedoids")

