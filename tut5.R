setwd("C:/Users/cyrus/iCloudDrive/Poly/AMA4602/AMA4602_Rfiles")
euclidean <- function(a, b) sqrt(sum((a - b)^2))

data("iris")
km<-kmeans(iris[,1:4],centers = 3,nstart = 25)
clust<-km$cluster #kmeans method

n<-nrow(iris)
S<- cov(iris[,1:4]) * (n-1)/n
e <- eigen(S)
e$vectors

xtilde<-apply(iris[,1:4],2, function(x)x-mean(x))
pc<-prcomp(iris[,1:4])
pc1<-xtilde %*% e$vectors[,1]
pc2<-xtilde %*% e$vectors[,2]# pc

# or
pc <- prcomp(iris[,1:4])
plot(pc$x[,1],pc$x[,2], col=km$cluster, xlab="pc1", ylab="pc2")
legend("bottomright", paste("Cluster",1:3), col=1:3, pch = 1)
table(iris[,5], km$cluster)

#c
ss<-vector(length=10)
for(i in 1:10){
  set.seed(0)
  ss[i] <- kmeans(iris[,1:4], center=i, nstart = 25)$tot.withinss
}
plot(1:10, ss, type="b", xlab = "Number of clusters", ylab = "Sums of squares")
