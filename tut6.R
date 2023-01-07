library(dplyr)
data(mtcars)
distance_mat<- dist(mtcars, method = "euclidean")
#Single
hierarch_sl<- hclust(distance_mat,method = "single")
plot(hierarch_sl, cex=0.6, ylab="",xlab="", main="Single linkage", sub="")
#Complete
hierarch_sl<- hclust(distance_mat,method = "complete")
plot(hierarch_sl, cex=0.6, ylab="",xlab="", main="Complete linkage", sub="")
#Average
hierarch_sl<- hclust(distance_mat,method = "average")
plot(hierarch_sl, cex=0.6, ylab="",xlab="", main="Average linkage", sub="")


data("iris")
distance_mat <- dist(iris[,1:4],method = "euclidean")
hierarch_ga <- hclust(distance_mat, method = "average")
clust.ga<- cutree(hierarch_ga,3)
pc<- prcomp(iris[,1:4])
plot(pc$x[,1],pc$x[,2],col=clust.ga,xlab = "PC1", ylab = "PC2")
legend("bottomright",paste("Cluster",1:3),col=1:3,pch=1)