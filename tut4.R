#Q1
setwd("C:/Users/cyrus/iCloudDrive/Poly/AMA4602/AMA4602_Rfiles")
x<-read.csv("StockReturn.csv", header=T, row.names = 1)
sigma <- cov(x) * (nrow(x)-1)/nrow(x)
xmean<- colMeans(x)
xtilde<-matrix(NA,nrow = nrow(x),ncol=ncol(x))
for(i in 1:nrow(x)) xtilde[i,]<- as.numeric(x[i,]-xmean)
e<-eigen(sigma)
pc1<-xtilde%*%e$vectors[,1]
pc2<-xtilde%*%e$vectors[,2]
lambda <- e$values
alpha <- 0.05
CI.IB <- lambda[1]/(1+qnorm(1-alpha/2)*sqrt(2/n))
CI.UB <- lambda[1]/(1-qnorm(1-alpha/2)*sqrt(2/n))
c(CI.IB,CI.UB)

#Q2
library(IMIFA)