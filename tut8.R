setwd("C:/Users/cyrus/iCloudDrive/Poly/AMA4602/AMA4602_Rfiles")
#2a
diabetes <- read.csv("diabetes.csv")
Y <- diabetes[,9]
X <- diabetes[,1:8]
n <- length(Y)
nN <- sum(Y==0)
nP <- sum(Y==1)
xbarN <- apply(X[Y==0,],2,mean)
xbarP <- apply(X[Y==1,],2,mean)
SN <- cov(X[Y==0,])
SP <- cov(X[Y==1,])
Spool <- ((nN-1)*SN + (nP-1)*SP) / (nN+nP-2) 
z<-c(2,116,50,32,0,32.9,0.254,37)
a<-t(xbarN-xbarP)%*%solve(Spool)
b<- -0.5*t(xbarN-xbarP)%*%solve(Spool)%*%(xbarN+xbarP) - log((nP/n)/(nN/n))
a%*%z+b #>0 class 1
#2b
k <- 0.5*log(det(SN)/det(SP))+0.5*(t(xbarN)%*%solve(SN)%*%xbarN-t(xbarP)%*%solve(SP)%*%(xbarP))
-0.5*t(z)%*%(solve(SN)-solve(SP))%*%z + (t(xbarN)%*%solve(SN)-t(xbarP)%*%solve(SP))%*%z-k-log((nP/n)/(nN/n))

fit <- glm(Y~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age, family="binomial", data = diabetes)
z<- t(t(round(fit$coefficients,4)))
x0 <- c(2,116,50,32,0,32.9,0.254,37)
xbeta <- fit$coefficients[1] + x0 %*% fit$coefficients[-1]
exp(xbeta)/(1+exp(xbeta))