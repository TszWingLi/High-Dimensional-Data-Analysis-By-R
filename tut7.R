xg<-c(214.97,141.52)
xc<-c(214.82,139.45)
sg<-matrix(c(0.1502,0.0055,0.0055,0.1998),2,2,byrow=T)
sc<-matrix(c(0.124,0.0116,0.0116,0.3112),2,2,byrow=T)
n1<-500
n2<-500

spool<- ((n1-1)*sg + (n2-1)*sc)/(n1+n2-2)
invspool <- solve(spool)

(xg-xc) %*% invspool
-0.5 * t(xg-xc) %*% invspool %*% (xg+xc)

x0<-c(214,140.4)
(xg-xc) %*% invspool %*% x0 -0.5 * t(xg-xc) %*% invspool %*% (xg+xc)