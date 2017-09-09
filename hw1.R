n=10000
dev.off()

U<-runif(n)
X= -log((1-U)/U)
Y<-rlogis(n)
par(mfrow=c(1,2))
hist(X);hist(Y)


 
options(scipen=999)

U<-runif(n)
X = tan( pi*U-pi/2 )
Y<-rcauchy(n,location=1,scale=1)
par(mfrow=c(1,2))
hist(X,xlim=c(-41,41));hist(Y,xlim=c(-41,41))

qplot(Y, geom="histogram",  xlim=c(-20,20)     ) 
qplot(X, geom="histogram",  xlim=c(-20,20)     ) 

