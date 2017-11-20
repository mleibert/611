a=4;b=7
N=5000
X=rep(NA,N)
X[1]<-rgamma(1,a,b)

acc<-rep(0,N)

for ( i in 2:N ){
	Y=rgamma(1,a,b)
			rho=	(dgamma(Y,4.3,6.2)/dgamma(X[i-1],4.3,6.2))*
				(dgamma(X[i-1],a,b)/dgamma(Y,a,b))
	if( runif(1) < rho){X[i]=Y;acc[i]<-1} else {X[i] = X[i-1] }}

mean(X)
mean(rgamma(11111,4.3,6.2))
sum(acc)/(N)

length(unique(X))/5000

plot(X, type="l", ylab="p")


a=5;b=6


N=5000
X=rep(NA,N)
X[1]<-rgamma(1,a,b)
X[1]
acc<-rep(0,N)

for ( i in 2:N ){
	Y=rgamma(1,a,b)
			rho=	(dgamma(Y,4.3,6.2)/dgamma(X[i-1],4.3,6.2))*
				(dgamma(X[i-1],a,b)/dgamma(Y,a,b))
	if( runif(1) < rho){X[i]=Y;acc[i]<-1} else {X[i] = X[i-1] }}

mean(X)
mean(rgamma(11111,4.3,6.2))
sum(acc)/N
length(unique(X))/5000

sink("foo.txt",append=F,split=T)

x=c(.12,.17,.32,.56,.98,1.03,1.1,1.18,1.23,1.67,1.68,2.33)

gammamixEM(x, lambda = NULL, alpha = c(1,1), beta = NULL, 
epsilon = 1e-08, maxit = 1000, maxrestarts=20,verb = FALSE)
sink()

\verbatiminput{output.txt}




N=5000
X=rep(NA,N)
X[1]<-1
X[1]
acc<-rep(0,N)

for ( i in 2:N ){
	Y=rgamma(1,a,b)
			rho=	(dgamma(Y,4.3,6.2)/dgamma(X[i-1],4.3,6.2))*
				(dgamma(X[i-1],a,b)/dgamma(Y,a,b))
	if( runif(1) < rho){X[i]=Y;acc[i]<-1} else {X[i] = X[i-1] }}

mean(X)
mean(rgamma(11111,4.3,6.2))
sum(acc)/N
length(unique(X))/5000


 
 rm(list = ls())
n=13;It=10000


MH<-function(n,It){
	nc<-function( M ){ gamma(M) /  ( gamma((M-1)/2) *  gamma((M-1)/2))  }
	Expmedian<<-function(Z ){( nc(n)* exp(-Z) * (1-exp(-Z))^((n-1)/2)	 * 
				exp(-Z *((n-1)/2) )  )}

	X<-acc<-rep(0,It)
	X[1]<-1
	
	
	for (i in 2:10000) {
		Y <- rexp(1, rate = X[i-1] )

		rho<-	(Expmedian(Y) / Expmedian(X[i-1] ))*
			(dexp( X[i-1] ,rate=Y) / dexp( Y ,rate=X[i-1] ) )

		if( runif(1) < rho){X[i]=Y;acc[i]<-1} else {X[i] = X[i-1] } }
	
	X<<-X
	print(X);print(paste("acceptance rate:", mean(acc) ,sep=" ") )
	print(summary(X));print(summary(acc))}

MH(13,10000)
par(mfrow=c(1,2))
(plot(density( X ))); (curve(Expmedian, from=0, to=2.5))	





