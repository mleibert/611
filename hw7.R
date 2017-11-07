a=4;b=7
 


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


x=c(.12,.17,.32,.56,.98,1.03,1.1,1.18,1.23,1.67,1.68,2.33)

gammamixEM(x, lambda = NULL, alpha = NULL, beta = NULL, 
epsilon = 1e-08, maxit = 1000, maxrestarts=20,
verb = FALSE)


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


