integrand <- function(x) {(exp(-x)) / (1+x^2 ) }

integrate(integrand ,0,1)



n=10000;f<-rep(0,n);g<-rep(0,n)
for ( i in 1:n) {  U<-runif(1);	f[i]<- integrand(U) 	 }
barhn<-sum(f)/n;barhn
