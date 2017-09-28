integrand <- function(x) {(exp(-x)) / (1+x^2 ) }

integrate(integrand ,0,1)

TF=rep(0,1000)
for (j in 1:1000) {

n=10000;f<-rep(0,n)
for ( i in 1:n) {  U<-runif(1);	f[i]<- integrand(U) 	 }
fbarhn<-sum(f)/n;fbarhn

I2<-function(G) { (1-exp(-1))/(1+G^2) }
g<-rep(0,n)
for ( i in 1:n) { U<-runif(1); G=-log(-U*(1-exp(-1))+1 ); g[i]<-I2(G)}
gbarhn<-sum(g)/n;gbarhn



if( sqrt((1/n^2) * sum( (f-barhn)^2 ))>
sqrt((1/n^2) * sum( (g-barhn)^2 )) ) {TF[j]<-1}

}


for (i in 1:n){	
U<-runif(1);V=1-U;	.5* (exp(U) + exp(V) )	