options(scipen=999)
require(ggplot2)
source("http://peterhaschke.com/Code/multiplot.R") 

#3
n=10000

#Logistic
U<-runif(n)
X= -log((1-U)/U)
Y<-rlogis(n)
dat<-data.frame(X,Y)
multiplot(qplot(dat$X, geom="histogram",col=I("#decbe4"),  
	fill=I("#fbb4ae"),xlab="Logistic from uniform") , 
		qplot(dat$Y, xlab="Logistic from R" ,geom="histogram", 
			col=I("#ccebc5"),  fill=I("#b3cde3")) ,cols=2)


#Cauchy
U<-runif(n)
X = tan( pi*U-pi/2 )
Y<-rcauchy(n,location=1,scale=1)
par(mfrow=c(1,2))
CHY<-data.frame(X,Y)
hist(X,xlim=c(-41,41));hist(Y,xlim=c(-41,41))
p<-ggplot(CHY, aes(X)) +  geom_bar(alpha = 0.1  ,fill = "green" ) 
qplot(CHY$X, geom="histogram",col=I("blue"),  fill=I("red"),alpha=.5) 


qplot(Y, geom="histogram",  xlim=c(-20,20)     ) 
qplot(X, geom="histogram",  xlim=c(-20,20)     ) 

#4
n=1000
U<-runif(n);V<-runif(n)
X <- sqrt( -2*log(U) ) * cos( 2*pi* V )
Y <- sqrt( -2*log(U) ) * sin( 2*pi* V )
X
BM<-data.frame(X,Y)
ggplot(BM, aes(Y)) +	  geom_density()
p<-ggplot(BM, aes(X)) +  geom_density(alpha = 0.1  ,fill = "green" ) 
p+ theme_bw()
q<-ggplot(BM, aes(X)) +  geom_density(alpha = 0.1  ,fill = "red" )
q + theme_bw()

#5

n=35
CLT<-rep(NA,100)

for( i in 1:100) { 	CLT[i]<-mean(rbeta(n,2,5))	}
summary(CLT);sd(CLT)
mean(2/7);sqrt(((2*7)/((2+7)^2*(2+7+1)))/n)
hist(CLT)



