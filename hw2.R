options(scipen=999)

#1
r<-.5
m=1000;n=100
circle<-0
PI<-rep(0,m)

#plot(-.5:(1.5), -.5:1.5, type = "n") 
for(j in 1:m){
for (i in 1:n) {
	xy<-c( runif(2,0,1) )
	if ((xy[1]-r)^2+(xy[2]-r)^2 < r^2) {circle<-circle + 1; 
		points(xy[1],xy[2],col="red")  } else {
		 points(xy[1],xy[2],col="blue") } }
PI[j]<-4*(circle/1000) 
}

r<-.5
m=1000;n=100
circle<-0
PI<-rep(0,m)

for(j in 1:m){circle<-0
for (i in 1:n) {	xy<-c( runif(2,0,1) )
	if ((xy[1]-r)^2+(xy[2]-r)^2 < r^2) {circle<-circle + 1  } }
PI[j]<-4*(circle/n) }


hbarn<-sum(PI)/m;hbarn
1/m^2 * sum( (PI-hbarn)^2 )


#2 

#example
U=runif(10000,0,1)
hist( (4*sqrt(1-U^2) )  , xlim=c(0,6))
sum( (4*sqrt(1-U^2) ) /10000 )




integrand <- function(x) {exp((-4*x)/3)*x^3 }
plot(integrand,xlim=c(0,15))
integrate(integrand  ,0, Inf )

n=10000
h<-rep(0,n)
for ( i in 1:n) {  U<-runif(1);	
h[i]<- exp((-4+4*U)/(3*U)) * ((1-U)/U)^3 * (1/U)^2;  }

barhn<-sum(h)/n;barhn

round(h,5)

sum( (h-barhn)^2 )/(n^2)

((4/3)^4)/gamma(4)


#3
	
n=10000;h<-rep(0,n)
for ( i in 1:n){ x<-rnorm(1,mean=0,sd=2); h[i]<-exp(-x^2) } 
sum(h)/n 

1/(sqrt(2*4+1))

#5


#need while loop

accepted<-rep(0,10000);c=1; 
while (  0 %in% accepted ) {  y=runif(1) ;
  if( runif(1) <= 4* y*(1-y)) { accepted[c]<-y;c<-c+1} else {next} }

dat<-as.data.frame(as.table(quantile(accepted, probs = seq(0, 1, .01))))
colnames(dat)<-c("percentile","value")
dat$beta<-(qbeta(seq(0, 1, .01), 2, 2, ))
dat<-rbind(dat, dat[nrow(dat),] )
cbind(dat[1:51,],dat[52:nrow(dat),])

hist(accepted,prob = TRUE)
curve(dbeta(x,2,2), col="darkblue", lwd=2, add=TRUE, yaxt="n")

plot(dat$beta,dat$value,xlab = 
	'Theoretical Quantiles from Beta Distribution', 
	ylab = "Accept-Reject Quantiles ");abline(0,1,col="red")
cor(dat$beta,dat$value)


#6
f1<-function(x) {   ( (1/(sqrt(2*pi))) * exp(-.5*x^2) )/(.5*exp(-abs(x))) } 
require("rmutil")
accepted<-rep(0,1000);c=1 
laplace<-scan("laplace")

while (  0 %in% accepted ) {  y=rlaplace(1000, m=0, s=1 );
  if( runif(1) <= (1/(sqrt(2*pi))* exp(-.5*y^2) )/
	((f1(1)/2) * exp(-abs(y)) ) ) { accepted[c]<-y;c<-c+1} else {next} }


hist(accepted,prob=T)
curve(dnorm(x,0,1), col="darkblue", lwd=2, add=TRUE, yaxt="n")

write(rlaplace(1000, m=0, s=1 ),"laplace")
