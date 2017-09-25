options(scipen=999)

#1
r<-.5
n=1000
circle<-0

plot(-.5:(1.5), -.5:1.5, type = "n") 
for (i in 1:n) {
	xy<-c( runif(2,0,1) )
	if ((xy[1]-r)^2+(xy[2]-r)^2 < r^2) {circle<-circle + 1; 
		points(xy[1],xy[2],col="red")  } else {
		 points(xy[1],xy[2],col="blue") } }

4*(circle/1000) 

(circle/1000) * (1 -  (circle/1000)  ) / n




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

s=2
f1<-function(x){exp(-x^2) * (1/sqrt(2*pi*s^2) ) * exp( (-x^2)/(2*s^2) ) }

hx<-function(x){exp(-x^2)}

n=10000
HX<-rep(NA,n)

for( i in 1:n ){ xj<-rnorm(1,0,2); HX[i]<- hx(xj) } 

round(HX,5)

sum(HX)/n


1/(sqrt(2*4+1))