
r<-.5
circle<-0
plot(-.5:(1.5), -.5:1.5, type = "n") 
for (i in 1:1000) {
	xy<-c( runif(2,0,1) )
	if ((xy[1]-r)^2+(xy[2]-r)^2 < r^2) {circle<-circle + 1; 
		points(xy[1],xy[2],col="red")  } else {
		 points(xy[1],xy[2],col="blue") } }

4*(circle/1000))



require(stats) # for rnorm
plot(-4:4, -4:4, type = "n")  # setting up coord. system
points(rnorm(200), rnorm(200), col = "red")
points(rnorm(100)/2, rnorm(100)/2, col = "blue", cex = 1.5)


#2 


U=runif(10000,0,1)

hist( (4*sqrt(1-U^2) )  , xlim=c(0,6))

sum( (4*sqrt(1-U^2) ) /10000 )




integrand <- function(x) {exp((-4*x)/3)*x^3 }
plot(integrand,xlim=c(0,15))

n=10000
MC<-rep(0,n)
for ( i in 1:n) {  U<-runif(1);	
MC[i]<- exp((-4+4*U)/(3*U)) * ((1-U)/U)^3 * (1/U)^2;  points(U,MC[i])}

sum(MC)/n*

((4/3)^4)/gamma(4)


