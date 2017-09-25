options(scipen=999)
#http://statweb.stanford.edu/~susan/courses/s208/node14.html
 
r<-.5
circle<-0
plot(-1.5:1.5, -1.5:1.5, type = "n") 
par(mfrow=c(1,1))
XY<-list()
for (i in 1:1000) {
	xy<-c( runif(2,0,1) )
	if ((xy[1]-r)^2+(xy[2]-r)^2 < r^2) {circle<-circle + 1; 
		points(xy[1],xy[2],col="red")  } else {
		 points(xy[1],xy[2],col="blue") } 
	XY[[i]]<-t(matrix(xy)) }

do.call(rbind,XY)


4*(circle/1000)
((circle/1000)*(1-(circle/1000)))/1000




require(stats) # for rnorm
plot(-4:4, -4:4, type = "n")  # setting up coord. system
points(rnorm(200), rnorm(200), col = "red")
points(rnorm(100)/2, rnorm(100)/2, col = "blue", cex = 1.5)