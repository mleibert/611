
r<-.5
circle<-0
plot(-2:2, -2:2, type = "n") 
for (i in 1:1000) {
	xy<-c( runif(2,-1,1) )
	if (xy[1]^2+xy[2]^2 < 1) {circle<-circle + 1; 
		points(xy[1],xy[2],col="red")  } else {
		 points(xy[1],xy[2],col="blue") } 

4*(circle/100)



require(stats) # for rnorm
plot(-4:4, -4:4, type = "n")  # setting up coord. system
points(rnorm(200), rnorm(200), col = "red")
points(rnorm(100)/2, rnorm(100)/2, col = "blue", cex = 1.5)
