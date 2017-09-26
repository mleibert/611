options(scipen=999)
#http://statweb.stanford.edu/~susan/courses/s208/node14.html
 
r<-.5
n=1000
circle<-0
plot(-.5:1.5 , -.5:1.5 , type = "n") 
XY<-list()
for (i in 1:n) {
	xy<-c( runif(2,0,1) )
	if ((xy[1]-r)^2+(xy[2]-r)^2 < r^2) {circle<-circle + 1; 
		points(xy[1],xy[2],col="red")  } else {
		 points(xy[1],xy[2],col="blue") } 
	XY[[i]]<-t(matrix(xy)) }

do.call(rbind,XY)


4*(circle/1000)

head(as.data.frame(do.call(rbind,XY)))
x<-as.data.frame(do.call(rbind,XY))[,1]

pimean<-circle/n;pimean
pimean*(1-pimean)*n
length(c(rep(1,circle),rep(0,n-circle)) )
sum((c(rep(1,circle),rep(0,n-circle)) -pimean)^2)/n^2






require(stats) # for rnorm
plot(-4:4, -4:4, type = "n")  # setting up coord. system
points(rnorm(200), rnorm(200), col = "red")
points(rnorm(100)/2, rnorm(100)/2, col = "blue", cex = 1.5)