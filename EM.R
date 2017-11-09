rm(list = ls())

#suppose theta=0.7 and the latent Bernoulli variable that assigns group has probability p=0.3

theta<- 0.4 
tau <- 0.6
x <- y <- rep(0,10000)
for( i in 1:10000) {
  if( runif(1) < tau ) {
    x[i] <- rgamma(1,shape=.5,scale= 1/(2*(1-theta)) )
    y[i] <- "heads"
  } else {
    x[i] <- rgamma(1,shape=.5,scale= 1/ (2*(theta)) )
    y[i] <- "tails"
  }
}

densityplot( ~x,  par.settings = list(
               plot.symbol = list(   col=as.factor(y)    )    )     )

dat<-data.frame(x,y)

plot (density(    dat[which(dat$y == "tails"),]$x  ) , col="#d53e4f",lwd=2)  
lines(   density(   dat[which(dat$y == "heads"),]$x    ) , col="#2166ac",lwd=2 ) 


#run the EM algorithm with initial guesses for scale and group assignement

sc_1 <- .7
 


tau_1 <- 1.5
tau_2 <- 0.5

for( i in 1:125 ) {
  T_1 <- tau_1 * rgamma(x,shape=.5,scale= 1/(2*(1-sc_1)) )
  T_2 <- tau_2 * rgamma(x,shape=.5,scale=  1/ (2*(sc_1 )) )

  P_1 <- T_1 / (T_1 + T_2)
  P_2 <- T_2 / (T_1 + T_2)
	
  tau_1 <- mean(P_1)
  tau_2 <- mean(P_2)
  sc_1 <- sum( P_1 * x ) / sum(P_1)
  print( c(sc_1 , 1-sc_1 ) )}

#It looks like the estimates for the theta parameter are converging to .5. This may make sense.
#Let's take another look at our densities. The red and the blue densities are centered around
#the same number because of their same scale parameter. The red has a larger spike at this
#center, and the blue is flatter, but they look somewhat similar. When evaluating our data
#the EM may be assigning nearly equal weights to each distribution for many of the data points.
#And in fact, when we evaluate for the scale parameter with theta=.5, we have a Gamma(.5,1)
#distribution. Plotting the Gamma(.5,1), we can see it between our orginial gamma distributions.


plot (density(    dat[which(dat$y == "tails"),]$x  ) , col="#d53e4f",lwd=2)  
lines(   density(   dat[which(dat$y == "heads"),]$x    ) , col="#2166ac",lwd=2 ) 
lines(   (density(  rgamma(1000,shape=.5,scale= 1) ) ), col="#1b7837",lwd=2,lty=2 )



