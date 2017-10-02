integrand <- function(x) {(exp(-x)) / (1+x^2 ) }
integrate(integrand ,0,1)

TF=rep(0,1000)

n=10000;f<-rep(0,n)
for ( i in 1:n) {  U<-runif(1);	f[i]<- integrand(U) 	 }
fbarhn<-sum(f)/n;fbarhn
var(f)

I2<-function(G) { (1-exp(-1))/(1+G^2) }
g<-rep(0,n)
for ( i in 1:n) { U<-runif(1); G=-log(-U*(1-exp(-1))+1 ); g[i]<-I2(G)}
gbarhn<-sum(g)/n;gbarhn
var(g)

data.frame(c(fbarhn,gbarhn),c(var(f),var(g)))

var(g) > var(f)

#2

integrand <- function(x) { exp( x)  }
integrate(integrand ,0,1)

n=10000
AV<-rep(NA,n)
for (i in 1:n){	U<-runif(1);V=1-U; AV[i]<-.5* (exp(U) + exp(V) )	}
mean(AV)
var(AV)

MC<-rep(NA,n*2)
#simple mc
for (i in 1:(n*2)){ U<-runif(1) ; MC[i]<- exp(U)	}
mean(MC)
var(MC)


var(AV)<var(MC)


(var(MC)-var(AV) / var(MC) ) * 100

require("markovchain")
require("diagram")
library(pracma)



plotmat(t(tm),pos = c(1,2,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "#a6bddb",
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = -.13,
        main = "")

tm<-matrix(c(.4,.3,.3,0,0,0,.5,0,.5,0,.5,0,.5,0,0,0,.5,0,.5,0,0,.3,0,.3,.4),5,
	5,byrow=T)

tmt<- new("markovchain",transitionMatrix=tm,
	states=c("1","2","3","4","5"),name="MarkovChain A") #create the DTMC

recurrentClasses(tmt)
summary(tmt)

mat<- matrix( c(.7,0,.3,0,.6,0,.4,0,0,.5,0,.5,0,.4,0,.6 ),4,4,byrow = T)
mat2<-matrix( c(-.3,.6,0,0,0,-1,.5,.4,.3,.4,-1,0,1,1,1,1),4,4,byrow = T)
mat2

PI=solve(mat2,c(0,0,0,1))

PI%*%mat



### 5
genes<-matrix(0,4,4)
N=3
j=0:3

for( i in 0:3){	genes[i+1,]<-dbinom(j,N, i/4) }

sum( dbinom(j,N, i/4) )

 dbinom(j,N, i/4) 

choose(N,0)*(i/N)^0*(1-(i/N))^(N-0) +
choose(N,1)*(i/N)^1*(1-(i/N))^(N-1) +
choose(N,2)*(i/N)^2*(1-(i/N))^(N-2) +
choose(N,3)*(i/N)^3*(1-(i/N))^(N-3)





