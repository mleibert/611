rm(list = ls())
setwd("g:\\math\\611")

bb<-read.csv("betaBLOCKERs.csv",header=T)
bblist<-list()

for( i in 1){

	CD<-bb[which(bb$center == i & bb$trt== "C" & bb$value== "Death" ) , ]
	CD<-CD[1,][rep(seq_len(nrow(CD[1,])), each=sum(CD[,1])),]
 
	TD<-bb[which(bb$center == i & bb$trt== "T" & bb$value== "Death" ) , ]
	TD<-TD[1,][rep(seq_len(nrow(TD[1,])), each=sum(TD[,1])),]

	CT<-bb[which(bb$center == i & bb$trt== "C" & bb$value== "Total" ) , ]
	CT<-CT[1,][rep(seq_len(nrow(CT[1,])), each=(sum(CT[,1]))-
		nrow(CD)),]

	TT<-bb[which(bb$center == i & bb$trt== "T" & bb$value== "Total" ) , ]
	TT<-TT[1,][rep(seq_len(nrow(TT[1,])), each=(sum(TT[,1]))-
		nrow(TD)),]
	
	bblist[[i]]<-rbind(CD,TD,CT,TT)}

bblist<- do.call("rbind", bblist);rownames(bblist)<-NULL

bblist$death<-ifelse(bblist$value == "Death" , 1,0)

sink("myfile.txt", append=FALSE, split=FALSE)
require("MCMCpack")
posterior <- MCMClogit(death~as.factor(trt) ,  data=bblist)

plot(posterior)
sink("myfile.txt", append=FALSE, split=FALSE)
summary(posterior)
sink()


posteriors <- MCMClogit(death~as.factor(trt) ,  data=bblist)
summary(posteriors)

posteriors <- MCMCmnl(death~as.factor(trt) ,  data=bblist)

#####################


 MHexp<-function(logf,currentvalue,B) 
	{# B: number of iterations 
	S<-rep(0,B) 
	n_accept<-0 

	for(i in 1:B) 
		{proposal<-rexp(1,currentvalue) 
		probacc<-

		exp(	
		logf(proposal)-logf(currentvalue) +
		dexp(currentvalue,proposal,log=TRUE)- 
		dexp(proposal,currentvalue,log=TRUE)
		) 

		accept<-ifelse(runif(1)<probacc,1,0) 
		
		currentvalue<-ifelse(accept==1,proposal,currentvalue) 
		S[i]<-currentvalue 
	
	n.accept<-n.accept+(accept==1)} 
	c(S,n.accept/B) 
}



 MHexp<-function(logf,currentvalue,B,SD=1) 
	{# B: number of iterations 
	S<-rep(0,B) 
	n_accept<-0 

	for(i in 1:B) 
		{proposal<-rnorm(1,currentvalue,SD) 	
		
		probacc<-exp( (logf(proposal) )-(logf(currentvalue) ) )

		accept<-ifelse(runif(1)<probacc,1,0) 
		
		currentvalue<-ifelse(accept==1,proposal,currentvalue) 
		S[i]<-currentvalue 
	
	n.accept<-n.accept+(accept==1)} 
	c(S,n.accept/B) 
}


x<-function(mu,var=1){rnorm(1,mu,sd=var)}

x(3,4)
x<-0:2

1-sum(	(2^x * exp(-2))/factorial(x)	)
	0.5939942* 0.4060058* 0.4060058

