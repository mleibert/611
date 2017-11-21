rm(list = ls())
setwd("g:\\math\\611")

bb<-read.csv("betaBLOCKERs.csv",header=T)
tail(bb)

bblist<-list()

for( i in 1:max(bb$center)){

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

require(MCMCpack)
posterior <- MCMClogit(death~as.factor(trt) , b0=0, B0=.001,data=bblist)
plot(posterior)
summary(posterior)

posteriors <- MCMClogit(death~as.factor(trt) ,  data=bblist)
summary(posteriors)


#####################


 MHexp<-function(logf,currentvalue,B) 
	{# B: number of iterations 
	S<-rep(0,B) 
	n_accept<-0 

	for(i in 1:B) 
		{proposal<-rexp(1,currentvalue) 
		probacc<-exp(
		
		logf(proposal)-logf(currentvalue) +
		dexp(currentvalue,proposal,log=TRUE)- 
		dexp(proposal,currentvalue,log=TRUE)) 

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
		
		probacc<-(logf(proposal) )/(logf(currentvalue) )

		accept<-ifelse(runif(1)<probacc,1,0) 
		
		currentvalue<-ifelse(accept==1,proposal,currentvalue) 
		S[i]<-currentvalue 
	
	n.accept<-n.accept+(accept==1)} 
	c(S,n.accept/B) 
}


x<-function(mu,var=1){rnorm(1,mu,sd=var)}

x(3,4)
