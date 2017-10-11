ALL<-list()
x=1:26
 x <- x[!x == 24]

for( i in  x ){

site<-paste0("https://www.basketball-reference.com/players/",letters[i],"/")

#players<-readLines(site)
players<-grep('data-append-csv',players,value=T) 

 
namez<-regmatches(players,regexec("html(.*?)<",players))
namez<-unlist(lapply(namez, `[[`, 2))
namez<-substring(namez,3)

yrMin<-regmatches(players,regexec("min(.*?)<",players))
yrMin<-substring(unlist(lapply(yrMin, `[[`, 2)),4)

yrMax<-regmatches(players,regexec("max(.*?)<",players))
yrMax<-substring(unlist(lapply(yrMax, `[[`, 2)),4)

position<-regmatches(players,regexec("pos(.*?)<",players))
position<-substring(unlist(lapply(position, `[[`, 2)),4)


height<-regmatches(players,regexec("height(.*?)<",players))
height<-substring(unlist(lapply(height, `[[`, 2)),15)

weight<-regmatches(players,regexec("weight(.*?)<",players))
weight<-substring(unlist(lapply(weight, `[[`, 2)),4)

dob<-regmatches(players,regexec("birth_date(.*?)>",players))
dob<-gsub("[^0-9]", "", unlist(lapply(dob, `[[`, 1)))

college<-regmatches(players,regexec("college=(.*?)td",players))
college<-lapply(college, function(x) if(identical(x, character(0))
	) ">No School<" 	else x)
college<-unlist(college)
college<-college[-grep("college", college)]
college<-unlist(lapply(regmatches(college,regexec(">(.*?)<",college)), 
	`[[`, 2))

HTMLs<-regmatches(players,regexec("/players(.*?)html",players))
HTMLs<-unlist(lapply(HTMLs, `[[`, 1))

ALL[[i]]<-data.frame(
	namez,yrMin,yrMax,position,height,weight,dob,college,HTMLs)
}

db<- do.call("rbind", ALL)
write.csv(db, file = "basketball.csv")

dat<-read.csv("2017bb.csv",header=T,stringsAsFactors=F)

tail(dat)
dat<-dat[which(dat$MP > 900),]

dat<-dat[which(dat$Pos  %in% c("PG","G","SG")),]
dat$ppg<-round(dat$PTS/dat$G,2)
dat<-dat[which(dat$ppg > 17),]

sites<-dat$html
playerz<-list()
draftpos<-list()

link<-paste0("https://www.basketball-reference.com/players/",
	substr(sites[i],1,1),"/",	sites[i], ".html")

#playerz[[i]]<-readLines(link)
for (i in 1:length(sites) ){

playerz[[i+100]]<-grep('draft.html',playerz[[i ]],value=T) 
playerz[[i+100]]<-regmatches(playerz[[i+100]],regexec("pick(.*?)overall",
	playerz[[i+100]]))[[1]][2]
playerz[[i+100]]<-gsub("[^0-9]", "", unlist(lapply(playerz[[i+100]], 
	`[[`, 1)))
}

dat$draft<-as.numeric(unlist(playerz[101:124]))
dat<-dat[which(dat$draft < 8),]

write.csv(dat, file = "top.csv")
dat
 
colnames(db)[1]<-"Player"

dat<-merge(dat,db[which(db$Player %in% dat$Player),][,1:2],"Player")


https://www.basketball-reference.com/players/w/wadedw01/gamelog/2004/



regexec("per_game.2009(.*?)per_game.2010",	playerz[[i]])

grep('per_game.2009',playerz[[i ]],value=T) 

cleanFun <- function(htmlString) {
  return(gsub("<.*?>", ";", htmlString))
}


dat$yrMin<-as.character(dat$yrMin)

statzh<-c("Player","Season","Age","Tm","Lg","Pos","G","GS","MP","FG",
	"FGA","FG%","3P","3PA","3P%","2P","2PA","2P%","eFG%","FT","FTA",
	"FT%","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS")

pgs<-c("Rk","G","Date","Age","Tm","Opp","GS","MP","FG","FGA","FG%","3P",
"3PA","3P%","FT","FTA","FT%","ORB","DRB","TRB","AST","STL","BLK","TOV","PF",
"PTS","GmSc","+/-")

rookstats<-list();c=1

for(i in 1:length(playerz) ){

pl<-grep('stats, NBA, players, ',playerz[[i ]],value=T) 
pl<-regmatches(pl,regexec("ers, (.*?),",pl))[[1]][2]

if( !(pl %in% dat$Player) == T ) {next}

statz<-cleanFun(grep(paste0("per_game.",dat[which(dat$Player == pl),]$yrMin),
	playerz[[i]],value=T) )

statz<-gsub(";;",",", statz);
statz<-gsub(",;",",", statz)
statz<-substring(statz, 2)
xxx<-as.data.frame( matrix(-50,1,length(statzh)  ) )
xxx[1,1]<-pl
xxx[1,2:length(xxx)]<-(unlist(strsplit(statz,","))[-4])
colnames(xxx)<-statzh
rookstats[[c]]<-xxx
c<-c+1}


rookstats<- do.call("rbind", rookstats)

dat
fmonth<-list()
fmonthstat<-list()
fmonthstats<-list()


for( i in 1:nrow(dat)){
link<-paste0("https://www.basketball-reference.com/players/",
	substr(dat$html[i],1,1),"/",dat$html[i], "/gamelog/" ,dat$yrMin[i])
fmonth[[i]]<-readLines(link)
}

for( i in 1:length(fmonth)){
 for (j in 1:15){
basc<-paste0("basic.",j)
xxx<-cleanFun(grep(basc,fmonth[[i]],value=T)[1] )
xxx<-gsub(";;;@;;;","@", xxx)
xxx<-gsub(";;;;;;","vs", xxx)
xxx<-gsub(";;;;",",", xxx)
xxx<-gsub(";;;",",", xxx)
xxx<-gsub(";;",",", xxx)
xxx<-substring(xxx, 2,(nchar(xxx)-1))
xxx<-(unlist(strsplit(xxx,",")))
if(length(xxx) == 27 ){
	if(xxx[16] == 0 ) { xxx<-c(xxx[1:16],0,xxx[17:27])}
	if(xxx[13] == 0 ) { xxx<-c(xxx[1:13],0,xxx[14:27])}}
if(length(xxx) == 26 ){ xxx<-c(xxx[1:13],0,xxx[14:16],0,xxx[17:26])}
if(length(xxx) == 25 ){ xxx<-c(xxx[1:10],0,xxx[11:13],0,
	xxx[14:16],0,xxx[17:25])}
fmonthstat[[j]]<-as.data.frame( matrix(-90, 1,length(pgs) ))
fmonthstat[[j]][1,]<-xxx
colnames(fmonthstat[[j]])<-pgs}

db<- do.call("rbind", fmonthstat)
fmonthstats[[i]]<-db
}


for( i in 1:length(fmonthstats)){
	fmonthstats[[i]]$Opp<-gsub(" ","",fmonthstats[[i]]$Opp)}



