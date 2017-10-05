d<-readLines("https://www.basketball-reference.com/players/h/")

grep('scope=',d,value=T)[2:15]

dd<-grep('data-append-csv',d,value=T) 

gsub('"<tr ><th scope=\"row\" class=\"left \" data-append-csv=\"',"",dd)

aaaa<-'<tr ><th scope=\"row\" class=\"left \" data-append-csv=\'

nchar("<tr ><th scope=\"row\" class=\"left \" data-append-csv=\")

