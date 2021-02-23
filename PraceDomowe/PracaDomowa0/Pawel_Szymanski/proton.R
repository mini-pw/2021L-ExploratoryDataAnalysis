library(proton)
proton()
head(employees)
login<-employees[employees$name=="John"& employees$surname=="Insecure" ,"login"]
proton(action="login",login=login)

for(i in top1000passwords){
  if(proton(action="login",login="johnins",password=i)=="Success! User is logged in!"){
    password<-i
  }
}
proton(action="login",login=login,password=password)
head(logs)
login2<-employees[employees$name=="Slawomir"& employees$surname=="Pietraszko" ,"login"]
library(dplyr)
count(logs[logs$login==login2,],host)
proton(action = "server", host="194.29.178.16")
head(bash_history)

split<-strsplit(bash_history," ")
head(split)
for(i in 1:length(bash_history)){
  split[i]<-split[[i]][1]
}
split<-unlist(split)
head(split)
unique(split)

proton(action="login",login=login2,password="DHbb7QXppuHnaXGN")
