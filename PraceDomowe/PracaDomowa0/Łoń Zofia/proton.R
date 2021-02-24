install.packages("proton")
library(proton)

proton()

##Etap 1
employees
data("employees")
employees[employees$name == "John",]

proton(action = "login",login = "johnins")

## Etap 2 

top1000passwords
top1000passwords[1]
for (i in 1:1000){
  proton(action = "login",login = "johnins", password=top1000passwords[i])
}

## Etap 3

employees[employees$surname == "Pietraszko",]

#dostałam login slap 
logs
logs[logs$login== "slap",c("host")]
table(logs[logs$login== "slap",c("host")]) -> tmp
data.frame(tmp)
#194.29.178.16
proton(action = "server", host="194.29.178.16")

## Etap 4
bash_history

split_bash_history <- strsplit(bash_history," ")
comands <- c()

for (x in split_bash_history){
  comands <- c(comands, x[[1]])
}
comands
