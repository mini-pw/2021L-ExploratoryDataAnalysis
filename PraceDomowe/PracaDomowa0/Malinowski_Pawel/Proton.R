install.packages("proton")
install.packages("dplyr")
library(proton)
library(dplyr)
proton()
head(employees)
employees[employees$surname == "Insecure",]
proton(action = "login", login="johnins")

for(passwrd in top1000passwords){
  response <- proton(action ="login", login="johnins",password=passwrd)
  if (response == 'Success! User is logged in!'){
    cat(passwrd)
  }
}
employees[employees$surname == "Pietraszko",]

count(logs[logs$login == "slap",], host)
split_bash_history <- strsplit(bash_history,"")
commands <- c()
for(x in split_bash_history){
  commands <-(commands, x[[1]])
}
