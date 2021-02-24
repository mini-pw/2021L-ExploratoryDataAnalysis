install.packages("proton")
library(proton)
proton()

 ## Etap 1

employees[employees$name == "John" & employees$surname == "Insecure", c("login")]
proton(action = "login", login="johnins")

## Etap 2


for (pass in top1000passwords) {
  response <- proton(action = "login", login= "johnins", password = pass) ##lewy alt i minus
  if (response == "Well done! This is the right password!"){
    cat(pass)}
  }

## Etap 3

employees[employees$surname == "Pietraszko", c("login")]
table(logs[logs$login == "slap", c("host")]) -> tmp
data.frame(tmp)
proton(action = "server", host="194.29.178.16")

## Etap 4
comands <- c()
split_bash_history <- str(bash_history, ' ')
for (x in split_bash_history){
  
}
