library(proton)
proton()
head(employees)
employees[employees$name == "John" & employees$surname == "Insecure", ]$login
proton(action = "login", login="johnins")
head(top1000passwords)
length(top1000passwords)

for(i in seq(1000)){
  wynik = proton(action = "login", login="johnins", password=top1000passwords[i])
  if (wynik == "Success! User is logged in!"){
    password = top1000passwords[i]
    break
  }
}
password

#dalej na labkach

employees[employees$surname =="Pietraszko",]
head(logs[logs$login=="slap",])
library(dplyr)
count(logs[logs$login=="slap",],host)

head(bash_history)
spli_bash_history <- strsplit(bash_history, " ")
spli_bash_history

commands <- c()
for (x in spli_bash_history) {
  commands <- c(commands, x[[1]])
}
head(commands)
unique(commands)
proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")
