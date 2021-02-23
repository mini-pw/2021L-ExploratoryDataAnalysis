library(proton)
library(dplyr)
proton()
employees[c(employees$name == 'John', employees$surname == 'Insecure'),]
proton(action = 'login', login = 'johnins')


top1000passwords
vapply(proton(action = 'login', login = 'johnins'),top1000passwords)

for (passwrd in top1000passwords){
  response <- proton(action = "login", login="johnins", password=passwrd)
  if (response == 'Success! User is logged in!'){
    cat(passwrd)
}
}

employees[employees$surname == "Pietraszko", ]
count(logs[logs$login == "slap", ], host)
proton(action = "server", host=" 194.29.178.16")

split_bash_history <- strsplit(bash_history, " ")

commands <- c()

for (x in split_bash_history) {
  commands <- c(commands, x[[1]])
}
unique(commands)
proton(action = "login", login="slap", password="DHbb7QXppuHnaXGN")
