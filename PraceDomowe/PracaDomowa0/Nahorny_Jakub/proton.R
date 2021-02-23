
library(proton)
proton()
head(employees)
employees[employees$name == "John",]
proton(action = "login", login="johnins")

for (password in top1000passwords) {
  response <- proton(action = "login", login="johnins", password=passwrd)
  if(response == 'Success! User is logged in!') {
    cat(passwrd)
  }
}

employees[emplyees$surname == "Pietraszko", ]

count(logs[logs$login =="slap",], host)

split_bash_history <- strsplit(bash_history, " ")

commands <- c()

for (x in split_bash_history) {
  commands <- c(commands, x[[1]])
}

proton(action = "login", login="slop", password ="DHbb7QXppuHnaXGN")