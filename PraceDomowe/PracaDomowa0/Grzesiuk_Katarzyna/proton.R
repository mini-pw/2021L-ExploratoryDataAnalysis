library(proton)
proton()

head(employees)
employees[employees$surname == "Insecure", ]

proton(action = "login", login="johnins")

head(top1000passwords)
for(password in top1000passwords){
  response <- proton(action = "login", login="johnins", password=password)
  if (response == "Success! User is logged in!"){
    cat(password)
  }
}

head(logs)
employees[employees$surname == "Pietraszko", ]
head(logs[logs$login == "slap", ])

library(dplyr)

count(logs[logs$login == "slap",], host)

proton(action = "server", host="194.29.178.16")

head(bash_history)
length(bash_history)

split_bash_history <- strsplit(bash_history, " ")

commands <- c()
for (x in split_bash_history){
  commands <- c(commands,x[[1]])
}

head(commands)
unique(commands)

proton(action="login",login="slap",password="DHbb7QXppuHnaXGN")
