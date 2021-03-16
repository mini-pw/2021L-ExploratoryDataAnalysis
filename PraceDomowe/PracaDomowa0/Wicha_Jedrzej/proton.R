install.packages("proton")
library(proton)

proton()

## Etap 1

employees[employees$surname == "Insecure",]

proton(action = 'login', login='johnins')


## Etap 2

for (p in top1000passwords) {
  res = proton(action = 'login', login='johnins', password=p)
  if (res == 'Success! User is logged in!'){
    cat(p[])
  }
}
proton(action = 'login', login='johnins', password='q1w2e3r4t5')

## Etap 3

employees[employees$surname == "Pietraszko",]
table(logs[logs$login == 'slap', c('host')]) -> hosts_p
data.frame(hosts_p)

proton(action="server", host="194.29.178.16")


## Etap 4

split_bash_history = strsplit(bash_history, " ")
split_bash_history

commands = c()

for (x in split_bash_history) {
  commands <- c(commands, x[[1]])
}

unique(commands)

proton(action = "login", login = "slap", password = "DHbb7QXppuHnaXGN")
