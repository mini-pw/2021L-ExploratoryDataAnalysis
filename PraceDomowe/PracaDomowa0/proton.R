#install.packages("proton")
library(proton)
proton()

## Etap 1
employees[employees$surname == 'Insecure',]
proton(action= "login", login = "johnins")

## Etap 2
top1000passwords
for (i in top1000passwords){
  response <- proton(action= "login", login = "johnins", password = i)
  if (response == 'Success! User is logged in!'){
    cat(i)
  }
}

## Etap 3
logs
employees[employees$surname == 'Pietraszko',]
# login slap
table(logs[logs$login == 'slap', c('host')]) -> tmp
data.frame(tmp)
proton(action = 'server', host = '194.29.178.16')


## Etap 4

head(bash_history)
