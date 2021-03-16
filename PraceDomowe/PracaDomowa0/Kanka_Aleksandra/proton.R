# 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).
install.packages("proton")
library(proton)
proton()



## Etap 1

employees[employees$surname == "Insecure", ]

proton(action = "login", login = "johnins")

## Etap 2

top1000passwords #wektor

for (pass in top1000passwords) {
  response <- proton(action = "login", login="johnins", password= pass)
  if (response == 'Success! User is logged in!'){
    cat(pass) ## chcemy wypisac to haslo
  }
}

## Etap 3

employees[employees$surname == "Pietraszko",]

## slap
table(logs[logs$login == "slap", c("host") ]) -> tmp
data.frame(tmp)
## count(logs[log$login == "slap", c("host")])

proton(action = "server", host = "194.29.178.16")


## Etap 4

bash_history #lista, nie mozna dim
# trzeba podzielic komendy
split_bash_history <- strsplit(bash_history, " ") # rozdziela napisy względem podanego znaku, tutaj względem spacji " "
## lista a w liscie wektory)
comands <-  c()

for (x in split_bash_history){ # wybieramy pierwszy napis po podzieleniu komendy z bash_history
  comands <- c(comands, x[[1]]) #comands cala lista, x to calywektor x(1)-1.wartosc wektora
}

for (comand in unique(comands)){ # wykonujemy w pętli logowanie używając po kolei wartości comands 
  proton(action = "login", login = "slap", password = comand)
  
}

# 5) Umieszczamy na repozytorium rozwiązanie.