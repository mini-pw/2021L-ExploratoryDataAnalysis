install.packages("proton")
library(proton) #prosze wczytaj mi pakiet

proton()

## Etap 1
head(employees)
employees[employees$name == "John" &
            employees$surname == "Insecure","login"]

proton(action = "login", login ='johnins')

## Etap 2
head(top1000passwords)
# lewy alt + minus to strzalka

for (pass in top1000passwords) {
  response <- proton(action = "login", login ='johnins', password= pass)
  if (response == 'Success! User is logged in!'){
    cat(pass)
  }  
}

## Etap 3

proton(action = "server", host="XYZ")
