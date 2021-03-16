install.packages("proton")
library("proton")

proton(action = "login", login = "johnins")

for (passwrd in top1000passwords) {
  response <-proton(action="login", login="johnins", passwrd=passwrd)
  if(response == "Success! User is logged in!"){
    cat(passwrd)
  }
  
}

employees[employees$surname == "Pietraszko",]

count(logs[logs$login == "slap",] host)