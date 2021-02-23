#Praca domowa nr.1

installed.packages("proton")
library(proton)
proton()

# Step 1: Find the login of John Insecure
head(employees)
employees[employees$name=="John"&employees$surname=="Insecure",]
proton(action="login",login="johnins")


# Step 2: Find John Insecure's password in top1000passwords

for (pass in top1000passwords){
  response <- proton(action="login",login="johnins",password=pass)
  if (response=='Success! User is logged in!'){
    cat(pass)
  }
}

# Step 3: Check from which server Pietraszko logs into the Proton server most often.

employees[employees$surname=="Pietraszko",]
logs[logs$login=="slap",]
count(logs[logs$login=="slap",],host)

proton(action="server",host="194.29.178.16")



# Step 4: Find the Pietraszko's password.

split_bash_history <- strsplit(bash_history," ")

commands <- c()

for (x in split_bash_history){
  commands <- c(commands, x[[1]])
}
unique(commands)

############# FINAL STEP ###############
proton(action="login",login="slap",password="DHbb7QXppuHnaXGN")
