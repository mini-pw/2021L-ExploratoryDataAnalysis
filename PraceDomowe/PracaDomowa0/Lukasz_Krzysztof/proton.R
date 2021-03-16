library(proton)

proton()

head(employees)
var1 <- employees[(employees$name=='John' & employees$surname=='Insecure'),c('login')]

proton(action = "login", login=var1)

str(top1000passwords)

for (i in top1000passwords) {
  x <- proton(action = "login", login='johnins', password= i)
  if (x =='Success! User is logged in!'){cat(x)}
}

head(logs)
var2 <- employees[(employees$surname=='Pietraszko'),c('login')]

logs[logs$login=='slap',c('host')] -> w
table(w)

proton(action='server', host = '194.29.178.16')
