library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
Sys.setlocale("LC_TIME", "C")

dt <- read.csv('oscars_demo.csv')

fun2 <- function(x){
  a <- as.Date(x,"%d-%b-%y")
  year(a) <- 2000
  a
}
daty <- dt %>% 
  filter(award=="Best Actor"|award=="Best Actress") %>% 
  select("date_of_birth") %>% 
  transmute(daty=fun2(date_of_birth))

daty


zodiak <- function(x){
  if(x<=as.Date("2000-01-19")){
    zodiak = "koziorożec"
  }else if(x<=as.Date("2000-02-18")){
    zodiak = "wodnik"
  }else if(x<=as.Date("2000-03-20")){
    zodiak = "ryby"
  }else if(x<=as.Date("2000-04-19")){
    zodiak = "baran"
  }else if(x<=as.Date("2000-05-22")){
    zodiak = "byk"
  }else if(x<=as.Date("2000-06-21")){
    zodiak = "bliźnięta"
  }else if(x<=as.Date("2000-07-22")){
    zodiak = "rak"
  }else if(x<=as.Date("2000-08-23")){
    zodiak = "lew"
  }else if(x<=as.Date("2000-09-22")){
    zodiak = "panna"
  }else if(x<=as.Date("2000-10-22")){
    zodiak = "waga"
  }else if(x<=as.Date("2000-11-21")){
    zodiak = "skorpion"
  }else if(x<=as.Date("2000-12-21")){
    zodiak = "strzelec"
  }else if(x<=as.Date("2000-12-31")){
    zodiak = "koziorożec"
  }
  zodiak
}

x <- apply(daty, 1, zodiak)
x <- as.data.frame(x)
colnames(x) <- "zodiak"
head(x)
?as.data.frame


x %>% 
  count(zodiak) %>% 
  ggplot(aes(x=reorder(zodiak,n),y=n)) + 
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("liczba aktorów")+
  xlab("znak zodiaku")
  

