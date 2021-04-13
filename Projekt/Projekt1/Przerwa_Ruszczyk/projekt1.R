library(ggplot2)
library(dplyr)
library(readr)

planets <- read_csv("planets.csv")
characters <- read_csv("characters.csv")

dane<-planets %>% 
  select(name,rotation_period,orbital_period)
dane<-na.omit(dane)
ggplot(dane,aes(x=rotation_period,y=orbital_period,label=name))+
  geom_point()+geom_text(aes(label=ifelse(orbital_period>1000|rotation_period<15|rotation_period>50,as.character(name),'')))+
  labs(title="Okres obiegu i obrotu planet",
       x="Okres obrotu (w godzinach)",
       y="Okres obiegu (w dniach)")+
  geom_point(aes(24,365),color="blue")

dane2<-characters %>% 
  group_by(homeworld) %>% 
  summarize(n=n()) %>% 
  arrange(-n)
dane2<-na.omit(dane2)
dane3<-dane2 %>% 
  group_by(n) %>% 
  summarize(k=n())
ggplot(dane3,aes(x=n,y=k))+
  geom_col()+
  geom_text(aes(label = k), position = position_dodge(width=0.5), vjust=-0.25)+
  scale_x_discrete(limits=c(1,2,3,4,5,6,7,8,9,10,11))+
  labs(title="Liczba planet z której pochodzi dana iloœæ postaci",
       x="liczba postaci",
       y="liczba planet")


dane4 <- planets %>%
  select(name, diameter, population) %>% 
  na.omit(c(diameter, population)) %>% 
  filter(diameter > 0) %>% 
  mutate(gest_zaludn = population/(3.14*(diameter)^2)) %>% 
  arrange(-gest_zaludn) %>% 
  head(6)

dane4$name <- factor(dane4$name, levels = dane4$name[order(-dane4$gest_zaludn)])

ggplot(dane4, aes(x=name, y=gest_zaludn)) +
  geom_bar(aes(x=name), stat="identity", color = "green", fill = "green") +
  geom_text(aes(label = gest_zaludn), position = position_dodge(width = 0.5), vjust=0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Gêstoœæ zaludnienia planet",
       x = "Klimat",
       y = "Gêstoœæ zaludnienia")
