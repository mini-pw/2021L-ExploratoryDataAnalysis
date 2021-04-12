library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

baza_oscary <- filter(read.csv('oscars.csv'),Ceremony>=35)
baza_filmy <- read.csv('movies.csv')

# funckja, która usunie spacje z nazw filmów w ramce oscary
spacje <- function(x){
  n <- nchar(x)
  ifelse(substr(x,n,n)==" ", substr(x,1,n-1), x)
}

oscary <- baza_oscary %>% 
  filter(Award == "Best Picture") %>% 
  select("Name","Winner","Year") %>% 
  mutate(Name=spacje(Name))


filmy <- select(baza_filmy, c("title","release_date"))

daty <- oscary %>% 
  inner_join(filmy, by = c("Name"="title")) %>% 
  filter(Year==substr(release_date,1,4))        #jest kilka filmów z tym samym 
                                                #tytułem, więc usuwam nieoscarowe      
miesiace <- daty %>% 
  select("release_date","Winner") %>% 
  mutate(release_date=months(as.Date(release_date)))

y <- count(filter(miesiace,Winner==1),release_date)
z <- data.frame("release_date"="czerwiec", "n" = 0)
x <- bind_rows(y,z)



statystyki <- x %>% 
  inner_join(count(miesiace,release_date), by="release_date") %>% 
  rename(laureaci=n.x, nominowani=n.y) %>% 
  mutate(procent_laureatów=100*laureaci/48) %>% 
  mutate(procent_nominowanych=100*nominowani/276)


  
cos <- gather(statystyki,"laureat_nominowany","procent",4:5)

cos$release_date <- factor(cos$release_date, levels = c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec", "lipiec", "sierpień", "wrzesień", "październik", "listopad", "grudzień"))


ggplot(cos,aes(x=release_date,y=procent, fill=laureat_nominowany))+
  geom_bar(stat="identity",position="dodge") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "miesiąc", fill = "") +
  scale_fill_discrete(labels = c("laureaci","nominowani"))
  

