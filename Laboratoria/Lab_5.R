#########################################
###    WSTĘP DO EKSPLORACJI DANYCH    ###
###         LABORATORIUM 5            ###
#########################################

library(dplyr)
library(ggplot2)
library(PogromcyDanych)
auta2012
# Zadania z Lab 4

# Zadanie 4
# Ile jest samochodów poszczególnych marek, których rok produkcji jest 2008?

auta2012 %>% 
  filter(Rok.produkcji  == 2008, Marka != "") %>% 
  group_by(Marka) %>% 
  summarise(n = n()) -> tmp

ggplot(tmp, aes(x = reorder(Marka, n), y = n)) +
  geom_col() + 
  coord_flip() +
  labs(title = "Liczba samochodów w 2008 w podziale na marki.", 
       x = "Marka",
       y = "Liczba samochodów")

# Zadanie 5
# Jaka zależność jest między liczbą koni mechanicznych a pojemnością skokową dla samochodów 
# wyprodukowanych w 2007 i 2008 roku?

auta2012 %>% 
  filter(Rok.produkcji == 2007 | Rok.produkcji == 2008) %>% 
  ggplot(aes(x = KM, y = Pojemnosc.skokowa, color = factor(Rok.produkcji))) + 
  geom_point() +
  scale_x_continuous(limits = c(0, 1000)) +
  labs(title = "Zależność między liczbą koni mechanicznych a pojemnością skokową dla samochodów wyprodukowanych w 2007 i 2008 roku",
       x = "KM",
       y = "Pojemność skokową",
       color = "Rok produkcji")


library(SmarterPoland)
?countries

## Skale 

p <- ggplot(data = countries, aes(x = continent, fill = continent)) +
  geom_bar()
p

p + scale_y_continuous(expand = c(0, 0))

p + scale_x_discrete(position = "top")

p + scale_y_continuous(position = "right")

p + scale_y_reverse()

p + scale_y_log10()

p + scale_y_sqrt()

## Kolory

p + scale_fill_manual(values = c("red", "grey", "black", "navyblue", "green"))
p + scale_fill_manual(values = c("#feedde", "#fdbe85", "#fd8d3c", "#e6550d", "#a63603"))

p + scale_fill_manual(values = rainbow(5))

# color brewer http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
install.packages("RColorBrewer")
library(RColorBrewer)

RColorBrewer::brewer.pal(n = 5, name = "Blues")
p + scale_fill_manual(values = c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C"))

p <- ggplot(countries, aes(x = birth.rate, y = death.rate, color = death.rate)) +
  geom_point()
p
p + scale_color_gradient(low = "navyblue", high = "red")
p + scale_color_gradient2(low = "navyblue", high = "red", mid = "white", midpoint = 8)

## Legenda

p <- ggplot(data = countries, aes(x = continent, fill = continent)) +
  geom_bar()
p

p + theme(legend.position = "bottom")
p + theme(legend.position = "none")
p + theme(legend.title = element_blank())
p + theme(legend.title = element_text(color = "blue"),
          legend.text = element_text(color = "red"))

p + labs(fill = "coś")

## Koordynaty (coord)

p + coord_flip() + scale_y_reverse()

p + coord_polar()

# wykres kołowy
tmp <- data.frame(table(countries$continent))

ggplot(tmp, aes(x = "", y = Freq, fill = Var1)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0)

## Panele (facet)

diamonds
sam <-  sample(1:nrow(diamonds), 500)
?sample
?sample_n
small_diamonds <- diamonds[sam, ]

p <- ggplot(small_diamonds, aes(x = cut, y = price)) + 
  geom_boxplot()
p

p + facet_wrap(~clarity)

p + facet_wrap(~clarity, scales = "free_y")
p + facet_wrap(~clarity, scales = "free_x")
p + facet_wrap(~clarity, scales = "free")
p + facet_wrap(color~clarity)
p + facet_grid(color~clarity)


library(PogromcyDanych)
data("serialeIMDB")
head(serialeIMDB)

## Wykres liniowy

tmp <- serialeIMDB %>% 
  filter(serial == "Breaking Bad", sezon == 1)
tmp

ggplot(tmp, aes(x = as.numeric(as.character(odcinek)), y = ocena)) +
  geom_line(color = "green", size = 3) +
  scale_y_continuous(limits = c(0,10))

ggplot(tmp, aes(x = as.numeric(as.character(odcinek)), y = ocena)) +
  geom_col()

tmp <- serialeIMDB %>% 
  filter(serial == "Breaking Bad")

ggplot(tmp, aes(x = as.numeric(as.character(odcinek)), y = ocena, color = sezon)) +
  geom_line() +
  scale_y_continuous(limits = c(0,10)) + 
  coord_polar()


ggplot(tmp[tmp$sezon ==1,], aes(x = as.numeric(as.character(odcinek)), y = ocena, color = sezon)) +
  geom_line() +
  scale_y_continuous(limits = c(0,10)) +
  geom_smooth(se = FALSE)

## Heatmap

ggplot(tmp, aes(x = sezon, y = as.numeric(as.character(odcinek)), fill = ocena)) + 
  geom_tile()


## Zadanie 1
# Jak zależy ocena od odcinka/sezonu/liczby głosów dla serialu *?
# * - wybrany serial

## Zadanie 2
# Jaka jest różnica w ocenie seriali x i *?
# x - serial z Zadania 1
# * - drugi wybrany serial