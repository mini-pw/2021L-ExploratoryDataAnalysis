library(DescTools)
library(dplyr)
library(ggplot2)
library(forcats)
## Wykres 1
Pokemony <- read.csv("Pokemony.csv", sep = ',')

pokemonType1 <- Pokemony %>% 
  mutate(Type=type1) %>% 
  select(Type,Color)

pokemonType2 <- Pokemony %>% 
  mutate(Type=type2) %>% 
  filter(Type!="") %>% 
  select(Type,Color)

# plot z odcieni dla typow
union(pokemonType1,pokemonType2) %>% 
  mutate(hue=ColToHsv(Color)["h",]) %>% 
  ggplot(aes(x = Type,y=hue,color=Color))+
  geom_point(shape=95,size=8,stroke=5,alpha=0.8)+
  geom_density(color="white")+
  scale_colour_identity()+
  scale_shape_identity()+
  labs(title = "Rozkład odcieni pokemonów dla różnych typów",
       x = "Typ pokemona",
       y = "Odcień")+
  theme(
    panel.background = element_rect(fill = "black", colour = "black",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "black"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "black")
  )

## Wykres 2
pokemonType1 <- Pokemony %>% 
  mutate(Type=type1) %>% 
  select(Type,Color,weight_kg, height_m)

pokemonType2 <- pokemony %>% 
  mutate(Type=type2) %>% 
  filter(Type!="") %>% 
  select(Type,Color, weight_kg, height_m)

x <- union(pokemonType1,pokemonType2) %>% 
  filter(Type == "dark" | Type == "ghost" | Type == "fire" | Type == "grass" | Type == "rock" | Type == "water")

ggplot(x, aes(x = height_m, y = weight_kg, color=Color)) + 
  scale_colour_identity() +
  geom_point()  + facet_wrap(~Type) + xlim(0,5) + ylim(0,300) + stat_function(fun = function(x) 25*x^2, color = 'red') +
  labs(title = "Rozkład wagi i wzrostu pokemonów w zależności od koloru i typu", x = "Wzrost_m", y="Waga_kg")

## Wykres 3
pokemony4 <-  pokemony %>% 
  group_by(type1) %>% 
  summarise(n = n())

pokemony3 <- pokemony %>%
  filter(BMI >= 30) %>% 
  group_by(type1) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

pokemony5 <- left_join(pokemony3, pokemony4, by = "type1") %>% 
  mutate(procent = round(n.x/n.y * 100)) %>% 
  arrange(desc(procent))

pokemony5 %>% 
  mutate(type1 = fct_reorder(type1, desc(procent))) %>% 
  ggplot(aes(x = type1, y=procent), ) + coord_flip() + geom_col(colour = "black", width = 0.5, fill = "blue") +
  labs(title = "Otyłość wśród pokemonów (BMI > 30)", x="TYP", y="Procent OTYŁYCH POKEMONÓW") + geom_text(aes(label = procent), size = 5, vjust = -0.5)
