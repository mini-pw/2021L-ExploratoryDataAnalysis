library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(hrbrthemes)
library(showtext)

'%nin%' <- function(a,b) ! a %in% b

font_add(family = "got", regular = "C:/Users/admin/Desktop/GoT.ttf")
showtext_auto()

df <- read.csv(file = "C:/Users/admin/Documents/labki/got.csv", sep = ";")

df <- df %>%
  filter(is.na(Season)==0)

zlicz <- df %>%
  count(Season, Method)

Metoda <- df %>%
  count(Method)

zlicz1 <- zlicz %>%
  pivot_wider(names_from = Method, values_from = n, values_fill = 0)

zlicz2 <- zlicz1 %>%
  select("Dragonfire (Dragon)", "Sword", "Wildfire", "Knife", "Arrow", "Poison")

pomocnicza <- count(df,Season)

zlicz3 <- data.frame(zlicz2, pomocnicza$n)

zlicz4 <- zlicz3 %>%
  mutate(Inne=pomocnicza.n-Dragonfire..Dragon.-Sword-Wildfire-Knife-Arrow-Poison) %>%
  select(-pomocnicza.n)

Sezon <- c("1","2","3","4","5","6","7","8")

zlicz5 <- data.frame(zlicz4, Sezon)

colnames(zlicz5) <- c("Dragon", "Sword", "Wild fire", "Knife", "Arrow", "Poison", "Other", "Sezon")

zlicz6 <- zlicz5 %>%
  pivot_longer(!Sezon, names_to = "Method", values_to = "n")

ggplot(data = zlicz6, aes(x=Sezon, y=n, fill=Method)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Season",
       y = "Deaths") +
  scale_fill_brewer(palette = "Set2") +
  theme(text = element_text(color = "white",family="got", size=25),
        axis.text.x = element_text(color="white", family="got", size=25),
        axis.text.y = element_text(color="white", family="got", size=25),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent")
  ) -> wykres
ggsave(wykres, filename = "C:/Users/admin/Documents/labki/wykres2.png",  bg = "transparent")
# Podpisy s¹ na wykresie tylko s¹ bia³e
