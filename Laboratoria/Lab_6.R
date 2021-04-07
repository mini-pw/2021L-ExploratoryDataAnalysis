#########################################
###    WSTĘP DO EKSPLORACJI DANYCH    ###
###         LABORATORIUM 6            ###
#########################################

library(ggplot2)
library(SmarterPoland)
library(dplyr)

library(PogromcyDanych)
serialeIMDB
# 0) Zadania z lab 5

## Zadanie 1
# Jak zależy ocena od odcinka/sezonu/liczby głosów dla serialu *?
# * - wybrany serial

## Zadanie 2
# Jaka jest różnica w ocenie seriali x i *?
# x - serial z Zadania 1
# * - drugi wybrany serial


# 1) Zaawansowane atrybuty  

ggplot(countries, aes(x = birth.rate, y = death.rate, color = continent)) +
  geom_point()

ggplot(countries, aes(x = birth.rate, y = death.rate, color = continent)) +
  geom_density2d()

ggplot(countries, aes(x = birth.rate, y = death.rate, color = continent)) +
  geom_density2d() +
  geom_point()

## stat

ggplot(countries, aes(x = birth.rate, y = death.rate, fill = continent)) +
  stat_density2d(geom = "polygon")


ggplot(countries, aes(x = birth.rate, y = death.rate, fill = continent)) +
  stat_density2d(geom = "polygon", color = "black", alpha = 0.2) 

# 2) Transformacje danych

countries_labeled <- countries %>% 
  mutate(label_for_plot = ifelse(death.rate %in% c(min(death.rate), max(death.rate)), country, ""))

ggplot(countries_labeled, aes(x = birth.rate, y = death.rate, label = label_for_plot)) +
  geom_point() +
  geom_text()

install.packages("ggrepel")
library(ggrepel)

ggplot(countries_labeled, aes(x = birth.rate, y = death.rate, label = label_for_plot)) +
  geom_point() + 
  geom_text_repel()

ggplot(countries_labeled, aes(x = birth.rate, y = death.rate, label = label_for_plot)) +
  geom_point() + 
  geom_text_repel()

ggplot(countries_labeled, aes(x = birth.rate, y = death.rate, label = country)) +
  geom_point() + 
  geom_text_repel()

# 3) Wiele wykresow na jednym rysunku 

install.packages("gridExtra")
library(gridExtra)

p <- ggplot(countries, aes(x = birth.rate, y = death.rate, color = continent)) +
  geom_point() +
  geom_smooth(se = FALSE)
p

grid.arrange(p + coord_cartesian(xlim = c(5, 10)) + ggtitle("coord_cartesian"),
             p + scale_x_continuous(limits = c(5, 10)) + ggtitle("scale_continous - limits"),
             ncol = 1)

## Wykresy gęstości brzegowych

main_plot <- ggplot(data = countries, aes(x = birth.rate, y = death.rate, color = continent)) +
  geom_point()

density_death <- ggplot(data = na.omit(countries), aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  coord_flip() +
  scale_y_reverse() +
  theme(legend.position = "none")


density_birth <- ggplot(data = na.omit(countries), aes(x = birth.rate, fill = continent)) +
  geom_density(alpha = 0.2) +
  scale_y_reverse() +
  theme(legend.position = "none")

grid.arrange(density_death, main_plot, density_birth, ncol = 2)


install.packages("patchwork")
library(patchwork)

p1 <- ggplot(countries, aes(x = continent, y = death.rate)) +
  geom_boxplot()

p2 <- ggplot(countries, aes(x = continent, y = death.rate)) +
  geom_point(position = "jitter")

p3 <- ggplot(countries, aes(x = continent)) +
  geom_bar()

p1 + p2
p1 / p2
(p1 + p2) / p3

((p1 + p2) / p3) & theme_bw()

density_death + main_plot + plot_spacer() + density_birth +
  plot_layout(ncol = 2, heights = c(0.7, 0.3), widths = c(0.3, 0.7))


# 4) ggstatsplot

#https://github.com/IndrajeetPatil/ggstatsplot

# 5) ggmap

#https://github.com/dkahle/ggmap

