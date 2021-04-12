library("dplyr")
library(ggplot2)
setwd("C:/_zc/wded")
characters <- read.csv("characters.csv")
charactersToComics <- read.csv("charactersToComics.csv")
charcters_stats <- read.csv("charcters_stats.csv")
comics <- read.csv("comics.csv")
marvel_characters_info <- read.csv("marvel_characters_info.csv")
marvel_dc_characters <- read.csv("marvel_dc_characters.csv")
superheroes_power_matrix <- read.csv("superheroes_power_matrix.csv")

calosc <- inner_join(charcters_stats, marvel_characters_info, by = "Name") %>%
  inner_join(superheroes_power_matrix, by = "Name")

# Oczy
calosc %>%
  filter(Total > 5) %>%
  group_by(EyeColor) %>%
  summarize(sila = round(mean(Total, na.rm = T), digits = 1), licznosc = n()) %>%
  filter(licznosc > 1, EyeColor != '-') %>%
  select(- licznosc) %>%
  arrange(desc(sila)) %>%
  head(5) %>%
  ggplot(mapping = aes(x = reorder(EyeColor,-sila), y = sila)) +
    geom_bar(stat = "identity") +
    labs(x = "Eye Color", y = "Power") +
    geom_text(aes(label=sila),
            position=position_dodge(width=0.9), vjust = -0.5) +
    theme(axis.text.y = element_text(size = 12))

# Włosy
calosc %>%
  filter(Total > 5) %>%
  group_by(HairColor) %>%
  summarize(sila = round(mean(Total, na.rm = T), digits = 1), licznosc = n()) %>%
  filter(licznosc > 1, HairColor != '-') %>%
  select(- licznosc) %>%
  arrange(desc(sila)) %>%
  head(5) %>%
  ggplot(mapping = aes(x = reorder(HairColor,-sila), y = sila)) +
    geom_bar(stat = "identity") +
    labs(x = "Hair Color", y = "Power") +
    geom_text(aes(label=sila),
            position=position_dodge(width=0.9), vjust = -0.5) +
  theme(axis.text.y = element_text(size = 12))

# Płeć
calosc %>%
  filter(Total > 5) %>%
  group_by(Gender) %>%
  summarize(sila = round(mean(Total, na.rm = T), digits = 1)) %>%
  arrange(desc(sila)) %>%
  ggplot(mapping = aes(x = reorder(Gender,-sila), y = sila)) +
    geom_bar(stat = "identity") +
    labs(x = "Gender", y = "Total power") +
    geom_text(aes(label=sila),
            position=position_dodge(width=0.9), vjust = -0.5) +
    theme(axis.text.y = element_text(size = 12))

# Skóra
calosc %>%
  filter(Total > 5) %>%
  group_by(SkinColor) %>%
  summarize(sila = round(mean(Total, na.rm = T), digits = 1), licznosc = n()) %>%
  filter(licznosc > 1, SkinColor != '-') %>%
  select(- licznosc) %>%
  arrange(desc(sila)) %>%
  head(5) %>%
  ggplot(mapping = aes(x = reorder(SkinColor,-sila), y = sila)) +
    geom_bar(stat = "identity") +
    labs(x = "Skin color", y = "Power") +
    geom_text(aes(label=sila),
            position=position_dodge(width=0.9), vjust = -0.5) +
    theme(axis.text.y = element_text(size = 12))


# Czy jest dobry czy zły
calosc %>%
  filter(Total > 5) %>%
  filter(Alignment.y == 'bad' | Alignment.y == "good") %>%
  group_by(Alignment.y) %>%
  summarize(sila = round(mean(Total, na.rm = T), digits = 1)) %>%
  arrange(desc(sila)) %>%
  ggplot(mapping = aes(x = reorder(Alignment.y,-sila), y = sila)) +
  geom_bar(stat = "identity") +
  labs(x = "Alignment", y = "Power") +
  geom_text(aes(label=sila),
            position=position_dodge(width=0.9), vjust = -0.5) +
  theme(axis.text.y = element_text(size = 12))

