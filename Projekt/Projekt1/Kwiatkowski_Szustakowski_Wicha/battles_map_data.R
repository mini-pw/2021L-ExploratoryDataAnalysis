battles <- read.csv("C:\\Users\\pc\\Desktop\\EDAprojekt1datasets\\battles.csv")
library(ggplot2)
library(dplyr)
library(tidyr)

region_table <- full_join(location_table_win, location_table_loss, by = "region")
region_table <- region_table %>% 
  replace(is.na(.),0) %>% 
  rename(Attacker_win = n.x) %>% 
  rename(Defender_win = n.y) 
region_table_battle <- region_table %>% 
  mutate(Total = Attacker_win + Defender_win) %>% 
  select(region, Total)
region_table_battle




