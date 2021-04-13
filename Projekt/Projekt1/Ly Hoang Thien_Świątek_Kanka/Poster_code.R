#Data source: https://www.kaggle.com/mariotormo/complete-pokemon-dataset-updated-090420?select=pokedex_%28Update_05.20%29.csv

# Import needed libraries:
library(dplyr)




# Import data set:
library(readr)
pokedex <- read.csv("C:/Users/patry/OneDrive/Pulpit/wstep_eksploracja_danych/pokedex_(Update_05.20).csv")





View(pokedex %>% filter(total_points==505))



# Cleaning data set and remove redundant columns:
pokedex1 <- pokedex %>% select(-german_name,-japanese_name,-egg_type_1,-egg_type_2,ability_1,-ability_2,
                               -ability_hidden,-generation,-egg_type_number,-egg_cycles,-catch_rate,-base_friendship,-base_experience)
str(pokedex1)
#unique(pokedex1$status)
#unique(pokedex1$generation)



#colnames(pokedex1)
#pokedex1 %>% filter(status=="Sub Legendary"|status=="Legendary"|status=="Mythical") %>% View()
#View(pokedex1)




# Chart of Sub Legendary & Legendary & Mythical pokemon by the generation!




#View(pokedex1)






# Lightest & Heaviest Pokemons:
#pokedex1 %>% select(name,weight_kg) %>% arrange(desc(weight_kg))
# Cosmoem, Celesteela, Primal Groudon 
#pokedex1 %>% select(name,weight_kg) %>% arrange(weight_kg)
# Gastly, Haunter, Flabébé, Cosmog, Kartana




# Pokemon the most well-rounded with total_point:
# pokedex1 %>% select(name,total_points) %>% arrange(desc(total_points))
# Eternatus Eternamax  
# pokedex1 %>% select(name,total_points) %>% arrange(total_points)
# Wishiwashi Solo Form 




# Pokemon with highest & lowest HP:
#pokedex1 %>% select(name,hp) %>% arrange(desc(hp))
# Highest: Blissey & Eternatus Eternamax
#pokedex1 %>% select(name,hp) %>% arrange(hp)
# Lowest:  Shedinja




# Pokemon with highest & lowest attack:
#pokedex1 %>% select(name,attack) %>% arrange(desc(attack))
# Mega Mewtwo X  
#pokedex1 %>% select(name,attack) %>% arrange(attack)
# Chansey, Happiny  




# Fastest & Slowliest pokemon:
#pokedex1 %>% select(name,speed) %>% arrange(desc(speed))
# Deoxys Speed Forme  
#pokedex1 %>% select(name,speed) %>% arrange(speed)
# Shuckle, Munchlax, Pyukumuku 



pokedex1 <- pokedex1 %>% select(-growth_rate)




# Distribution of pokemons respect to their original type:



pokedex1 %>% select(name,type_1) %>% group_by(type_1) %>% summarise(num_of_pok=n()) %>% arrange(desc(num_of_pok))



library(waffle)
library(magrittr)
library(hrbrthemes)
library(ggplot2)
library(dplyr)
library(waffle)



packageVersion("waffle")



pokemon_types <- c("Water"=134,"Normal"=115,"Grass"=91,"Bug"=81,"Psychic"=76,
                   "Fire"=65,"Electric"=61,"Rock"=60,"Dark"=44,"Ghost"= 41,"Ground"=41,
                   "Dragon"=40,"Poison"=39,"Fighting"=38,"Ice"=36,"Steel"=36,"Fairy"=22,"Flying"=8)



View(pokemon_types)



waffle(pokemon_types/8, rows=8, size=0.3,
       colors=c("#53AFFE","#BBBDAF","#8ED752","#C3D221","#FB61B4",
                "#F95643","#F8E64E","#CDBD72","#8E6856","#7673DA",
                "#F0CA42","#8B76FF","#AD5CA2","#A35449","#66EBFF",
                "#C3C1D7","#F9AEFE","#75A4F9"), title="Distribution of pokemons' types",
       xlab = "1 square ~ 10 pokemons")





####### Ternary Plot for Pokemon:



## Firstly, we define the strong, average and weak pokemons based on the total_point of that pokemon
## NOTICE that: total_point= hp+ attack+ defense + speed + sp_attack+ sp_defense + speed.
summary(pokedex1$total_points)
quantile(pokedex1$total_points,c(.15,.50,.85))



# 15-th Percentile = 300, Median = 455, 85-th Percentile = 555
# That's the reason we propose the definition of: 
# Weak pokemons: are pokemons with total_point <= 300=
# Average pokemons: are pokemons with 300< total_point < 555.
# Strong pokemons:  are pokemons with 555 <= total_point
View(pokedex1)
pokedex_ternary <- pokedex1 %>% select(-height_m,-weight_kg,-(percentage_male:against_fairy),-(species:ability_1))
View(pokedex_ternary)



pokedex_ter_reduced<-pokedex_ternary %>% mutate(classify= ifelse(total_points<=300,"Weak",ifelse(total_points<555,"Average","Strong")))



View(pokedex_ter_reduced)



View(pokedex_ter_reduced %>% filter(total_points==220))



## Heat map to show the correlation between indices:
#pokadex_ter_heat <- pokedex_ternary %>% select("total_points","hp","attack","defense","sp_attack","sp_defense","speed")



#matrix_heat <- round(cor(pokadex_ter_heat),2)
#head(matrix_heat)



#library(reshape2)
#library(ggplot2)



#melt_matrix_heat <- melt(matrix_heat)
#View(melt_matrix_heat)
#ggplot(data = melt_matrix_heat, aes(x=Var1,y=Var2,fill=value)) + geom_tile() +
#  scale_fill_viridis_c(option = "B", direction = -1)+
#  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) + 
#  theme(axis.title.x = element_blank(),
#        axis.title.y = element_blank(),
#        panel.grid.major = element_blank(),
#        panel.border = element_blank(),
#        panel.background = element_blank())



## Hence, we choose attack, speed and hp as 3 main components for the ternery chart.
library(ggtern)
ggtern(data= pokedex_ter_reduced,aes(x=attack,y=hp,z=defense,color=classify)) + 
  geom_point()
View(pokedex_ter_reduced)        



ggtern(data= pokedex_ter_reduced,aes(x=attack,y=hp,z=defense,color=classify)) + 
  geom_point()+ theme_showarrows() 




#Radar charts

library('dplyr')
library('stringi')
library(fmsb)
library(ggplot2)
library(tidyverse)
library("ggradar")

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4))
colors_in


#squirtle

squirtle <- pokedex %>% 
  filter(name == "Squirtle" | name == "Blastoise") %>% 
  select(name, hp:catch_rate)
squirtle

rownames(squirtle) <- squirtle$name
squirtle <- squirtle[,-1]

squirtle2 <- rbind(rep(40,7), rep(110,7), squirtle) 
squirtle2
squirtle3 <- squirtle2[-c(1,2),]
squirtle3
squirtle3[2:1,]


trawa.file <- file.path("C:/Users/patry/OneDrive/Pulpit/trawa.jpg")
trawa_wykres <- jpeg::readJPEG(trawa.file)

radarchart(squirtle2,
           pcol=colors_border , pfcol=colors_in[1:2] , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
           vlcex=0.8)

legend(x=0.9, y=0.6,legend = rownames(squirtle3[2:1,]), 
       bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.1, pt.cex=4)

ggplot(squirtle) +
  ggradar(
    values.radar = c("40","70","110"),
    grid.min = 40, grid.mid = 70, grid.max = 110,
    # Polygons
    group.line.width = 1, 
    group.point.size = 3,
    group.colours = c("#00AFBB", "#E7B800", "#FC4E07"),
    # Background and grid lines
    background.circle.colour = "white",
    gridline.mid.colour = "grey",
    legend.position = "bottom",
  ) + background_image = trawa_wykres



#charmander

charmander <- pokedex %>% 
  filter(name == "Charmander" | name == "Charizard") %>% 
  select(name, hp:catch_rate)

rownames(charmander) <- charmander$name
charmander <- charmander[, -1]

charmander
charmander2 <- rbind(rep(40,7), rep(115,7), charmander)  
charmander2
charmander2[-c(1,2),]
charmander3 <- charmander2[-c(1,2),]
charmander3

radarchart(charmander2,
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
           vlcex=0.8)

legend(x=0.9, y=0.6,legend = rownames(charmander3[2:1]), 
       bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.1, pt.cex=4)


#bulbasaur

bulbasaur <- pokedex %>% 
  filter(name == "Bulbasaur" | name == "Mega Venusaur") %>% 
  select(name, hp:catch_rate)
rownames(bulbasaur) <- bulbasaur$name
bulbasaur <- bulbasaur[, -1]
bulbasaur

bulbasaur2 <- rbind(rep(125,7) , rep(40,7) , bulbasaur)
library(RColorBrewer)

library(scales)
bulbasaur3 <- bulbasaur2[-c(1,2),]
bulbasaur3

radarchart(bulbasaur2,
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
           vlcex=0.8)

legend(x=0.9, y=0.6,legend = rownames(bulbasaur3), 
       bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.1, pt.cex=4)