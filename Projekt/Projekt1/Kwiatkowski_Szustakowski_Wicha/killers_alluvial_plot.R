library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(showtext)

`%nin%` <- function(a,b) ! a %in% b

death <- read.csv("C:\\Users\\jedrek\\Documents\\Eksploracja_danych_2021\\GotDeaths.csv")

font_add(family = "got", regular = "C:\\Users\\jedrek\\Documents\\Eksploracja_danych_2021\\GoT.ttf")
showtext_auto()

generalise_allegiance <- function(allegiance) {
  if(grepl("House Baratheon", allegiance))
    result <- "House Baratheon"
  else
    result <- allegiance
   
  if(result %nin% c("House Lannister", "House Baratheon", "Night's Watch", "Sons of the Harpy", 
                    "House Bolton", "House Frey"))
    result <- "Other"
  if(result == "Night's Watch")
    result <- "Nights Watch"
  
  result
}

generalise_killers <- function(killer) {
  if(killer %nin% c("Cersei Lannister", "Arya Stark", "Jon Snow", "Sandor Clegane", 
                    "Grey Worm", "Bronn", "Daario Naharis", "Tormund", "Ramsay Bolton"))
    result <- "Other"
  else
    result <- killer
    
  result
}

top2to9killers <- death %>% 
  rowwise() %>% 
  mutate(Killer = generalise_killers(Killer)) %>%
  mutate(Allegiance = generalise_allegiance(Allegiance)) %>%
  filter(Killer != "Other") %>%
  group_by(Season, Allegiance, Killer) %>%
  summarise(n=n(), .groups="keep")

p <- ggplot(as.data.frame(top2to9killers),
       aes(y=n, axis1 = Killer, axis2 = Allegiance, axis3 = Season)) +
  geom_alluvium(aes(fill = Killer),
                width = 0, knot.pos = 0.5, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/12, color = "#dddddd", reverse = FALSE, size=1, fill='#999999') +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE, color="white", family='got', size = 12) +
  scale_x_discrete(limits = c("Killer", "Victims Allegiance", "Season"), expand=c(0.5,0.5)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("The Top Killers")

p <- p +
  theme(
    plot.title = element_text(color="white", family="got", size=35),
    axis.text.x = element_text(color="white", family="got", size=25),
    axis.text.y = element_text(color="white", family="got", size=25),
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
p
ggsave(p, height=25, units='cm', dpi=300, filename = "C:\\Users\\jedrek\\Documents\\Eksploracja_danych_2021\\TheTopKillers.png",  bg = "transparent")

daenerys <- death %>% filter(Killer == "Daenerys Targaryen")

daenerys %>% mutate(Cathegory = Allegiance) %>% group_by(Cathegory) %>% summarise(n=n()) %>% top_n(1)
daenerys %>% mutate(Cathegory = Season) %>% group_by(Cathegory) %>% summarise(n=n()) %>% top_n(1)
daenerys %>% mutate(Cathegory = Location) %>% group_by(Cathegory) %>% summarise(n=n()) %>% top_n(1)
daenerys %>% mutate(Cathegory = Method) %>% group_by(Cathegory) %>% summarise(n=n()) %>% top_n(1)
