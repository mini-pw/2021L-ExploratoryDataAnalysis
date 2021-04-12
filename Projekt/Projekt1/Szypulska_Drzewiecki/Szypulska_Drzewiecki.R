library(gtrendsR)
library(ggplot2)
library(dplyr)
library(maps)
library(sf)

miesiace <- c('Jul 2020','Aug 2020','Sep 2020','Oct 2020', 'Nov 2020','Dec 2020', 'Jan 2021', 'Feb 2021')
gry_fp <- c(127417878,126188966, 125931456,124943149, 158261639  ,203853206, 230524668, 261186236)
gracze_fp <- c(5288318,5198672, 5283581, 5266420,6965852, 9053655, 10359219, 11540956)
chess_com <- data_frame(miesiace, gry_fp, gracze_fp)
chess_com$miesiace <- factor(chess_com$miesiace, levels =  c('Jul 2020','Aug 2020','Sep 2020','Oct 2020','Nov 2020', 'Dec 2020', 'Jan 2021', 'Feb 2021'))

options(scipen = 999)

theme_set(theme_bw())

## Lollipop Plot
ggplot(chess_com, aes(x=miesiace, y=gracze_fp)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=miesiace, 
                   xend=miesiace, 
                   y=0, 
                   yend=gracze_fp)) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,15000000)) +
  labs(title = "Accounts reviewed for fair play",
       subtitle = "Source: chess.com", 
       x = " ",
       y = "accounts") +
  theme(axis.text = element_text(size = 9), 
        plot.subtitle = element_text(size = 7.5, hjust = 1),
        plot.title = element_text(face = "bold", size = 14))

ggplot(chess_com, aes(x=miesiace, y=gry_fp)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=miesiace, 
                   xend=miesiace, 
                   y=0, 
                   yend=gry_fp)) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,300000000)) +
  labs(title = "Games reviewed for fair play",
       subtitle = "Source: chess.com", 
       x = " ",
       y = " ") +
  theme(axis.text = element_text(size = 9), 
        plot.subtitle = element_text(size = 7.5, hjust = 1),
        plot.title = element_text(face = "bold", size = 14))

## Map

how_to_chess <- gtrends(keyword = c("how to play chess"),time = "2020-03-01 2021-02-01", geo="US")
how_to_chess_time <- how_to_chess$interest_over_time

mapka <- map_data("state")
regiony <- how_to_chess$interest_by_region
regiony <- rename(regiony, region = location)
regiony$region <- tolower(regiony$region)
stany <- inner_join(mapka, regiony, by = "region")
stany
p <- ggplot()
p + geom_polygon(data=stany, 
                 aes(x=long, y=lat, group=group, fill = hits), 
                 color="white", size = 0.7) +
  scale_fill_gradient(low="white", high="#5000d9")


## Trends

amazon_chess <- gtrends(keyword = c("chess amazon"),time = "2020-03-01 2021-02-01", geo="US")
amazon_chess_time <- amazon_chess$interest_over_time

amazon_play <- gtrends(keyword = c("how to play chess", "chess amazon"),
                       time = "2020-03-01 2021-02-01", geo="US")


amazon_play <- amazon_play$interest_over_time

amazon_play %>%
  ggplot(aes(x = date, y = hits, colour = keyword)) +
  geom_line(size = 1.1)  +
  labs(title = "Popularity of search 'chess amazon' 
       & 'how to play chess' in US",
       subtitle = "Source: GoogleTrends", 
       x = " ",
       y = " ") +
  theme(axis.text = element_text(size = 8), 
        plot.subtitle = element_text(size = 7.5, hjust = 2.2),
        plot.title = element_text(face = "bold", size = 14, hjust = 0))


## Twitch


ggplot(twitch, aes(x = Month, y = Average)) +
  geom_col() +
  scale_y_continuous(expand = c(0,0), limits = c(0,35000)) +
  labs(title = "Concurrent viewers - Average",
       subtitle = "Source: twitchtracker.com", 
       x = " ",
       y = "average of concurrent viewers") +
  theme(axis.text = element_text(size = 9), 
        plot.subtitle = element_text(size = 7.5, hjust = 1),
        plot.title = element_text(face = "bold", size = 14))


ggplot(twitch, aes(x = Month, y = Peak)) +
  geom_point() +
  scale_y_continuous(expand = c(0,0), limits = c(0,400000)) +
  labs(title = "Concurrent viewers - Peak",
       subtitle = "Source: twitchtracker.com", 
       x = " ",
       y = "peak of concurrent viewers") +
  theme(axis.text = element_text(size = 9), 
        plot.subtitle = element_text(size = 7.5, hjust = 1),
        plot.title = element_text(face = "bold", size = 14))

