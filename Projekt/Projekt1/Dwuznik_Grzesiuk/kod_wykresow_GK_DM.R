#Same wywo³ania wykresóW s¹ stosunkwo proste, przez wzgl¹d na to, ¿e od razu zak³adaliœm, ¿e wiêkszosæ parametróW takich jak kolor
#itd. bêdziemy zmieniaæ za pomoc¹ programów graficznych, bo to i tak byloby konieczne w celu uzyskania lepszego efektu. 
#Wiêkszoœæ pracy przypad³a na etap wstêpnej analizy i obróbki graficznej.
############## WORDCLOUD ##########################################################################################################

wordcloud %>%
  anti_join(mystopwords,by = "Words") %>%
  head(100)%>%
  select(Words,n) %>%
  wordcloud2(size=0.8,color=rep(brewer.pal(8, "OrRd"),10),backgroundColor = "black",
             fontFamily = "Roman Wood Type JNL",gridSize=0,
             minRotation = -pi/10, maxRotation = pi/10,ellipticity = 0.45)


############## TOMMY WYKRES ##################################################################################################

ggplot(tommy_graph,aes(x=Words,y=n)) + 
  geom_col()+
  theme_wsj()+ 
  scale_colour_wsj("colors6")+
  ggtitle("Characters")

############## GIN #############################################################################################################

gin_graph %>%
  ggplot(aes(x=Season, y=n)) + 
  geom_point(size=6)+
  geom_line(size=1.05)+
  theme_wsj()+ 
  scale_colour_wsj("colors6")+
  ggtitle("Gin")

############# WYKRESY U¯YWEK #############################################################
ggplot(temp,aes(x=Episode,y=liczba)) + 
  geom_col()+
  facet_wrap(~Season)+
  theme_wsj()+ 
  scale_colour_wsj("colors6")+
  ggtitle("Drugs")
