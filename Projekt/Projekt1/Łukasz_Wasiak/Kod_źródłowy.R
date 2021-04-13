library('dplyr')
library('tm')
library('wordcloud2')
library('data.table')
library('stringi')
library(ggplot2)
library(tidytext)


p1 <- scripts %>% 
  filter(char == 'GANDALF')
p2 <- scripts %>% 
  filter(char == 'GOLLUM' | char=='SMEAGOL')
p3 <- scripts %>% 
  filter(char=='FRODO')


####################################################CHMURKA1
corpus1 <- Corpus(VectorSource(p1[[2]])) #zamienia dialogi na typ danych z tego pakietu do slow
corpus1cleared <- corpus1 %>% #oczyszczanie
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, c(stopwords('en'), 'will')) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace)

#zliczanie slow

tmp1 <- TermDocumentMatrix(corpus1cleared)
m <- as.matrix(tmp1)
z <- sort(rowSums(m),decreasing=TRUE)
qw <- data.frame(word = names(z),freq=z)   
colors<-rep_len(c("#772F1A", "#570000",'#1D1A05','#BB4430'), nrow(qw))
wordcloud2(qw,size = 0.8, gridSize = 0.2,figPath='gandalf2.png',color = colors, ellipticity = 0.8, backgroundColor = '#EFE6DD')

####################################################CHMURKA2
corpus2 <- Corpus(VectorSource(p2[[2]]))

corpus2cleared <- corpus2 %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords('en')) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace)

tmp1 <- TermDocumentMatrix(corpus2cleared)
m <- as.matrix(tmp1)
z <- sort(rowSums(m),decreasing=TRUE)
qw <- data.frame(word = names(z),freq=z)
colors<-rep_len(c("#772F1A", "#570000",'#1D1A05','#BB4430'), nrow(qw))
wordcloud2(qw,size = 0.8, gridSize = 0.2,figPath='azx.png',color = colors, ellipticity = 0.8, backgroundColor = '#EFE6DD')



############################################### Frodo
corpus3 <- Corpus(VectorSource(p3[[2]]))

corpus3cleared <- corpus3 %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords('en')) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace)

tmp1 <- TermDocumentMatrix(corpus3cleared)
m <- as.matrix(tmp1)
z <- sort(rowSums(m),decreasing=TRUE)
qw <- data.frame(word = names(z),freq=z)
colors<-rep_len(c("#772F1A", "#F58549",'#F2A65A','#EEC170'), nrow(qw))
wordcloud2(qw,size = 1, gridSize = 0.5,color = colors, maxRotation = pi/2)


## Najbardziej rozgadane postacie

WBC <- WordsByCharacter %>% 
  mutate(NAME=toupper(Character)) %>% 
  group_by(NAME) %>% 
  mutate(n=sum(Words)) %>% 
  select(NAME, n, Race) %>% 
  unique() %>% 
  arrange(-n) %>% 
  head(15)

ggplot(WBC, aes(y=as.factor(reorder(NAME,n)), x=n, colour=Race, fill=Race))+
  geom_bar(stat='identity')+
  theme_minimal()+
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1))+
  scale_fill_manual(values = c(rgb(244, 184, 83, max=255),rgb(239, 118, 26,max=255), rgb(205, 0, 0, max=255),
                               rgb(130, 4, 15, max=255), rgb(90, 50, 16, max=255),
                               rgb(0, 114, 0, max=255), rgb(70,180,0,max=255)))+
  scale_color_manual(values = c(rgb(244, 184, 83, max=255),rgb(239, 118, 26,max=255), rgb(205, 0, 0, max=255),
                                rgb(130, 4, 15, max=255), rgb(90, 50, 16, max=255),
                                rgb(0, 114, 0, max=255), rgb(70,180,0,max=255)))+
  labs(y = NULL, x = NULL, title = "Number of words spoken by each character")


## Oœ czasu

WordsByCharacter %>% 
  filter(Character=="Gandalf" | Character=="Sam" | Character=="Aragorn" | Character=="Frodo" | Character=="Bilbo" | Character=="Theoden" | Character=="Gollum") %>% 
  ggplot(aes(x=factor(Chapter,levels=c("01: Prologue", "02: Concerning Hobbits",
                                       "03: The Shire", "04: Very Old Friends","05: A Long Expected Party","06: Farewell Dear Bilbo", "07: Keep It Secret Keep It Safe",
                                       "08: The Account of Isildur", "09: At The Green Dragon", "10: The Shadow Of The Past","11: The Passing Of The Elves","12: Saruman The White",
                                       "13: A Shortcut To Mushrooms", "14: Buckleberry Ferry", "15: At The Sign of the Prancing Pony",
                                       "16: The Nazgul","17: The Midgewater Marshes","18: The Spoiling Of Isengard","19: A Knife In The Dark","20: The Caverns Of Isengard","21: Flight To The Ford",
                                       "22: Rivendell","23: Many Meetings","24: The Fate Of The Ring","25: The Sword That Was Broken", "26: The Evenstar","27: The Council Of Elrond",
                                       "28: Gilraen's Memorial","29: Bilbo's Gifts","30: The Departure Of The Fellowship","31: The Ring Goes South","32: The Pass Of Caradhras","33: Moria",
                                       "34: A Journey In The Dark","35: Balin's Tomb","36: The Bridge Of Khazad-dum","37: Lothlorien","38: Caras Galadhon","39: The Mirror Of Galadriel",
                                       "40: The Fighting Uruk-hai","41: Farewell To Lorien","42: The Great River","43: Parth Galen","44: The Breaking Of The Fellowship","45: The Departure Of Boromir",
                                       "46: The Road Goes Ever On?",
                                       "01: The Foundations Of Stone", "02: Elven Rope",
                                       "03: The Taming of Sm?agol", "04: The Uruk-Hai","05: The Three Hunters","06: The Burning of the Westfold", "07: Massacre at the Fords of Isen",
                                       "08: The Banishment of ?omer", "09: On The Trail Of The Uruk-Hai", "10: Night Camp At Fangorn","11: The Riders Of Rohan","12: The Fate Of Merry And Pippin",
                                       "13: Treebeard", "14: The Passage Of The Marshes", "15: The White Rider",
                                       "16: The Song Of The Entwives","17: The Heir Of N?menor","18: The Black Gate Is Closed","19: Ent Draft","20: The King Of The Golden Hall","21: The Funeral Of Th?odred",
                                       "22: Simbelmyn? on the Burial Mound","23: The King's Decision","24: Brego","25: The Ring Of Barahir", "26: A Daughter Of Kings","27: Exodus From Edoras",
                                       "28: The Forests Of Ithilien","29: Gollum and Sm?agol","30: Of Herbs And Stewed Rabbit","31: Dwarf Women","32: One of the D?nedain","33: The Evenstar",
                                       "34: The Wolves Of Isengard","35: Helm's Deep","36: Isengard Unleashed","37: The Grace Of The Valar","38: Arwen's Fate","39: The Story Foreseen From L?rien",
                                       "40: The Windows On The West","41: Sons Of The Steward","42: The Forbidden Pool","43: Aragorn's Return","44: Entmoot","45: The Glittering Caves",
                                       "46: Where Is The Horse And The Rider","47: Don't Be Hasty Master Meriadoc!","48: The Host Of The Eldar","49: The Battle Of The Hornburg","50: Old Entish",
                                       "51: The Breach Of The Deeping Wall","52: The Entmoot Decides","53: The Retreat To The Hornburg","54: Master Peregrin's Plan","55: Osgiliath",
                                       "56: The Last March Of The Ents","57: The Nazgul Attack","58: Forth Eorlingas","59: The Flooding Of Isengard",
                                       "60: The Tales That Really Mattered?","61: Fangorn Comes To Helm's Deep","62: The Final Tally","63: Flotsam And Jetsam",
                                       "64: Farewell To Faramir","65: The Battle For Middle Earth Is About To Begin","66: Gollum's Plan",
                                       "01: The Finding Of The Ring", "02: Journey To The Cross-roads",
                                       "03: The Road To Isengard", "04: The Voice Of Saruman","05: Return To Edoras","06: Gollum's Villany", "07: Eowyn's Dream",
                                       "08: The Palantir", "09: Arwen's Vision", "10: The Reforging Of Narsil","11: Minas Tirith","12: The Decline Of Gondor",
                                       "13: Cross-roads Of The Fallen King", "14: The Deep Breath Before The Plunge", "15: Minas Morgul",
                                       "16: Sam's Warning","17: Pippin's Task","18: Osgiliath Invaded","19: The Lighting Of The Beacons","20: Theoren's Decision","21: The Fall of Osgiliath",
                                       "22: The Wizard's Pupil","23: The Stairs Of Cirith Ungol","24: Courage Is The Best Defense","25: Peregrin Of The Tower Guard", "26: Allegiance To Denethor","27: The Parting Of Sam & Frodo",
                                       "28: The Sacrifice Of Faramir","29: Marshalling At Dunharrow","30: Anduril - Flame Of The West","31: Aragorn Takes The Paths Of The Dead","32: No More Despair","33: Dwimoberg ~ The Haunted Mountain",
                                       "34: The Muster Of Rohan","35: The Paths Of The Dead","36: The Siege Of Gondor","37: The Corsairs Of Umbar","38: Shelob's Lair","39: Merry's Simple Courage",
                                       "40: Grond ~ The Hammer Of The Underworld","41: The Tomb Of The Stewards","42: Breaking The Gate Of Gondor","43: The Choices Of Master Samwise Gamgee","44: Denethor's Madness","45: The Witch King's Hour",
                                       "46: The Ride Of The Rohirrim","47: The Pyre Of Denethor","48: The Battle Of The Pelennor Fields","49: A Far Green Country","50: The Nazgul And His Prey",
                                       "51: The Black Ships","52: Shieldmaiden Of Rohan","53: Victory At Minar Tirith","54: The Passing Of Theoden","55: Oaths Fulfilled",
                                       "56: The Houses Of Healing","57: Pippin Looks After Merry","58: The Tower Of Cirith Ungol","59: The Last Debate",
                                       "60: Aragorn Masters The Palantir","61: The Captain And The White Lady","62: In The Company Of Orcs","63: The Land Of Shadow",
                                       "64: The Mouth Of Sauron","65: The Black Gate Opens","66: I Can't Carry It For You? But I Can Carry You",
                                       "67: The Last Move", "68: Mount Doom","69: The Eagles Are Coming",
                                       "70: The Crack Of Doom","71: Sauron Defeated","72: The End Of All Things","73: The Fellowship Reunited",
                                       "74: The Return Of The King","75: Homeward Bound","76: The Grey Havens")),
             y=factor(Character, levels=c("Gollum","Theoden","Bilbo","Frodo","Aragorn","Sam","Gandalf")),color=Character))+
  geom_point()+
  scale_color_manual(values = c(rgb(103, 56, 19, max=255),rgb(241, 140, 61 ,max=255), rgb(187, 80, 37, max=255),
                                rgb(56, 176, 0, max=255), rgb(184, 138, 62, max=255),
                                rgb(0, 114, 0, max=255), rgb(244, 184, 83,max=255)))+
  theme_minimal()+
  theme(axis.text.x=element_text(color="white"), legend.position = "none")+
  labs(y=NULL, x= NULL, title= "Timeline")