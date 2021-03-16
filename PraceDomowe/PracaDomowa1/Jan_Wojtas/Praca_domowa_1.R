# Praca domowa 1 , Jan Wojtas

install.packages("PogromcyDanych")
library(PogromcyDanych)
library(dplyr)
library(tidyr)
library(tidyverse)


View(auta2012)




#1.SprawdŸ ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?


auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)",Rok.produkcji == 2007) %>% 
  summarise(Liczba_aut_diesel=n())


# ODP: 11621



#2.Jakiego koloru auta maj¹ najmniejszy medianowy przebieg?


auta2012 %>% 
  group_by(Kolor) %>% 
  summarise (mediana_przebiegu = median(Przebieg.w.km,na.rm=TRUE)) %>% 
  arrange(mediana_przebiegu) %>% 
  head(1)


# ODP: bialy-metallic 



#3.Gdy ograniczyæ siê tylko do aut wyprodukowanych w 2007, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?


auta2012 %>% 
  filter(Rok.produkcji == 2007) %>% 
  group_by(Marka) %>% 
  summarise (Licznoœæ = n()) %>% 
  arrange(desc(Licznoœæ)) %>% 
  head(1)


# ODP: Volkswagen


#4.Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtañsza?


auta2012 %>% 
  filter(Rok.produkcji == 2007, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(œrednia = mean(Cena.w.PLN,na.rm=TRUE)) %>% 
  arrange(œrednia) %>% 
  head(1)


# ODP: Aixam



#5.Spoœród aut marki Toyota, który model najbardziej straci³ na cenie pomiêdzy rokiem produkcji 2007 a 2008.


datafr1 <- auta2012 %>% 
  filter(Marka == "Toyota", Rok.produkcji == c(2007,2008)) %>% 
  group_by(Model, Rok.produkcji) %>%
  summarise(œrednia_cena = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Rok.produkcji, values_from = œrednia_cena)
colnames(datafr1)[2:3] <- c("Rok_2007","Rok_2008")
datafr1 %>% 
  mutate(ró¿nica_cen_2007_2008 = Rok_2007-Rok_2008) %>%
  arrange(desc(ró¿nica_cen_2007_2008)) %>% 
  head(1)


# ODP: Matrix


#6.W jakiej marce klimatyzacja jest najczêœciej obecna?


 auta2012 %>% 
   filter(str_detect(Wyposazenie.dodatkowe,"klimatyzacja")) %>% 
   group_by(Marka) %>% 
   summarise(n=n()) %>% 
   top_n(1,n)
   

# ODP: Volkswagen


#7.Gdy ograniczyæ siê tylko do aut z silnikiem ponad 100 KM, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?


auta2012 %>% 
  filter(KM > 100) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(1)


# ODP: Volkswagen


#8.Gdy ograniczyæ siê tylko do aut o przebiegu poni¿ej 50 000 km o silniku diesla, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?


auta2012 %>% 
  filter(Przebieg.w.km < 50000, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  top_n(1,n)


#ODP: BMW


#9.Spoœród aut marki Toyota wyprodukowanych w 2007 roku, który model jest œrednio najdro¿szy?


auta2012 %>% 
  filter(Marka == "Toyota", Rok.produkcji == 2007) %>% 
  group_by(Model) %>% 
  summarise(œrednia_cena = mean(Cena.w.PLN,na.rm=TRUE)) %>% 
  top_n(1,œrednia_cena)


#ODP: Land Cruiser


#10.Spoœród aut marki Toyota, który model ma najwiêksz¹ ró¿nicê cen gdy porównaæ silniki benzynowe a diesel?
  

silniki <- auta2012 %>% 
  filter(Marka =="Toyota", Rodzaj.paliwa == c("benzyna","olej napedowy (diesel)")) %>% 
  group_by(Model,Rodzaj.paliwa) %>% 
  select(Cena.w.PLN,Model, Rodzaj.paliwa) %>% 
  summarise(n=mean(Cena.w.PLN,na.rm=TRUE)) %>% 
  pivot_wider(names_from = Rodzaj.paliwa, values_from = n)
  
colnames(silniki)[3] <- "diesel" 
silniki %>% 
  mutate(ró¿nica_cen_silników = abs(diesel - benzyna)) %>% 
  arrange(desc(ró¿nica_cen_silników)) %>% 
  head(1)
  

#ODP: 4-Runner