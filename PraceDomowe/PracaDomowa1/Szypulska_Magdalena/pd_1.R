
library(PogromcyDanych)
library(dplyr)
auta2012


#1.Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?

auta2012 %>%
  filter(Rok.produkcji == 2007 & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  summarise(n = n())

#11621

#2.Jakiego koloru auta mają najmniejszy medianowy przebieg?

auta2012 %>%
  group_by(Kolor) %>%
  summarise(przebieg_mediana = median(Przebieg.w.km, na.rm = TRUE)) %>%
  arrange(przebieg_mediana) %>%
  head(1)

#bialy-metallic       


#3.Gdy ograniczyć się tylko do aut wyprodukowanych w 2007, która Marka występuje najczęściej w zbiorze danych auta2012?
 
auta2012 %>%
  group_by(Marka) %>% 
  filter(Rok.produkcji == 2007) %>%
  summarise(n = n()) %>%
  arrange(-n)  %>%
  select(Marka) %>%
  head(1)

#Volkswagen
 
#4.Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?

auta2012 %>%
  filter(Rok.produkcji == 2007 & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>%
  summarise(srednia_cena = mean(Cena.w.PLN)) %>%
  arrange(srednia_cena) %>%
  select(Marka) %>%
  head(1)

#Aixam

#5.Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.

Toyota2007 <- auta2012 %>% 
  filter(Marka == "Toyota", Rok.produkcji == 2007) %>% 
  group_by(Model) %>% 
  summarise(srednia_cena_2007 = mean(Cena.w.PLN))

Toyota2008 <- auta2012 %>% 
  filter(Marka == "Toyota", Rok.produkcji == 2008) %>% 
  group_by(Model) %>% 
  summarise(srednia_cena_2008 = mean(Cena.w.PLN))

inner_join(Toyota2008, Toyota2007, by = "Model") %>% 
  mutate(Roznica = srednia_cena_2008 - srednia_cena_2007) %>% 
  arrange(-Roznica) %>% 
  head(1) %>%
  select(Model)

#Land Cruiser
  

#W jakiej marce klimatyzacja jest najczęściej obecna?

auta2012 %>% 
  group_by(Marka) %>% 
  summarise(srednia = mean(grepl("klimatyzacja", Wyposazenie.dodatkowe))) %>% 
  arrange(-srednia) %>%
  head(7) %>%
  select(Marka)

#Brilliance, DFSK, GMC, Saturn, Scion, Shuanghuan, Vauxhall  
 

  
#Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>%
  group_by(Marka) %>%
  filter(KM > 100) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  head(1) %>%
  select(Marka)

#Volkswagen

#Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>% 
  filter(Przebieg.w.km < 50000 & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  head(1) %>%
  select(Marka)

#BMW

#Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?

auta2012 %>%
  group_by(Model) %>%
  filter(Marka == "Toyota" & Rok.produkcji == 2007)  %>%
  summarise(cena_srednia = mean(Cena.w.PLN)) %>%
  arrange(-cena_srednia) %>%
  head(1) %>%
  select(Model)

#Land Cruiser

#Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?


Toyota_diesel <- auta2012 %>% 
  filter(Marka == "Toyota", Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarise(srednia_diesel = mean(Cena.w.PLN))

Toyota_benzyna <- auta2012 %>% 
  filter(Marka == "Toyota", Rodzaj.paliwa == "benzyna") %>% 
  group_by(Model) %>% 
  summarise(srednia_benzyna = mean(Cena.w.PLN))

inner_join(Toyota_diesel, Toyota_benzyna, by = "Model") %>% 
  mutate(roznica = abs(srednia_benzyna - srednia_diesel)) %>% 
  arrange(-roznica) %>% 
  head(1) %>%
  select(Model)

#Camry
  
