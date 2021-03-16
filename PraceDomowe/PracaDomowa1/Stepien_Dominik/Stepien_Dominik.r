library(PogromcyDanych)
library(dplyr)
library(tidyr)



## 1. Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
auta2012 %>% 
  filter(Rok.produkcji==2007) %>% 
  filter(Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  count()
  #Odp: 11621

## 2. Jakiego koloru auta mają najmniejszy medianowy przebieg?
auta2012 %>% 
  group_by(Kolor) %>% 
  summarise(medianowy_przebieg=median(Przebieg.w.km, na.rm=TRUE)) %>% 
  arrange(medianowy_przebieg)
  #Odp: bialy-metallic

## 3. Gdy ograniczyć się tylko do aut wyprodukowanych w 2007,
##    która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Rok.produkcji==2007) %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  arrange(-n)
  #Odp: Volkswagen

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku,
##    która marka jest najtańsza?
auta2012 %>% 
  filter(Rok.produkcji == 2007 & Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(średnia=mean(Cena.w.PLN)) %>% 
  arrange(średnia)
  #Odp: Aixam

## 5. Spośród aut marki Toyota, który model najbardziej stracił
##    na cenie pomiędzy rokiem produkcji 2007 a 2008.
Toyota_2007 <- auta2012 %>% 
  filter(Marka=="Toyota" & Rok.produkcji==2007)
Toyota_2008 <- auta2012 %>% 
  filter(Marka=="Toyota" & Rok.produkcji==2008)
inner_join(Toyota_2007, Toyota_2008, by="Model") %>% 
  mutate(różnica=Cena.w.PLN.x-Cena.w.PLN.y) %>% 
  group_by(Model) %>% 
  summarise(średnia=mean(różnica)) %>% 
  arrange(-średnia)
  #Odp: Hiace 
  
## 6. W jakiej marce klimatyzacja jest najczęściej obecna?
auta2012 %>% 
  filter(grepl("klimatyzacja", auta2012$Wyposazenie.dodatkowe, ignore.case = TRUE)) %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  arrange(-n)
  #Odp: Volkswagen

## 7. Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, 
##    która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% 
  filter(KM>100) %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  arrange(-n)
  #Odp: Volkswagen

## 8. Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla,
##    która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Przebieg.w.km<50000 & Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  arrange(-n)
  #Odp: BMW

## 9. Spośród aut marki Toyota wyprodukowanych w 2007 roku, 
##    który model jest średnio najdroższy?
Toyota_2007 %>% 
  group_by(Model) %>% 
  summarise(średnia=mean(Cena.w.PLN)) %>% 
  arrange(-średnia)
  #Odp: Land Cruiser

## 10. Spośród aut marki Toyota, który model ma największą różnicę cen
##     gdy porównać silniki benzynowe a diesel?
Toyota_benzyna <- auta2012 %>% 
  filter(Marka=="Toyota" & Rodzaj.paliwa=="benzyna") %>% 
  group_by(Model) %>% 
  summarise(cena_benzyna=mean(Cena.w.PLN))
Toyota_diesel <- auta2012 %>% 
  filter(Marka=="Toyota" & Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarise(cena_diesel=mean(Cena.w.PLN))
inner_join(Toyota_benzyna, Toyota_diesel, by="Model") %>% 
  mutate(różnica=abs(cena_benzyna-cena_diesel)) %>% 
  arrange(-różnica)
  #Odp; Camry  
