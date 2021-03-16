library(tidyr)
library(dplyr)
library(PogromcyDanych)

## 1.Sprawdź ile jest aut z silnikiem diesla 
## wyprodukowanych w 2007 roku?

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  filter(Rok.produkcji == "2007") %>%
  nrow()

##Odp: 11621

## 2.Jakiego koloru auta mają 
## najmniejszy medianowy przebieg?

auta2012 %>% 
  group_by(Kolor) %>% 
  summarise(mediana=median(Przebieg.w.km, na.rm=TRUE)) %>% 
  arrange(mediana) %>% 
  head(1)

## Odp: bialy-metallic

## 3. Gdy ograniczyć się tylko do 
## aut wyprodukowanych w 2007, która Marka występuje 
## najczęściej w zbiorze danych auta2012?  

auta2012 %>% 
  filter(Rok.produkcji=="2007") %>% 
  count(Marka) %>% 
  arrange(-n) %>% 
  head(1)

## Odp: Volkswagen

## 4.Spośród aut z silnikiem diesla wyprodukowanych 
## w 2007 roku która marka jest najtańsza?
auta2012 %>% 
  filter(Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  filter(Rok.produkcji=="2007") %>% 
  group_by(Marka) %>% 
  summarise(średnia_cena=mean(Cena.w.PLN, na.rm=TRUE)) %>% 
  arrange(średnia_cena) %>% 
  head(1)

## Odp: Aixam

## 5. Spośród aut marki Toyota, który model 
## najbardziej stracił na cenie pomiędzy rokiem 
## produkcji 2007 a 2008.

cena2007 <- auta2012 %>% 
  filter(Marka=="Toyota") %>% 
  filter(Rok.produkcji=="2007") %>% 
  group_by(Model) %>%
  summarise(średnia_cena_2007=mean(Cena.w.PLN, na.rm=TRUE))
  
cena2008 <- auta2012 %>% 
  filter(Marka=="Toyota") %>% 
  filter(Rok.produkcji=="2008") %>% 
  group_by(Model) %>%
  summarise(średnia_cena_2008=mean(Cena.w.PLN, na.rm=TRUE))  

cena2007 %>% 
  left_join(cena2008, by="Model") %>% 
  mutate(różnica=średnia_cena_2007-średnia_cena_2008) %>% 
  arrange(-różnica) %>% 
  head(1) %>% 
  select("Model")

## Odp: Hiace

## 6. W jakiej marce klimatyzacja jest najczęściej 
## obecna?

# Nie wiedziałem, jak zinterpretować "najczęściej obecna",
# więc znalazłem markę, której samochodów z klimatyzacją jest
# najwięcej, oraz markę, dla której udział procentowy
# samochodów z klimatyzacją jest największy.

auta2012 %>% 
  filter(grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>% 
  count(Marka) %>% 
  top_n(1,n)

auta2012 %>% 
  filter(grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>% 
  count(Marka, name = "z_klimatyzacją") %>%
  left_join(count(auta2012, Marka, name = "bez_klimatyzacji"), by="Marka") %>% 
  mutate(odsetek=100*z_klimatyzacją/bez_klimatyzacji) %>% 
  top_n(7, odsetek)

## Odp:
## Marka, której samochodów z klimatyzacją jest najwięcej: Volkswagen
## Marki, których odsetek samochodów z klimatyzacją jest największy:
## Brilliance, DFSK, GMC, Saturn, Scion, Shuanghuan, Vauxhall  
  
## 7. Gdy ograniczyć się tylko do aut z silnikiem 
## ponad 100 KM, która Marka występuje najczęściej 
## w zbiorze danych auta2012?

auta2012 %>% 
  filter(KM>100) %>% 
  select("Marka") %>% 
  table() %>% 
  as.data.frame() %>%
  top_n(1,Freq)

## Odp: Volkswagen

## 8. Gdy ograniczyć się tylko do aut o przebiegu 
## poniżej 50 000 km o silniku diesla, która Marka 
## występuje najczęściej w zbiorze danych auta2012?

auta2012 %>% 
  filter(Przebieg.w.km<50000) %>% 
  filter(Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  count(Marka) %>% 
  top_n(1, n)  

## Odp: BMW

## 9. Spośród aut marki Toyota wyprodukowanych w 
## 2007 roku, który model jest średnio najdroższy?

auta2012 %>% 
  filter(Marka == "Toyota") %>% 
  filter(Rok.produkcji == "2007") %>% 
  group_by(Model) %>% 
  summarise(średnia_cena=mean(Cena.w.PLN, na.rm=TRUE)) %>% 
  top_n(1, średnia_cena)

## Odp: Land Cruiser

## 10. Spośród aut marki Toyota, który model ma 
## największą różnicę cen gdy porównać silniki 
## benzynowe a diesel?

cena_diesel <- auta2012 %>% 
  filter(Marka == "Toyota") %>%
  filter(Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarise(średnia_cena_diesel = mean(Cena.w.PLN, na.rm=TRUE))
  
cena_benzyna <- auta2012 %>% 
  filter(Marka == "Toyota") %>%
  filter(Rodzaj.paliwa=="benzyna") %>% 
  group_by(Model) %>% 
  summarise(średnia_cena_benzyna = mean(Cena.w.PLN, na.rm=TRUE))

cena_diesel %>% 
  left_join(cena_benzyna, by="Model") %>% 
  mutate(różnica = abs(średnia_cena_diesel-średnia_cena_benzyna)) %>% 
  top_n(1, różnica)

## Odp: Camry  
  
