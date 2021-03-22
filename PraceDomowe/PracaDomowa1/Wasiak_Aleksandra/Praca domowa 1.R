install.packages("PogromcyDanych")
library(PogromcyDanych)
auta2012

library(dplyr)

## 1. SprawdŸ ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?

auta2012 %>% 
  filter(Rok.produkcji == 2007, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  summarise(n = n())

## Odp.: 11621


## 2. Jakiego koloru auta maj¹ najmniejszy medianowy przebieg?

auta2012 %>% 
  group_by(Kolor) %>% 
  summarise(mediana = median(Przebieg.w.km, na.rm = TRUE)) %>% 
  top_n(-1,mediana) %>% 
  select(Kolor)

## Odp.: bialy-metallic


## 3. Gdy ograniczyæ siê tylko do aut wyprodukowanych w 2007,
## która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

auta2012 %>% 
  filter(Rok.produkcji == 2007) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  top_n(1,n) %>% 
  select(Marka)

## Odp.: Volkswagen


## 4. Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku
## która marka jest najtañsza?

auta2012 %>% 
  filter(Rok.produkcji == 2007, Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>%
  top_n(-1,srednia_cena) %>% 
  select(Marka)

## Odp.: Aixam
  
  
## 5. Spoœród aut marki Toyota, który model najbardziej straci³ na cenie
## pomiêdzy rokiem produkcji 2007 a 2008.
  
auta2012 %>% 
  filter(Marka == "Toyota", Rok.produkcji == 2007 | Rok.produkcji == 2008) %>% 
  group_by(Model) %>% 
  summarise(sr_cena_2007 = mean(Cena.w.PLN[Rok.produkcji==2007]),
            sr_cena_2008 = mean(Cena.w.PLN[Rok.produkcji==2008])) %>% 
  mutate(spadek_ceny = sr_cena_2007-sr_cena_2008) %>% 
  top_n(1,spadek_ceny) %>% 
  select(Model)

## Odp.: Hiace


## 6. W jakiej marce klimatyzacja jest najczêœciej obecna?

auta2012 %>% 
  mutate(wyposazenie = strsplit(as.character(Wyposazenie.dodatkowe), ",")) %>% 
  mutate(klimatyzacja = is.element("klimatyzacja", wyposazenie)) %>%


## Odp.:


## 7. Gdy ograniczyæ siê tylko do aut z silnikiem ponad 100 KM,
## która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

auta2012 %>% 
  filter(KM > 100) %>%
  group_by(Marka) %>%
  summarise(n = n()) %>%
  top_n(1,n) %>% 
  select(Marka)

## Odp.: Volkswagen


## 8. Gdy ograniczyæ siê tylko do aut o przebiegu poni¿ej 50 000 km o silniku
## diesla, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

auta2012 %>%
  filter(Przebieg.w.km < 50000, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  top_n(1,n) %>% 
  select(Marka)

## Odp.: BMW


## 9. Spoœród aut marki Toyota wyprodukowanych w 2007 roku,
## który model jest œrednio najdro¿szy?

auta2012 %>% 
  filter(Rok.produkcji == 2007, Marka == "Toyota") %>% 
  group_by(Model) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>% 
  top_n(1,srednia_cena) %>% 
  select(Model)

## Odp.: Land Cruiser


## 10. Spoœród aut marki Toyota, który model ma najwiêksz¹ ró¿nicê cen
## gdy porównaæ silniki benzynowe a diesel?

auta2012 %>% 
  filter(Marka == "Toyota") %>% 
  group_by(Model) %>% 
  summarise(sr_cena_diesel = mean(Cena.w.PLN[Rodzaj.paliwa == "olej napedowy (diesel)"]),
            sr_cena_benzyna = mean(Cena.w.PLN[Rodzaj.paliwa == "benzyna"])) %>% 
  mutate(roznica_cen = abs(sr_cena_diesel - sr_cena_benzyna)) %>% 
  top_n(1, roznica_cen) %>% 
  select(Model)
  
## Odp.: Camry

