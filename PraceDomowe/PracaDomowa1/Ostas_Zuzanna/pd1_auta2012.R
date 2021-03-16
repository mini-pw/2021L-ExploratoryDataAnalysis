install.packages("PogromcyDanych")
library(PogromcyDanych)
library(dplyr)
library(tidyr)

## 1. Ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?

length(filter(auta2012, Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007))
  
## Odp: 21

## 2. Jakiego koloru auta maja najmniejszy medianowy przebieg?

auta2012 %>% 
  filter(Kolor != "") %>% 
  group_by(Kolor) %>% 
  summarise(mediana = median(Przebieg.w.km, na.rm = TRUE)) %>% 
  head(1)

## Odp: Auta koloru beżowego.

## 3. Gdy ograniczyć się tylko do aut wyprodukowanych w 2007, która marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>% 
  filter(Rok.produkcji == 2007) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(1)

## Odp: Volkswagen

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?

auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007) %>%
  group_by(Marka) %>% 
  summarize(średnia_cena = mean(Cena.w.PLN)) %>% 
  arrange(średnia_cena) %>% 
  head(1)

## Aixam

## 5. Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008?

auta2012 %>% 
  filter(Marka == 'Toyota', (Rok.produkcji == 2007 | Rok.produkcji == 2008)) %>% 
  group_by(Model, Rok.produkcji) %>% 
  summarise(średnia_cena = mean(Cena.w.PLN)) %>% 
  pivot_wider(names_from = Rok.produkcji, values_from = średnia_cena, names_prefix = 'rok') %>%  
  na.omit() %>% 
  mutate(różnica = rok2008 - rok2007) %>% 
  arrange(różnica) %>% 
  head(1)

## Odp: Toyota Hiace

## 6. W jakiej marce klimatyzacja jest najczęściej obecna?

auta2012 %>% 
  select(Marka, Wyposazenie.dodatkowe) %>% 
  filter(Marka != "") %>% 
  mutate(klimatyzacja = grepl("klimatyzacja", Wyposazenie.dodatkowe, fixed = 1)) %>% 
  group_by(Marka, klimatyzacja) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = klimatyzacja, values_from = count, names_prefix = "n_") %>% 
  na.omit() %>% 
  mutate(procent = ((100*n_TRUE)/(n_TRUE + n_FALSE))) %>% 
  arrange(desc(procent)) %>% 
  head(1)

## Odp: Saab

## 7. Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>% 
  filter(KM > 100, Marka != "") %>% 
  group_by(Marka) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(1)

## Odp: Volkswagen

## 8. Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>% 
  filter(Przebieg.w.km < 50000, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(1)

## Odp: BMW

## 9. Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?

auta2012 %>% 
  filter(Marka == 'Toyota', Rok.produkcji == 2007) %>% 
  group_by(Model) %>% 
  summarise(średnia_cena = mean(Cena.w.PLN)) %>%
  arrange(desc(średnia_cena)) %>% 
  head(1)

## Odp: Land Cruiser

## 10. Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?

auta2012 %>% 
  filter(Marka == 'Toyota', (Rodzaj.paliwa == 'benzyna' | Rodzaj.paliwa == 'olej napedowy (diesel)')) %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarize(średnia_cena = mean(Cena.w.PLN)) %>% 
  pivot_wider(names_from = Rodzaj.paliwa, values_from = średnia_cena) %>% 
  na.omit() %>% 
  rename('diesel' = 'olej napedowy (diesel)' ) %>% 
  mutate(różnica = abs(diesel - benzyna)) %>% 
  arrange(desc(różnica)) %>% 
  head(1)

## Odp: Toyota Camry