library(tidyr)
library(dplyr)

install.packages("PogromcyDanych")
library(PogromcyDanych)
auta <- auta2012

## 1. Sprawdz ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
auta %>% filter(Rok.produkcji == 2007, Rodzaj.paliwa == 'olej napedowy (diesel)') %>% count()
## Odp: 11621

## 2. Jakiego koloru auta maja najmniejszy medianowy przebieg?
auta %>% group_by(Kolor) %>% summarise(median = median(Przebieg.w.km, na.rm = TRUE)) %>% top_n(1, -median)
## Odp: bialy-metallic

## 3. Gdy ograniczyc sie tylko do aut wyprodukowanych w 2007, która Marka występuje najczęściej w zbiorze danych auta2012?
auta %>% filter(Rok.produkcji == 2007) %>% count(Marka) %>% top_n(1)
## Odp: Volkswagen

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?
auta %>% filter(Rok.produkcji == 2007, Rodzaj.paliwa == 'olej napedowy (diesel)') %>% 
  group_by(Marka) %>% summarise(mean=mean(Cena.w.PLN)) %>% top_n(1, -mean)
## Odp: Aixam

## 5. Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.
Toyota2007 <- auta %>% filter(Rok.produkcji == 2007, Marka == 'Toyota') %>% 
  group_by(Model) %>% summarise(mean2007=mean(Cena.w.PLN))
Toyota2008 <- auta %>% filter(Rok.produkcji == 2008, Marka == 'Toyota') %>% 
  group_by(Model) %>% summarise(mean2008=mean(Cena.w.PLN))
Toyota2007 %>% left_join(Toyota2008, by='Model') %>% mutate(strata=mean2007-mean2008) %>% 
  top_n(1, strata)
## Odp: Hiace

## 6. W jakiej marce klimatyzacja jest najczęściej obecna?
Klimatyzacja <- auta %>% filter(grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>%
  group_by(Marka) %>% summarise(n_klim = n())
auta %>% group_by(Marka) %>% summarise(n = n()) %>% left_join(Klimatyzacja, by='Marka') %>% 
  mutate(odsetek_klim=n_klim/n) %>% top_n(1, odsetek_klim)
## Odp: Brilliance, DFSK, GMC, Saturn, Scion, Shuanghua, Vauxhall

## 7.Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, która Marka występuje najczęściej w zbiorze danych auta2012?
auta %>% filter(KM > 100) %>% group_by(Marka) %>% summarise(n=n()) %>% top_n(1)
## Odp: Volkswagen

## 8.Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, która Marka występuje najczęściej w zbiorze danych auta2012?
auta %>% filter(Przebieg.w.km < 50000, Rodzaj.paliwa == 'olej napedowy (diesel)') %>% 
  group_by(Marka) %>% summarise(n=n()) %>% top_n(1)
## Odp: BMW

## 9.Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?
auta %>% filter(Rok.produkcji == 2007, Marka == 'Toyota') %>% 
  group_by(Model) %>% summarise(mean2008=mean(Cena.w.PLN)) %>% top_n(1)
## Odp: Land Cruiser

## 10. Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?
Toyota_Benzyna <- auta %>% filter(Rodzaj.paliwa == 'benzyna', Marka == 'Toyota') %>% 
  group_by(Model) %>% summarise(meanB=mean(Cena.w.PLN))
Toyota_Diesel <- auta %>% filter(Rodzaj.paliwa == 'olej napedowy (diesel)', Marka == 'Toyota') %>% 
  group_by(Model) %>% summarise(meanD=mean(Cena.w.PLN))
Toyota_Benzyna %>% inner_join(Toyota_Diesel, by='Model') %>% mutate(diff=abs(meanB-meanD)) %>% 
  top_n(1, diff)
## Odp: Camry