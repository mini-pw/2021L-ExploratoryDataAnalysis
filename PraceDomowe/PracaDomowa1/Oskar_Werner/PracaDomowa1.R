install.packages('PogromcyDanych')
library(PogromcyDanych)
View(auta2012)

## 1. Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
auta2012 %>% 
  filter(Rok.produkcji == 2007, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  count()
## 11621

## 2. Jakiego koloru auta mają najmniejszy medianowy przebieg?
auta2012 %>% 
  group_by(Kolor) %>% 
  summarise(Mediana = median(Przebieg.w.km, na.rm = T)) %>% 
  top_n(-1,Mediana) %>% 
  select(Kolor)
## bialy-metallic 

## 3. Gdy ograniczyć się tylko do aut wyprodukowanych w 2007,
##    która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Rok.produkcji == 2007) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  top_n(1,n) %>% 
  select(Marka)
## Volkswagen 

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?
## *** Co to znaczy najtańsza? ***
auta2012 %>% 
  filter(Rok.produkcji == 2007, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(Srednia = mean(Cena.w.PLN)) %>% 
  top_n(1, Srednia) %>% 
  select(Marka)
  
## 5. Spośród aut marki Toyota,
##    który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.

Toyota2007 <- auta2012 %>% 
  filter(Marka == 'Toyota', Rok.produkcji == 2007) %>% 
  group_by(Model) %>% 
  summarise(srednia_cena_2007 = mean(Cena.w.PLN))
  
Toyota2008 <- auta2012 %>% 
  filter(Marka == 'Toyota', Rok.produkcji == 2008) %>% 
  group_by(Model) %>% 
  summarise(srednia_cena_2008 = mean(Cena.w.PLN))

inner_join(Toyota2008, Toyota2007, by = 'Model') %>% 
  mutate(Roznica = srednia_cena_2008 - srednia_cena_2007) %>% 
  top_n(1, Roznica) %>% 
  select(Model)

# Land Cruiser

## 6. W jakiej marce klimatyzacja jest najczęściej obecna?

## Srednia
auta2012 %>% 
  group_by(Marka) %>% 
  summarise(Srednia = mean(grepl("klimatyzacja", Wyposazenie.dodatkowe))) %>% 
  top_n(1, Srednia) %>% 
  select(Marka)
## Brilliance, DFSK, GMC, Saturn, Scion, Shuanghuan, Vauxhall
  
##Ilosc
auta2012 %>% 
  group_by(Marka) %>% 
  summarise(Ilosc = sum(grepl("klimatyzacja", Wyposazenie.dodatkowe))) %>% 
  top_n(1, Ilosc) %>% 
  select(Marka)
## Wolkswagen

## 7. Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM,
##    która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% 
  filter(KM >100) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  top_n(1, n) %>% 
  select(Marka)
## Wolkswagen

## 8. Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla,
##    która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Przebieg.w.km < 50000, Rodzaj.paliwa == 'olej napedowy (diesel)') %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  top_n(1, n) %>% 
  select(Marka)
## BMW

## 9. Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?
auta2012 %>% 
  filter(Marka == 'Toyota', Rok.produkcji == 2007) %>% 
  group_by(Model) %>% 
  summarise(Srednia = mean(Cena.w.PLN)) %>% 
  top_n(1, Srednia) %>% 
  select(Model)
## Land Cruiser

## 10. Spośród aut marki Toyota,
##     który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?
benzynowe <- auta2012 %>% 
  filter(Marka == 'Toyota', Rodzaj.paliwa == 'benzyna') %>% 
  group_by(Model) %>% 
  summarise(Srednia.benzyna = mean(Cena.w.PLN))


diesel <- auta2012 %>% 
  filter(Marka == 'Toyota', Rodzaj.paliwa == 'olej napedowy (diesel)') %>% 
  group_by(Model) %>% 
  summarise(Srednia.diesel = mean(Cena.w.PLN))

inner_join(benzynowe, diesel) %>% 
  mutate(Roznica = Srednia.benzyna - Srednia.diesel) %>% 
  top_n(1, Roznica) %>% 
  select(Model)
## Camry
























