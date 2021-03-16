install.packages("PogromcyDanych")
library(PogromcyDanych)
View(auta2012)

## 1. Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?

auta2012 %>% 
  filter(Rok.produkcji == "2007", Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  summarise(n = n())

## Odp: Jest ich 11621

## 2. Jakiego koloru auta mają najmniejszy medianowy przebieg?

auta2012 %>%
  group_by(Kolor) %>% 
  summarise(Przebieg.med = median(Przebieg.w.km,na.rm = TRUE)) %>% 
  top_n(1, -Przebieg.med)

## Odp: Najmniejszy medianowy przebieg maja auta w kolorze bialy-metallic.

## 3. Gdy ograniczyć się tylko do aut wyprodukowanych w 2007, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>% 
  filter(Rok.produkcji == "2007") %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  top_n(1)

## Odp: Najczesciej wystepuje w tym przypadku marka Volkswagen

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?

auta2012 %>% 
  filter(Rok.produkcji == "2007", Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(cena.srednia = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  top_n(1,-cena.srednia)

## Odp: Przyjmujac za wyznacznik "taniosci" srednia arytmetyczna z cen samochodow danej marki, najtanszy okazal sie Aixam.

## 5. Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.

a <- auta2012 %>% 
  filter(Marka == "Toyota", Rok.produkcji =="2007") %>%
  group_by(Model) %>% 
  summarise(cena.srednia.2007 = mean(Cena.w.PLN))

b <- auta2012 %>% 
  filter(Marka == "Toyota", Rok.produkcji =="2008") %>%
  group_by(Model) %>% 
  summarise(cena.srednia.2008 = mean(Cena.w.PLN))

left_join(a,b) %>% 
  mutate(roznica=cena.srednia.2008 - cena.srednia.2007) %>% 
  top_n(1,-roznica)

## Odp: Najbardziej na cenie stracił model Hiace.

## 6. W jakiej marce klimatyzacja jest najczęściej obecna?

x <- grep("klimatyzacja", auta2012$Wyposazenie.dodatkowe)

auta2012[x,] %>%
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  top_n(1)

## Odp: Klimatyzacja jest najczęściej obecna w samochodach marki Volkswagen.

## 7. Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>% 
  filter(KM > 100) %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>%
  top_n(1)

## Odp: Najczesciej wystepuja marka Volkswagen.

## 8. Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>% 
  filter(Przebieg.w.km < 50000, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  top_n(1)

# Odp: Najczesciej wystepuje marka BMW

## 9. Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?

auta2012 %>% 
  filter(Marka == "Toyota",Rok.produkcji=="2007") %>% 
  group_by(Model) %>% 
  summarise(mean_price=mean(Cena.w.PLN,na.rm=TRUE)) %>% 
  top_n(1)

## Odp: Srednio najdrozszy byl w tym przypadku model Land Cruiser.

## 10. Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?

benz <- auta2012 %>% 
  filter(Marka == "Toyota", Rodzaj.paliwa =="benzyna") %>% 
  group_by(Model) %>% 
  summarise(srednia_b = mean(Cena.w.PLN))

diesel <- auta2012 %>% 
  filter(Marka == "Toyota", Rodzaj.paliwa =="olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarise(srednia_d = mean(Cena.w.PLN))
  
left_join(benz,diesel) %>% 
  mutate(roznica_bezw = abs(srednia_d-srednia_b)) %>% 
  top_n(1,roznica_bezw)

## Odp: Najwieksza roznice cen zanotował model Camry.
