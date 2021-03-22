## PRACA DOMOWA 1

install.packages("PogromcyDanych")
library(PogromcyDanych)
auta2012

## 1. Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?

auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  filter(Rok.produkcji == "2007") %>%
  summarise(n = n())

## Odp: 11621

## 2. Jakiego koloru auta mają najmniejszy medianowy przebieg?

auta2012 %>%
  group_by(Kolor) %>%
  summarise(mediana = median(Przebieg.w.km, na.rm = TRUE)) %>%
  top_n(1, -mediana)

## Odp: bialy-metallic

## 3. Gdy ograniczyć się tylko do aut wyprodukowanych w 2007, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>%
  filter(Rok.produkcji == "2007") %>%
  group_by(Marka) %>%
  summarise(n = n()) %>%
  top_n(1, n)

## Odp: Volkswagen

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?

auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  filter(Rok.produkcji == "2007") %>%
  group_by(Marka) %>%
  summarise(srednia.cena = mean(Cena.w.PLN, na.rm = TRUE)) %>%
  top_n(1, -srednia.cena)

## Odp: Aixam

## 5. Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.

tab1 <- auta2012 %>%
  filter(Marka == "Toyota") %>%
  filter(Rok.produkcji == "2007") %>%
  group_by(Model) %>%
  summarise(sr.cena.07 = mean(Cena.w.PLN, na.rm = TRUE))

tab2 <- auta2012 %>%
  filter(Marka == "Toyota") %>%
  filter(Rok.produkcji == "2008") %>%
  group_by(Model) %>%
  summarise(sr.cena.08 = mean(Cena.w.PLN, na.rm = TRUE))

left_join(tab1, tab2, by = "Model") %>%
  mutate(roznica= sr.cena.07 - sr.cena.08) %>%
  top_n(1,roznica)

## Odp: Hiace

## 6. W jakiej marce klimatyzacja jest najczęściej obecna?

auta2012 %>%
  filter(grepl('klimatyzacja', Wyposazenie.dodatkowe)) %>%
  group_by(Marka) %>%
  summarise(n = n()) %>%
  top_n(1,n)


## Odp: Volkswagen

## 7. Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>%
  filter(KM > 100) %>%
  group_by(Marka)%>%
  summarise(n = n()) %>%
  top_n(1,n)

## Odp. Volkswagen

## 8. Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>%
  filter(Przebieg.w.km < 50000) %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka)%>%
  summarise(n = n()) %>%
  top_n(1,n)

## Odp: BMW

## 9. Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?

auta2012 %>%
  filter(Marka == "Toyota") %>%
  filter(Rok.produkcji == "2007")%>%
  group_by(Model) %>%
  summarise(sr.cena = mean(Cena.w.PLN,na.rm = TRUE)) %>%
  top_n(1,sr.cena)

## Odp: Land Cruiser

## 10. Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?

tab3 <- auta2012 %>%
  filter(Marka == "Toyota") %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Model) %>%
  summarise(sr.cena.diesel = mean(Cena.w.PLN, na.rm = TRUE))

tab4 <- auta2012 %>%
  filter(Marka == "Toyota") %>%
  filter(Rodzaj.paliwa == "benzyna") %>%
  group_by(Model) %>%
  summarise(sr.cena.benzynowy = mean(Cena.w.PLN, na.rm = TRUE))

left_join(tab3, tab4, by = "Model") %>%
  mutate(roznica= abs(sr.cena.benzynowy-sr.cena.diesel)) %>%
  top_n(1,roznica)

## Odp: Camry