install.packages("PogromcyDanych")
library(PogromcyDanych)

# 1. Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
auta2012 %>%
  filter(Rok.produkcji == 2007 & Rodzaj.paliwa == 'olej napedowy (diesel)') %>%
  count()
## Odp: 11621

# 2. Jakiego koloru auta mają najmniejszy medianowy przebieg?
auta2012 %>%
  group_by(Kolor) %>%
  summarise(medianowy_przebieg = median(Przebieg.w.km, na.rm = T)) %>%
  arrange(medianowy_przebieg) %>%
  head(1) %>%
  select(Kolor)
## Odp: bialy-metallic

# 3. Gdy ograniczyć się tylko do aut wyprodukowanych w 2007, która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>%
  filter(Rok.produkcji == 2007) %>%
  group_by(Marka) %>%
  summarise(liczba_samochodow = n()) %>%
  arrange(desc(liczba_samochodow)) %>%
  head(1) %>%
  select(Marka)
## Odp: Volkswagen

# 4. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?
auta2012 %>%
  filter(Rok.produkcji == 2007 & Rodzaj.paliwa == 'olej napedowy (diesel)') %>%
  group_by(Marka) %>%
  summarise(srednia_cena = mean(Cena.w.PLN, na.rm = T)) %>%
  arrange(srednia_cena) %>%
  head(1) %>%
  select(Marka)
## Odp: Aixam

# 5. Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.
auta2012 %>%
  filter(Marka == 'Toyota' & Rok.produkcji == 2007) %>%
  group_by(Model) %>%
  summarise(srednia_cena_2007 = mean(Cena.w.PLN, na.rm = T)) -> Rok_2007

auta2012 %>%
  filter(Marka == 'Toyota' & Rok.produkcji == 2008) %>%
  group_by(Model) %>%
  summarise(srednia_cena_2008 = mean(Cena.w.PLN, na.rm = T)) %>%
  left_join(Rok_2007) %>%
  transmute(Model, spadek = srednia_cena_2007 - srednia_cena_2008) %>%
  arrange(desc(spadek)) %>%
  head(1) %>%
  select(Model)
## Odp: Hiace

# 6. W jakiej marce klimatyzacja jest najczęściej obecna?
auta2012 %>% 
  group_by(Marka) %>% 
  summarise(klimatyzowane_pojazdy = sum(grepl("klimatyzacja", Wyposazenie.dodatkowe))) -> z_klimatyzacja

auta2012 %>%
  group_by(Marka) %>%
  summarise(liczba_pojazdow = n()) %>%
  left_join(z_klimatyzacja) %>%
  transmute(Marka, odsetek_klimatyzacji = klimatyzowane_pojazdy/liczba_pojazdow) %>%
  arrange(desc(odsetek_klimatyzacji))
## Odp: Brilliance, DFSK, GMC, Saturn, Scion, Shuanghuan, Vauxhall

# 7. Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>%
  filter(KM > 100) %>%
  group_by(Marka) %>%
  summarise(liczba_wystapien = n()) %>%
  arrange(desc(liczba_wystapien)) %>%
  head(1) %>%
  select(Marka)
## Odp: Volkswagen

# 8. Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla,
#    która Marka występuje najczęściej w zbiorze danych auta2012?
auta2012 %>%
  filter(Przebieg.w.km < 50000 & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>%
  summarise(liczba_wystapien = n()) %>%
  arrange(desc(liczba_wystapien)) %>%
  head(1) %>%
  select(Marka)
## Odp: BMW

# 9. Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?
auta2012 %>%
  filter(Marka == "Toyota" & Rok.produkcji == 2007) %>%
  group_by(Model) %>%
  summarise(Cena_modelu = mean(Cena.w.PLN)) %>%
  arrange(desc(Cena_modelu)) %>%
  head(1) %>%
  select(Model)
## Odp: Land Cruiser

# 10. Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?
auta2012 %>%
  filter(Rodzaj.paliwa == "benzyna" | Rodzaj.paliwa == "benzyna+LPG") %>%
  filter(Marka == "Toyota") %>%
  group_by(Model) %>%
  summarise(Cena_benz = mean(Cena.w.PLN)) -> benzyna

auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Marka == "Toyota") %>%
  group_by(Model) %>%
  summarise(Cena_diesel = mean(Cena.w.PLN)) %>%
  left_join(benzyna) %>%
  transmute(Model,roznica = abs(Cena_diesel - Cena_benz)) %>%
  arrange(desc(roznica)) %>%
  head(1) %>%
  select(Model)
## Odp:Camry














