install.packages("PogromcyDanych")
library(PogromcyDanych)
library(dplyr)
auta2012

View(auta2012)
str(auta2012)

# 1. Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?

auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji == 2007) %>% 
  count()
#Odp: 11621


# 2. Jakiego koloru auta mają najmniejszy medianowy przebieg?

auta2012 %>% 
  group_by(Kolor) %>% 
  summarise(mediana = median(na.omit(Przebieg.w.km))) %>% 
  arrange(mediana) %>% 
  head(1) %>% 
  select(Kolor)
#Odp: bialy-metallic


# 3. Gdy ograniczyć się tylko do aut wyprodukowanych w 2007, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>% 
  filter(Rok.produkcji == 2007) %>% 
  count(Marka, name = "liczba_wystapien") %>% 
  arrange(desc(liczba_wystapien)) %>% 
  head(1) %>% 
  select(Marka)
#Odp: Volkswagen


# 4. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?

auta2012 %>% 
  filter(Rok.produkcji == 2007 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(srednia = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  top_n(1, -srednia) %>% 
  select(Marka)
#Odp: Aixam


# 5.Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.

Modele <- auta2012 %>% 
  filter(Marka == "Toyota") %>% 
  group_by(Model, Rok.produkcji) %>% 
  filter(Rok.produkcji == 2007 | Rok.produkcji == 2008) %>% 
  summarise(srednia = mean(na.omit(Cena.w.PLN)))

#Stworzymy dwie tabele, jedną dla średnich z roku 2007, drugą z roku 2008  
Modele_2007 <- Modele %>% 
  filter(Rok.produkcji == 2007) %>% 
  rename(srednia_2007 = srednia)

Modele_2008 <- Modele %>% 
  filter(Rok.produkcji == 2008) %>% 
  rename(srednia_2008 = srednia)

#Łączymy tabele, liczymy bilans cenowy dla każdego modelu, potem wybieramy model o najmniejszym wyniku 
Strata <- merge(Modele_2007, Modele_2008, by = "Model") %>% 
  mutate(bilans_ceny = srednia_2008 - srednia_2007) %>% 
  arrange(bilans_ceny) %>% 
  head(1) %>% 
  select(Model)

Strata
#Odp: Hiace


# 6. W jakiej marce klimatyzacja jest najczęściej obecna?
library('stringi')
levels(auta2012$Wyposazenie.dodatkowe)
summary(auta2012$Wyposazenie.dodatkowe)

#I wersja - częstotliwość występowania klimatyzacji w samochodach danej marki
unique(auta2012$Marka) #możemy zauważyć, że istnieją dane, w których nazwa marki jest nieokreślona 

samochody_klima <- auta2012 %>% 
  mutate(Klimatyzacja = grepl("klimatyzacja", as.character(auta2012$Wyposazenie.dodatkowe))) %>% 
  filter(Klimatyzacja == TRUE & Marka != "") %>%    #pomijamy samochody bez nazwy marki
  count(Marka, name = "liczba_samochodow_z_klimatyzacja")

samochody_ogolnie <- auta2012 %>% 
  filter(Marka != "") %>% 
  count(Marka, name = "liczba_samochodow")

samochody_ogolnie %>% 
  merge(samochody_klima, by = "Marka") %>% 
  mutate(czestotliwosc = liczba_samochodow_z_klimatyzacja / liczba_samochodow) %>% 
  arrange(-czestotliwosc) %>% 
  head(7) %>% 
  select(Marka)
#Odp: Brilliance, DFSK, GMC, Saturn, Scion, Shuanghuan, Vauxhall  (7 samochodów o równej częstotliwości)

#II wersja - bezwzględna liczba samochodów z klimatyzacją w zależności od Marki
auta2012 %>% 
  mutate(Klimatyzacja = grepl("klimatyzacja", as.character(auta2012$Wyposazenie.dodatkowe))) %>% 
  filter(Klimatyzacja == TRUE & Marka != "") %>%    #pomijamy samochody bez nazwy marki
  count(Marka, name = "liczba_samochodow_z_klimatyzacja") %>% 
  top_n(1, liczba_samochodow_z_klimatyzacja) %>% 
  select(Marka)
#Odp: Volkswagen


# 7. Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>% 
  filter(KM > 100) %>% 
  count(Marka, name = "liczba_samochodow") %>% 
  top_n(1, liczba_samochodow) %>% 
  select(Marka)
#Odp: Volkswagen


# 8. Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>%
  filter(Przebieg.w.km < 50000 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  count(Marka, name = "liczba_aut") %>% 
  arrange(-liczba_aut) %>% 
  head(1) %>% 
  select(Marka)
#Odp: BMW


# 9. Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?

suma_cen <- auta2012 %>% 
  filter(Marka == "Toyota" & Rok.produkcji == 2007) %>%
  group_by(Model) %>% 
  summarise(suma = sum(Cena.w.PLN)) %>% 
  select(Model, suma)

liczba_modeli <- auta2012 %>% 
  filter(Marka == "Toyota" & Rok.produkcji == 2007) %>%
  count(Model, name = "liczba")

suma_cen %>% 
  merge(liczba_modeli, by = "Model") %>% 
  mutate(srednia = suma / liczba) %>% 
  top_n(1, srednia) %>% 
  select(Model)
#Odp: Land Cruiser


# 10. Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?

suma_cen_diesel <- auta2012 %>% 
  filter(Marka == "Toyota" & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Model) %>% 
  summarise(suma = sum(Cena.w.PLN)) %>% 
  select(Model, suma)

liczba_modeli_diesel <- auta2012 %>% 
  filter(Marka == "Toyota" & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  count(Model, name = "liczba")

srednia_diesel <- suma_cen_diesel %>% 
  merge(liczba_modeli_diesel, by = "Model") %>% 
  mutate(srednia_diesel = suma / liczba) %>% 
  select(Model, srednia_diesel)


suma_cen_benzyna <- auta2012 %>% 
  filter(Marka == "Toyota" & Rodzaj.paliwa == "benzyna") %>%
  group_by(Model) %>% 
  summarise(suma = sum(Cena.w.PLN)) %>% 
  select(Model, suma)

liczba_modeli_benzyna <- auta2012 %>% 
  filter(Marka == "Toyota" & Rodzaj.paliwa == "benzyna") %>%
  count(Model, name = "liczba")

srednia_benzyna <- suma_cen_benzyna %>% 
  merge(liczba_modeli_benzyna, by = "Model") %>% 
  mutate(srednia_benzyna = suma / liczba) %>% 
  select(Model, srednia_benzyna)

#teraz połączymy tabele ze średnimi cenami w dieslu i dla benzyny,
#stworzymy nową kolumnę pokazującą różnice w cenach i wybierzemy największą z nich

srednia_benzyna %>% 
  merge(srednia_diesel, by = "Model") %>% 
  mutate(roznica_cen = abs(srednia_benzyna - srednia_diesel)) %>% 
  arrange(desc(roznica_cen)) %>% 
  head(1) %>% 
  select(Model)
#Odp: Camry