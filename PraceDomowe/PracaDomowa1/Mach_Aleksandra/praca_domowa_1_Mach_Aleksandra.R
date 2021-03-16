install.packages("PogromcyDanych")
library(PogromcyDanych)
View(auta2012)

## 1. SprawdŸ ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji == 2007) %>% 
  nrow()
## Odp: 11621


## 2. Jakiego koloru auta maj¹ najmniejszy medianowy przebieg?
auta2012 %>% 
  group_by(Kolor) %>% 
  summarise(med_przeb = median(Przebieg.w.km, na.rm = TRUE)) %>% 
  arrange(med_przeb)
## Odp: bialy-metallic
    

## 3. Gdy ograniczyæ siê tylko do aut wyprodukowanych w 2007, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Rok.produkcji ==2007) %>% 
  group_by(Marka) %>% 
  count() %>% 
  arrange(-n)
## Odp: Volkswgagen  

  
## 4. Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtañsza?
auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji == 2007) %>% 
  group_by(Marka) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>% 
  arrange(srednia_cena)
## Odp: Aixam


## 5. Spoœród aut marki Toyota, który model najbardziej straci³ na cenie pomiêdzy rokiem produkcji 2007 a 2008.
auta2012 %>% 
  filter(Marka == "Toyota" & Rok.produkcji == 2007) %>% 
  group_by(Model) %>% 
  summarise(sr_2007 = mean(Cena.w.PLN)) %>% 
  select(sr_2007, Model) -> srednia_cena_2007

auta2012 %>% 
  filter(Marka == "Toyota" & Rok.produkcji == 2008) %>% 
  group_by(Model) %>% 
  summarise(sr_2008 = mean(Cena.w.PLN)) %>% 
  select(sr_2008, Model) -> srednia_cena_2008

srednia_cena_2007 %>% 
  inner_join(srednia_cena_2008, by = "Model") %>% 
  mutate(roznica = (sr_2007-sr_2008)) %>% 
  arrange(-roznica)

## Odp: Hiace


## 6. W jakiej marce klimatyzacja jest najczêœciej obecna?
auta2012 %>%
  filter("klimatyzacja" %in% Wyposazenie.dodatkowe) %>% 
  group_by(Marka) %>% 
  count() %>% 
  arrange(-n)
## Odp: Volkswagen


## 7. Gdy ograniczyæ siê tylko do aut z silnikiem ponad 100 KM, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>% 
  filter(KM > 100) %>% 
  group_by(Marka) %>% 
  count() %>% 
  arrange(-n) 
## Odp: Volkswagen


## 8. Gdy ograniczyæ siê tylko do aut o przebiegu poni¿ej 50 000 km o silniku diesla, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Przebieg.w.km < 50000 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  count() %>% 
  arrange(-n)
## Odp: BMW


## 9. Spoœród aut marki Toyota wyprodukowanych w 2007 roku, który model jest œrednio najdro¿szy?
auta2012 %>% 
  filter(Marka == "Toyota" & Rok.produkcji == 2007) %>%
  group_by(Model) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  arrange(-srednia_cena)
## Odp: Land Cruiser


## 10. Spoœród aut marki Toyota, który model ma najwiêksz¹ ró¿nicê cen gdy porównaæ silniki benzynowe a diesel?
auta2012 %>% 
  filter(Marka == "Toyota" & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarise(sr_diesel = mean(Cena.w.PLN, na.rm = TRUE)) -> diesel

auta2012 %>% 
  filter(Marka == "Toyota" & Rodzaj.paliwa == "benzyna") %>% 
  group_by(Model) %>% 
  summarise(sr_benzyna = mean(Cena.w.PLN, na.rm = TRUE)) -> benzyna

diesel %>% 
  inner_join(benzyna, by = "Model") %>% 
  mutate(roznica = abs(sr_diesel - sr_benzyna)) %>% 
  arrange(-roznica)

## Odp: Camry


