######################################################
########### PRACA DOMOWA NR 1 ########################
######################################################

install.packages("PogromcyDanych")
library(PogromcyDanych)

auta2012

## 1. Ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji == 2007) %>%
  summarise(n = n())
## odp.: 11621

## 2. Jakiego koloru auta maj¹ najmniejszy medianowy przebieg?
auta2012 %>%
  group_by(Kolor) %>%
  summarise(mediana = median(Przebieg.w.km, na.rm = TRUE)) %>%
  arrange(mediana) %>%
  head(1)
## odp.: bialy-metallic

## 3. Gdy ograniczyæ siê tylko do aut wyprodukowanych w 2007, która Marka wystêpuje najczêœciej w zbiorze
##    danych auta2012?
auta2012 %>%
  filter(Rok.produkcji == 2007) %>%
  group_by(Marka) %>%
  summarise(ilosc = n()) %>%
  arrange(-ilosc) %>%
  head(1)
## odp.: Volkswagen

## 4. Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtañsza?
auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji == 2007) %>%
  group_by(Marka) %>%
  summarise(srednia_cena = mean(Cena, na.rm = TRUE)) %>%
  arrange(srednia_cena) %>%
  head(1)
## odp. Aixam

## 5. Spoœród aut marki Toyota, który model najbardziej straci³‚ na cenie pomiêdzy rokiem produkcji
##    2007 a 2008.
cena_po_produkcji <- auta2012 %>%
  filter(Marka == "Toyota" & Rok.produkcji == c(2007, 2008)) %>%
  group_by(Model, Rok.produkcji) %>%
  summarise(cena_nowa = ifelse(Rok.produkcji == 2007, mean(Cena),
                               ifelse(Rok.produkcji == 2008, mean(Cena), 0))) %>%
  unique()

cena_po_produkcji2 <- cena_po_produkcji %>% filter(Rok.produkcji == 2008) %>% rename(cena2008 = cena_nowa)

cena_po_produkcji %>%
  filter(Rok.produkcji == 2007) %>%
  rename(cena2007 = cena_nowa) %>%
  left_join(cena_po_produkcji2, by = "Model") %>%
  mutate(cena2007=replace(cena2007, is.na(cena_po_produkcji$cena2007), 0L)) %>%
  mutate(cena2008=replace(cena2008, is.na(cena_po_produkcji$cena2008), 0L)) %>%
  mutate(roznica = cena2008 - cena2007) %>%
  arrange(roznica) %>%
  head(1)
## odp.: Prius

## 6. W jakiej marce klimatyzacja jest najczêœciej obecna?
auta2012 %>%
  select(Cena:Wyposazenie.dodatkowe, contains("klimatyzacja")) %>%
  group_by(Marka) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  head(1)
## odp.: Volkswagen
## 7. Gdy ograniczyæ siê tylko do aut z silnikiem ponad 100 KM, która Marka wystêpuje najczÄ™Å›ciej
##    w zbiorze danych auta2012?
auta2012 %>%
  filter(KM > 100) %>%
  group_by(Marka) %>%
  summarise(ilosc_aut = n()) %>%
  arrange(-ilosc_aut) %>%
  head(1)
## odp.: Volkswagen

## 8. Gdy ograniczyæ siê tylko do aut o przebiegu poni¿ej 50 000 km o silniku diesla, która Marka
##    wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Przebieg.w.km < 50000) %>%
  group_by(Marka) %>%
  summarise(ilosc_aut = n()) %>%
  arrange(-ilosc_aut) %>%
  head(1)
## odp.: BMW

## 9. Spoœród aut marki Toyota wyprodukowanych w 2007 roku, który model jest œrednio najdro¿szy?
auta2012 %>%
  filter(Marka == "Toyota" & Rok.produkcji == "2007") %>%
  group_by(Model) %>%
  summarise(sredniacena = mean(Cena, na.rm = TRUE)) %>%
  arrange(-sredniacena) %>%
  head(1)
## odp.: Prius

## 10. Spoœród aut marki Toyota, który model ma najwiêksz¹ ró¿nicê cen gdy porównaæ silniki benzynowe
##     a diesel?
cena_po_silniku <- auta2012 %>%
  filter(Marka == "Toyota" & Rodzaj.paliwa == c("benzyna", "olej napedowy (diesel)")) %>%
  group_by(Model, Rodzaj.paliwa) %>%
  summarise(cena_nowa2 = ifelse(Rodzaj.paliwa == "benzyna", mean(Cena),
                               ifelse(Rodzaj.paliwa == "olej napedowy (diesel)", mean(Cena), 0))) %>%
  unique()

cena_po_silniku2 <- cena_po_silniku %>% filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>% rename(cena_diesel = cena_nowa2)

cena_po_silniku %>%
  filter(Rodzaj.paliwa == "benzyna") %>%
  rename(cena_benzyna = cena_nowa2) %>%
  left_join(cena_po_silniku2, by = "Model") %>%
  mutate(cena_benzyna=replace(cena_benzyna, is.na(cena_po_silniku$cena_benzyna), 0L)) %>%
  mutate(cena_diesel=replace(cena_diesel, is.na(cena_po_silniku$cena_diesel), 0L)) %>%
  mutate(roznica = abs(cena_diesel - cena_benzyna)) %>%
  arrange(roznica) %>%
  head(1)
## odp.: inny


























