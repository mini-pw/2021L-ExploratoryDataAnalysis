#install.packages("PogromcyDanych")
library(PogromcyDanych)
library(dplyr)
#auta2012



##1.  SprawdŸ ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
dim(auta2012 %>% 
      filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji == 2007))
# Odp: 11621



##2.  Jakiego koloru auta maj¹ najmniejszy medianowy przebieg?
auta2012 %>% 
  group_by(Kolor) %>% 
  summarise(mediana = median(Przebieg.w.km, na.rm = TRUE)) %>% 
  arrange(mediana)
# Odp: bia³y-metalic



##3.  Gdy ograniczyæ siê tylko do aut wyprodukowanych w 2007, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Rok.produkcji == 2007) %>% 
  group_by(Marka) %>% 
  summarise(ilosc = n()) %>% 
  arrange(-ilosc)
# Odp: Volkswagen



##4.  Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtañsza?
auta2012 %>% 
  filter(Rok.produkcji == 2007 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) %>% 
  arrange(srednia_cena)
# Odp: Aixam



##5.  Spoœród aut marki Toyota, który model najbardziej straci³‚ na cenie pomiêdzy rokiem produkcji 2007 a 2008.
x5_1 <- auta2012 %>% 
  filter(Rok.produkcji == 2007 & Marka == "Toyota") %>%
  group_by(Model) %>%
  summarise(cena_2007 = mean(Cena.w.PLN))
x5_2 <- auta2012 %>% 
  filter(Rok.produkcji == 2008 & Marka == "Toyota") %>%
  group_by(Model) %>%
  summarise(cena_2008 = mean(Cena.w.PLN))
zadanie5 <- x5_1 %>% 
  inner_join(x5_2, by="Model") %>% 
  mutate(roznica = (cena_2008 - cena_2007)) %>% 
  arrange(roznica)
# Odp: Hiace



##6.  W jakiej marce klimatyzacja jest najczêœciej obecna?
auta2012 %>% 
  mutate(wyposazenie = strsplit(as.character(Wyposazenie.dodatkowe), ", ")) %>% 
  mutate(klimatyzacja = sapply(wyposazenie, function(x) "klimatyzacja" %in% x)) %>% 
  filter(klimatyzacja == TRUE) %>% 
  group_by(Marka) %>% 
  summarise(z_klimatyzacja = n()) %>% 
  arrange(-z_klimatyzacja)
# Odp: Volkswagen



##7.  Gdy ograniczyæ siê tylko do aut z silnikiem ponad 100 KM, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>% 
  filter(KM > 100) %>% 
  group_by(Marka) %>% 
  summarise(ilosc = n()) %>% 
  arrange(-ilosc)
# Odp: Volkswagen


##8.  Gdy ograniczyæ siê tylko do aut o przebiegu poni¿ej 50 000 km o silniku diesla, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Przebieg.w.km < 50000 & Rodzaj.paliwa == "olej napedowy (diesel)")%>% 
  group_by(Marka) %>% 
  summarise(ilosc = n()) %>% 
  arrange(-ilosc)
# Odp: BMW


##9.  Spoœród aut marki Toyota wyprodukowanych w 2007 roku, który model jest œrednio najdro¿szy?
auta2012 %>% 
  filter(Rok.produkcji == 2007, Marka=="Toyota") %>% 
  group_by(Model) %>% 
  summarise(sredna_cena = mean(Cena.w.PLN)) %>% 
  arrange(-sredna_cena)
# Odp: Land Cruiser



##10.  Spoœród aut marki Toyota, który model ma najwiêksz¹ ró¿nicê cen gdy porównaæ silniki benzynowe a diesel?
x10_1 <- auta2012 %>% 
  filter(Marka == "Toyota" & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Model) %>%
  summarise(cena_diesel = mean(Cena.w.PLN))
x10_2 <- auta2012 %>% 
  filter(Marka == "Toyota" & Rodzaj.paliwa == "benzyna") %>%
  group_by(Model) %>%
  summarise(cena_benzyna = mean(Cena.w.PLN))
zadanie10 <- x10_1 %>% 
  inner_join(x10_2, by="Model") %>% 
  mutate(roznica = abs(cena_diesel - cena_benzyna)) %>% 
  arrange(-roznica)
# Odp: Camry