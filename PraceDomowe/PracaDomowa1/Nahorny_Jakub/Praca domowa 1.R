install.packages("PogromcyDanych")
library(PogromcyDanych)
auta2012

## 1. Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?

dim(auta2012 %>% 
  filter(Rok.produkcji == 2007 & Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  select(Marka))

##Odp: 11621

## 2. Jakiego koloru auta mają najmniejszy medianowy przebieg?

auta2012 %>% 
  group_by(Kolor) %>% 
  summarise(mediana = median(Przebieg.w.km, na.rm = TRUE)) %>% 
  arrange(mediana) %>% 
  head(1)

##Odp: bialy-mettalic

## 3. Gdy ograniczyć się tylko do aut wyprodukowanych w 2007, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>%
  filter(Rok.produkcji == 2007) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

##Odp: Volkswagen

## 4. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?
auta2012 %>% 
  filter(Rok.produkcji == 2007 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(mean = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  arrange(mean) %>% 
  head(1)

##Odp: Aixam

## 5. Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.

mod2007 <- (auta2012 %>% 
  filter(Marka == "Toyota" & Rok.produkcji == 2007) %>% 
  group_by(Model) %>% 
  summarise(mean2007 = mean(Cena.w.PLN, na.rm = TRUE)))

mod2008 <- (auta2012 %>% 
              filter(Marka == "Toyota" & Rok.produkcji == 2008) %>% 
              group_by(Model) %>% 
              summarise(mean2008 = mean(Cena.w.PLN, na.rm = TRUE)))

nowa <- merge(mod2007,mod2008,by.x="Model",by.y="Model")

nowa %>% 
  mutate(roznica = mean2008-mean2007) %>% 
  arrange(-roznica) %>% 
  head(1)

##Odp: Land Cruiser

## 6. W jakiej marce klimatyzacja jest najczęściej obecna?

install.packages("stringi")
library(stringi)

zklima <- stri_detect_fixed(auta2012$Wyposazenie.dodatkowe, pattern = "klimatyzacja")
nowa2 <- cbind(auta2012, zklima)

nowa3 <- nowa2 %>% 
  filter(zklima == TRUE) %>% 
  group_by(Marka) %>% 
  summarise(n = n())

nowa2 %>% 
  group_by(Marka) %>% 
  summarise(m = n()) %>% 
  left_join(nowa3) %>% 
  group_by(Marka) %>% 
  summarise(procent = n/m*100) %>% 
  arrange(-procent)

##Odp: Brilliance, DFSK, GMC, Saturn, Scion, Shuanghuan, Vauxhall

## 7. Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>%
  filter(KM > 100) %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

##Odp: Volkswagen

## 8. Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>%
  filter(Przebieg.w.km < 50000 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  head(1)

## Odp: BMW

## 9. Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?

auta2012 %>% 
  filter(Marka == "Toyota" & Rok.produkcji == 2007) %>% 
  group_by(Model) %>% 
  summarise(mean = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  arrange(-mean) %>% 
  head(1)

##Odp: Land Cruiser

## 10. Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?

diesel <- (auta2012 %>% 
  filter(Marka == "Toyota" & Rodzaj.paliwa== "olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarise(mean = mean(Cena.w.PLN, na.rm = TRUE)))

benzyna <- (auta2012 %>% 
            filter(Marka == "Toyota" & Rodzaj.paliwa== "benzyna") %>% 
            group_by(Model) %>% 
            summarise(mean = mean(Cena.w.PLN, na.rm = TRUE)))

merge(diesel, benzyna, by.x="Model",by.y="Model") %>% 
  mutate(roznica = abs(mean.x-mean.y)) %>% 
  arrange(-roznica) %>% 
  head(1)

##Odp: Camry