install.packages("PogromcyDanych")
library(PogromcyDanych)
library(dplyr)
library(tidyr)

#Praca domowa 1 - Katarzyna Grzesiuk


#Zadanie 1
## Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?

auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007) %>%
  count()

##Odpowiedź: 11621 



#Zadanie 2
## Jakiego koloru auta mają najmniejszy medianowy przebieg?

auta2012 %>%
  group_by(Kolor) %>%
  summarise(mediana = median(Przebieg.w.km,na.rm = TRUE))%>%
  arrange(mediana)%>%
  head(1)

##Odpowiedź: bialy-metallic 



#Zadanie 3
## Gdy ograniczyć się tylko do aut wyprodukowanych w 2007, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>%
  filter(Rok.produkcji == 2007) %>%
  group_by(Marka) %>%
  count() %>%
  arrange(-n) %>%
  head(1)

##Odpowiedź: Volkswagen



# Zadanie 4
## Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?

auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007) %>%
  group_by(Marka) %>%
  summarise(srednia = mean(Cena.w.PLN)) %>%
  arrange(srednia) %>%
  head(1)

##Odpowiedź: Aixam



#Zadanie 5
## Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.

toyota_2007 <- auta2012 %>%
  filter(Marka == "Toyota",Rok.produkcji == 2007) %>%
  group_by(Model) %>%
  summarise(srednia_cena2007 = mean(Cena.w.PLN))

toyota_2008 <- auta2012 %>%
  filter(Marka == "Toyota",Rok.produkcji == 2008) %>%
  group_by(Model) %>%
  summarise(srednia_cena2008 = mean(Cena.w.PLN))

toyota_2007 %>%
  left_join(toyota_2008) %>%
  group_by(Model) %>%
  summarise(roznica = srednia_cena2007-srednia_cena2008) %>%
  top_n(1,roznica)

##Odpowiedź: Hiace



#Zadanie 6
## W jakiej marce klimatyzacja jest najczęściej obecna?
### Wersja 1

auta2012 %>%
  group_by(Marka) %>%
  summarise(klimatyzacja = sum(grepl("klimatyzacja", Wyposazenie.dodatkowe))) %>%
  top_n(1,klimatyzacja)

##Odpowiedź: Volkswagen

###Wersja 2

auta2012 %>%
  group_by(Marka) %>%
  summarise(klimatyzacja = sum(grepl("klimatyzacja", Wyposazenie.dodatkowe))/n()) %>%
  top_n(1,klimatyzacja)

##Odpowiedź: Brilliance, DFSK, GMC, Saturn, Scion, Shuanghuan, Vauxhall (klimatyzacja w każdym samochodzie danej marki)


#Zadanie 7
## Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>%
  filter(KM > 100) %>%
  group_by(Marka) %>%
  summarise(n = n()) %>%
  top_n(1,n)

##Odpowiedź: Volkswagen



#Zadanie 8
## Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, która Marka występuje najczęściej w zbiorze danych auta2012?

auta2012 %>%
  filter(Przebieg.w.km < 50000, Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>%
  summarise(n = n()) %>%
  top_n(1,n)

##Odpowiedź: BMW



#Zadanie 9
## Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?

auta2012 %>%
  filter(Marka == "Toyota",Rok.produkcji == 2007) %>%
  group_by(Model) %>%
  summarise(srednia = mean(Cena.w.PLN)) %>%
  top_n(1,srednia)

##Odpowiedź: Land Cruiser



#Zadanie 10
## Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?

toyota_diesel <- auta2012 %>%
  filter(Marka == "Toyota",Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Model) %>%
  summarise(srednia_cena_diesel = mean(Cena.w.PLN))

toyota_benzyna <- auta2012 %>%
  filter(Marka == "Toyota",Rodzaj.paliwa == "benzyna") %>%
  group_by(Model) %>%
  summarise(srednia_cena_benzyna = mean(Cena.w.PLN))

toyota_benzyna %>%
  left_join(toyota_diesel) %>%
  group_by(Model) %>%
  summarise(roznica = abs(srednia_cena_benzyna-srednia_cena_diesel)) %>%
  top_n(1,roznica)

##Odpowiedź: Camry
