
library(PogromcyDanych)
library(dplyr)
library(tidyr)
## Paraca Domowa 1
## 1. SprawdŸ ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku

auta2012 %>%
  filter(Rok.produkcji == 2007, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  count() %>% 
  rename(diesel_2007 = n)
## odp 11621

## 2. Jakiego koloru auta maj¹ najmniejszy medianowy przebieg?

auta2012 %>%
  filter(!Kolor == "",!is.na(Przebieg.w.km)) %>% 
  group_by(Kolor) %>% 
  summarise(a = median(Przebieg.w.km)) %>% 
  top_n(-1)
## odp bialy-metallic 60000 km

## 3. Gdy ograniczyæ siê tylko do aut wyprodukowanych w 2007, która Marka wystêpuje najczêœciej w zbiorze danych auta 2012
auta2012 %>% 
  filter(Rok.produkcji==2007) %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  top_n(1)
## Volkswagen 1679 razy

## 4. Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtañsza?

df_VAT <- function(Cena.w.PLN)
{
  stawka<-c(123/100)
  wynik<-Cena.w.PLN %*% stawka
  return(wynik)
}

auta_netto <- auta2012 %>%
  filter(Brutto.netto=="netto")
auta_brutto <- auta2012 %>%
  filter(Brutto.netto=="brutto") 
  
cena_brutto<-lapply(auta_netto$Cena.w.PLN, df_VAT)
df_cena_brutto <- data.frame(matrix(unlist(cena_brutto), nrow=length(cena_brutto)))
colnames(df_cena_brutto) <- "Brutto"

auta_netto$Cena.w.PLN <- df_cena_brutto$Brutto
auta_taxes <- full_join(auta_brutto,auta_netto)

auta_taxes %>%
  filter(Rok.produkcji == 2007, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(n=mean(Cena.w.PLN)) %>% 
  top_n(-1)

## Odp. Aixam 13533 PLN


## 5. Spoœród aut marki Toyota, który model najbardziej straci³ na cenie pomiêdzy rokiem produkcji 2007 a 2008.

toyota_2007<- auta_taxes %>% 
  filter(Rok.produkcji == 2007, Marka == "Toyota",!Model=="inny") %>% 
  group_by(Model) %>% 
  summarise(n = mean(Cena.w.PLN))
toyota_2008 <- auta_taxes %>% 
  filter(Rok.produkcji == 2008, Marka == "Toyota",!Model=="inny") %>% 
  group_by(Model) %>% 
  summarise(n = mean(Cena.w.PLN))
toyota<- inner_join(toyota_2007,toyota_2008, by = c("Model"))
toyota$diff<-(toyota$n.x - toyota$n.y)
top_n(toyota,1,diff)

## Odp. Hiace stania³ o 30000 PLN

## 6. W jakiej marce klimatyzacja jest najczêœciej obecna?

AC_included <-auta2012 %>% 
  filter(grepl("klimatyzacja", Wyposazenie.dodatkowe),!Marka=="") %>% 
  group_by(Marka) %>% 
  summarise(AC_on=n())
AC_not_included <- auta %>% 
  filter(!Marka=="") %>% 
  group_by(Marka) %>% 
  summarise(AC_off=n())
AC_on_off <- full_join(AC_included,AC_not_included)
AC_on_off$freq<-(AC_on_off$AC_on / AC_on_off$AC_off)
top_n(AC_on_off,1,freq)
 

## Odp. Brilliance,DFSK,GMC,Saturn,Scion,Shuanghuan,Vauxhall

## 7. Gdy ograniczyæ siê tylko do aut z silnikiem ponad 100 KM, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>% 
  filter(KM > 100) %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  top_n(1)
## Odp. Volkswagen 13317

## 8.Gdy ograniczyæ siê tylko do aut o przebiegu poni¿ej 50 000 km o silniku diesla, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

auta2012 %>% 
  filter(Przebieg.w.km < 50000, Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  top_n(1)

## Odp. BMW 1217

## 9.Spoœród aut marki Toyota wyprodukowanych w 2007 roku, który model jest œrednio najdro¿szy?
auta_taxes %>% 
  filter(Rok.produkcji == 2007, Marka == "Toyota",!Model=="inny") %>% 
  group_by(Model) %>% 
  summarise(n = mean(Cena.w.PLN)) %>% 
  top_n(1)
## Odp. Land Cruiser

## 10.Spoœród aut marki Toyota, który model ma najwiêksz¹ ró¿nicê cen gdy porównaæ silniki benzynowe a diesel?

toyota_petrol<-auta_taxes %>% 
  filter("benzyna"%in% Rodzaj.paliwa, Marka =="Toyota",!Model=="inny") %>% 
  group_by(Model) %>% 
  summarise(price_petrol=mean(Cena.w.PLN))
toyota_diesel <- auta_taxes %>% 
  filter(Rodzaj.paliwa=="olej napedowy (diesel)",Marka == "Toyota",!Model=="inny") %>% 
  group_by(Model) %>% 
  summarise(price_diesel=mean(Cena.w.PLN))

toyota_diesel_petrol<-inner_join(toyota_diesel,toyota_petrol, by = c("Model"))
toyota_diesel_petrol$diff <-abs(toyota_diesel_petrol$price_diesel-toyota_diesel_petrol$price_petrol)
top_n(toyota_diesel_petrol,1,diff)   

## Odp. Camry 44890 PLN
