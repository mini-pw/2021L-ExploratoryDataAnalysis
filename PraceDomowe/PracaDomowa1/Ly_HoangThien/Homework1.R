######################
##### HOMEWORK 1 #####
### Lý Hoàng Thiên ###
######################

# Loading data set: auta2012 from package: PogromcyDanych.
install.packages("PogromcyDanych")
library(PogromcyDanych)
auta2012
?auta2012

# Loading dplyr package
library(dplyr)





## 1) Sprawdz ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
## (Find out how many diesel cars are made in 2007?)
auta2012 %>% filter(Rok.produkcji== 2007, Rodzaj.paliwa=="olej napedowy (diesel)") %>% summarize(n())
    #Odpowiedz: 11621





## 2) Jakiego koloru auta maja najmniejszy medianowy przebieg?
## (What color of cars have the lowest median mileage in km?)
auta2012 %>% group_by(Kolor) %>% summarize(mediana=median(Przebieg.w.km,na.rm=TRUE)) %>% arrange(mediana)  %>% head(1)
    #Odpowiedz: bialy-metallic





## 3) Gdy ograniczyc sie tylko do aut wyprodukowanych w 2007, która Marka wystepuje najczesciej w zbiorze danych auta2012?
## (When we limit to only cars made in 2007, which Brand #Marka occurs most frequently in the auta2012 dataset?)
auta2012 %>% filter(Rok.produkcji==2007) %>% group_by(Marka) %>% summarize(frequency=n()) %>% top_n(1,frequency)
    #Odpowiedz: Volkswagen






## 4) Sposród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtansza?
##  (Among the diesel cars produced in 2007, which brand is the cheapest? #with the notice respect to currency, 
##   hence, we will compace the prices in PLN):
auta2012 %>% filter(Rok.produkcji==2007, Rodzaj.paliwa=="olej napedowy (diesel)") %>%
          group_by(Marka) %>%
          summarize(price_mean=mean(Cena.w.PLN)) %>% arrange(price_mean) %>% head(1)
    #Odpowiedz: Aixam




## 5) Sposród aut marki Toyota, który model najbardziej stracil na cenie pomiedzy rokiem produkcji 2007 a 2008.
##  (Among Toyota cars, which model lost the most in price between the production years 2007 and 2008.)
Toyota_2007 <- auta2012 %>% filter(Marka=="Toyota", Rok.produkcji==2007) %>% 
             select(Model,Cena.w.PLN) %>% group_by(Model) %>% summarize(price_mean_2007=mean(Cena.w.PLN,na.rm = TRUE))


Toyota_2008 <- auta2012 %>% filter(Marka=="Toyota", Rok.produkcji==2008) %>%  
             select(Model,Cena.w.PLN) %>% group_by(Model) %>% summarize(price_mean_2008=mean(Cena.w.PLN,na.rm = TRUE))

  
  #We will use inner join to find the fluctuation (difference) between mean_prices of Toyota car models in 2007 and 2008.
Toyota_2007_2008 <- merge(Toyota_2007,Toyota_2008,by="Model")

  # Final step: finding the Toyota model lost the most in mean_price between 2007 and 2008:
Toyota_2007_2008 %>% mutate(price_lost= price_mean_2008-price_mean_2007)  %>% arrange(price_lost) %>%head(1)

  # ODPOWIEDZ: Hiace.




## 6) W jakiej marce klimatyzacja jest najczesciej obecna?
##    (In which brand is A/C most often found?)

trimmed_auta2012 <- auta2012  %>% select(Marka,Wyposazenie.dodatkowe)

trimmed_auta2012 <- trimmed_auta2012 %>% mutate_if(is.factor,as.character) 

trimmed_auta2012 %>% group_by(Marka) %>% mutate(n_AC= length(grep("klimatyzacja",Wyposazenie.dodatkowe,fixed=TRUE))) %>%
                     distinct(Marka, .keep_all = TRUE) %>% arrange(-n_AC) %>% head(1)

   # Odpowiedz: Volkswagen



## 7) Gdy ograniczyc sie tylko do aut z silnikiem ponad 100 KM, która Marka wystepuje najczesciej w zbiorze danych auta2012?
##  (When limited only to cars with engines over 100 hp, which Brand occurs most frequently in the auta2012 dataset
auta2012 %>% filter(KM > 100) %>% group_by(Marka)%>% summarize(n=n()) %>% top_n(1,n)

   # Odpowiedz:  Volkswagen



## 8) Gdy ograniczyc sie tylko do aut o przebiegu ponizej 50 000 km o silniku diesla, 
## która Marka wystepuje najczesciej w zbiorze danych auta2012? 
## (When limited only to cars with less than 50,000 miles with a diesel engine, 
## which Brand occurs most frequently in the auta2012 dataset?)
auta2012 %>% filter(Przebieg.w.km < 50000,Rodzaj.paliwa=="olej napedowy (diesel)") %>% group_by(Marka) %>% 
             summarize(n=n()) %>% top_n(1,n) 

   # Odpowiedz:  BMW



## 9) Sposród aut marki Toyota wyprodukowanych w 2007 roku, który model jest srednio najdrozszy?
## (Among the Toyota cars produced in 2007, which model is on average the most expensive?)
auta2012  %>% filter(Rok.produkcji==2007,Marka=="Toyota") %>% group_by(Model) %>% 
              summarize(price_mean=mean(Cena.w.PLN,na.rm = TRUE)) %>% top_n(1,price_mean)

   # Odpowiedz: Land Cruiser


## 10) Sposród aut marki Toyota, który model ma najwieksza róznice cen gdy porównac silniki benzynowe a diesel?
## (Among Toyota cars, which model has the biggest price difference when you compare petrol and diesel engines?)
toyota_benzynowe  <- auta2012 %>% filter(Marka=="Toyota",Rodzaj.paliwa=="benzyna") %>% group_by(Model) %>% 
                      summarize(price_mean_benzynowe=mean(Cena.w.PLN,na.rm = TRUE))

toyota_diesel     <- auta2012 %>% filter(Marka=="Toyota",Rodzaj.paliwa=="olej napedowy (diesel)") %>% group_by(Model) %>%
                      summarize(price_mean_diesel   = mean(Cena.w.PLN,na.rm = TRUE))


merge(toyota_benzynowe,toyota_diesel,by="Model") %>% mutate(dif_price=abs(price_mean_benzynowe-price_mean_diesel)) %>% 
                    top_n(1,dif_price)

   # Odpowiedz: Camry