install.packages("PogromcyDanych")
library(PogromcyDanych)
library(dplyr)
library(tidyr)
auta2012
View(auta2012)

#Zadanie 1
auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)" & Rok.produkcji == 2007) %>% 
  summarise(n=n())
#Odpowiedz: 11621

#Zadanie 2
auta2012 %>% 
  group_by(Kolor) %>% 
  summarise(mediana_przebieg = median(Przebieg.w.km, na.rm = TRUE)) %>% 
  arrange(mediana_przebieg)
#odpowiedz: bialy-metalic

#Zadanie 3
auta2012 %>%
  group_by(Marka) %>% 
  filter(Rok.produkcji == 2007) %>% 
  summarise(n=n()) %>% 
  arrange(-n)
#Odpowiedz: Volkswagen

#Zadanie 4
auta2012 %>%
  group_by(Marka) %>% 
  filter(Rok.produkcji == 2007 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  summarise(srednia_cena = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  arrange(srednia_cena)
#Odpowiedz: Aixam

#Zadanie 5
toyota_2007 <- auta2012 %>%
                  filter(Marka == "Toyota" & Rok.produkcji == 2007) %>%
                  select(Model, Cena.w.PLN) %>%
                  group_by(Model) %>%
                  summarise(srednia_2007 = mean(Cena.w.PLN, na.rm = TRUE))
toyota_2008 <- auta2012 %>%
                  filter(Marka == "Toyota" & Rok.produkcji == 2008) %>%
                  select(Model, Cena.w.PLN) %>% 
                  group_by(Model) %>%
                  summarise(srednia_2008 = mean(Cena.w.PLN, na.rm = TRUE))
toyota_merge <- inner_join(toyota_2007, toyota_2008, by = "Model")
toyota_merge$roznica <- toyota_merge$srednia_2008 - toyota_merge$srednia_2007
toyota_merge %>% 
  arrange(roznica) %>% 
  head(1)
#Odpowiedz: Hiace

#Zadanie 6
auta2012 %>%
  group_by(Marka) %>% 
  filter(grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>%
  summarise(klima_n=n()) %>%
  arrange(-klima_n) %>% 
  head(1)
#Odpowiedz: Volkswagen

#Zadanie 7
auta2012 %>% 
  filter(KM>100) %>% 
  group_by(Marka) %>% 
  summarise(n=n()) %>% 
  arrange(-n)
#Odpowiedz: Volkswagen

#Zadanie 8
auta2012 %>% 
  filter(Przebieg.w.km<50000 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  group_by(Marka) %>%
  summarise(n=n()) %>% 
  arrange(-n)
#Odpowiedz: BMW

#Zadanie 9
auta2012 %>%
  filter(Marka == "Toyota" & Rok.produkcji == 2007) %>% 
  group_by(Model) %>% 
  summarise(cena_2007_toy = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  arrange(-cena_2007_toy)
#Odpowiedz: Land Cruiser

#Zadanie 10
toyota_olej <- auta2012 %>% 
                  filter(Marka == "Toyota" & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
                  select(Model, Cena.w.PLN) %>%
                  group_by(Model) %>%
                  summarise(srednia_olej = mean(Cena.w.PLN, na.rm = TRUE))
toyota_benzyna <- auta2012 %>%
                  filter(Marka == "Toyota" & Rodzaj.paliwa == "benzyna") %>% 
                  select(Model, Cena.w.PLN) %>%
                  group_by(Model) %>%
                  summarise(srednia_benzyna = mean(Cena.w.PLN, na.rm = TRUE))
toyota_paliwa <- inner_join(toyota_olej, toyota_benzyna, by = "Model")
toyota_paliwa$roznica <- abs(toyota_paliwa$srednia_olej - toyota_paliwa$srednia_benzyna)
toyota_paliwa %>%
  arrange(-roznica) %>% 
  head(1)
#Odpowiedz: Camry


  