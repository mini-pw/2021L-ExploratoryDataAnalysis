install.packages("PogromcyDanych")
library(PogromcyDanych)
auta2012

View(auta2012)
library(dplyr)
library(tidyr)

#Komentarz w ramach wstêpu: Celem pracy domowej by³o æwiczenie pakietów dplyr i tidyr, a nie analiza statystyczna tych danych, dlatego nad niektórymi z odpowiedzi 
#                           mo¿na by³oby siê dalej zastanowiæ, czy faktycznie stanowi¹ najtrafniejsz¹ odpowiedŸ na zadane pytanie.
#                           Nie chcia³em jednak pracy zbytnio niepotrzebnie komplikowaæ, dlatego poprzesta³em na czymœ podobnym co robiliœmy na laboratoriach. 
#                           (Przyk³ad w komentarzu do zadania 10)

#1.SprawdŸ ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?

auta2012 %>% 
  filter(Rok.produkcji == 2007,Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  summarise('Liczba dieseli w 2007 roku'=n())
  
#Odp: 11621

#2.Jakiego koloru auta maj¹ najmniejszy medianowy przebieg?
  
  auta2012 %>% 
    group_by(Kolor) %>% 
    summarise(Medianowy_przebieg = median(Przebieg.w.km,na.rm =TRUE)) %>% 
    arrange(Medianowy_przebieg) %>% 
    head(1) %>% 
    select(1)

#Odp: bialy-metallic
    
#3.Gdy ograniczyæ siê tylko do aut wyprodukowanych w 2007, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

auta2012 %>% 
  filter(Rok.produkcji==2007) %>% 
  group_by(Marka) %>% 
  summarise(Ilosc_marka=n()) %>% 
  top_n(1) %>% 
  select(1)

#Odp: Volkswagen

#4.Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtañsza?

auta2012 %>% 
  filter(Rodzaj.paliwa=="olej napedowy (diesel)",Rok.produkcji==2007) %>% 
  group_by(Marka) %>% 
  summarise(Srednia_cena=mean(Cena.w.PLN,na.rm = TRUE)) %>% 
  arrange(Srednia_cena) %>%
  head(1) %>% 
  select(1)
 
#odp: Aixam

#5.Spoœród aut marki Toyota, który model najbardziej straci³‚ na cenie pomiêdzy rokiem produkcji 2007 a 2008.

toyota1<-auta2012 %>% 
  filter(Marka=="Toyota") %>% 
  group_by(Model,Rok.produkcji) %>% 
  filter(Rok.produkcji == 2007 | Rok.produkcji==2008) %>% 
  select(Cena.w.PLN,Model,Rok.produkcji) %>% 
  summarise(Srednia = mean(Cena.w.PLN, na.rm = TRUE)) 

toyota2007<-toyota1 %>% 
  filter(Rok.produkcji==2007)

toyota2008<-toyota1 %>% 
  filter(Rok.produkcji==2008)
  
toyota2007 %>% 
  inner_join(toyota2008,by='Model') %>% 
  summarise(Spadek_ceny=Srednia.y-Srednia.x) %>%  
  arrange(-Spadek_ceny) %>% 
  head(1) %>% 
  select(1)


#Odp: Land Cruiser

#6.W jakiej marce klimatyzacja jest najczêœciej obecna?

auta2012 %>% 
  group_by(Marka) %>% 
  mutate(Klimatyzacja = grepl(pattern="Klimatyzacja",Wyposazenie.dodatkowe)) %>% 
  select(Klimatyzacja) %>% 
  summarise(Ilosc=n()) %>% 
  top_n(1)
 

#Odp: Volkswagen

#7.Gdy ograniczyæ siê tylko do aut z silnikiem ponad 100 KM, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

auta2012 %>% 
  filter(KM>100) %>% 
  group_by(Marka) %>% 
  summarise(Ilosc=n()) %>% 
  top_n(1) %>% 
  select(1)
  
#Odp: Volkswagen

#8.Gdy ograniczyæ siê tylko do aut o przebiegu poni¿ej 50 000 km o silniku diesla, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

auta2012 %>% 
  filter(Przebieg.w.km<50000) %>% 
  group_by(Marka) %>% 
  summarise(Ilosc=n()) %>% 
  top_n(1) %>% 
  select(1)

#Odp:Volkswagen

#9.Spoœród aut marki Toyota wyprodukowanych w 2007 roku, który model jest œrednio najdro¿szy?

auta2012 %>% 
  filter(Marka=="Toyota",Rok.produkcji==2007) %>% 
  group_by(Model) %>% 
  summarise(mean(Cena.w.PLN,na.rm = TRUE)) %>% 
  top_n(1) %>% 
  select(1)

#Odp: Land Cruiser


#10.Spoœród aut marki Toyota, który model ma najwiêksz¹ ró¿nicê cen gdy porównaæ silniki benzynowe a diesel?

toyota2<-auta2012 %>% 
  filter(Marka=="Toyota",Rodzaj.paliwa=="benzyna"|Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  group_by(Model,Rodzaj.paliwa) %>% 
  select(Cena.w.PLN,Model,Rodzaj.paliwa) %>% 
  summarise(Srednia_cena=mean(Cena.w.PLN,na.rm=TRUE))

toyotaD<-toyota2 %>% 
  filter(Rodzaj.paliwa=='olej napedowy (diesel)')

toyotaB<-toyota2 %>% 
  filter(Rodzaj.paliwa=="benzyna")

toyotaD %>% 
  inner_join(toyotaB,by="Model") %>% 
  summarise(Roznica_cen=abs(Srednia_cena.y-Srednia_cena.x)) %>% 
  top_n(1) %>% 
  select(1)


#Odp: Camry. Najwiêksz¹ ró¿nicê cen zinterpretowa³em jako najwiêksz¹ ró¿nicê œredniej ceny. Nie uwa¿am, ¿eby to by³o idealne kryterium,
#            weŸmy na przyk³ad pod uwagê sytuacjê, w której bêdziemy mieæ rzadki model samochodu, z którego sprzeda³o siê po jednej sztuce diesla i benzynowego,
#            diesel jednak by³ wyprodukowany w 2016 roku a benzynowy w 2005, zatem oczywiœcie ró¿nica cen jest bardzo du¿a, ale nie ma wiele wspólnego z tym,
#            czy samochód to diesel czy benzyna, a z jego rokiem produkcji. W³aœciwa interpretacja statystyczna nie jest jednak g³ównym celem tej pracy domowej, 
#            dlatego pozwoli³em sobie na tego rodzaju uproszczenia skupiaj¹c g³ównie na zastosowaniu R i pakietów dplyr i tidyr.