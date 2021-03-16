install.packages("PogromcyDanych")
library(PogromcyDanych)
auta2012



# 1.SprawdŸ ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
auta2012 %>%
  filter(Rodzaj.paliwa=="olej napedowy (diesel)",Rok.produkcji==2007) %>% 
  count()
# Odp: 11621

# 2.Jakiego koloru auta maj¹ najmniejszy medianowy przebieg?
auta2012 %>%
  group_by(Kolor) %>% 
  summarise(Mediana=median(Przebieg.w.km,na.rm = TRUE)) %>% 
  slice_min(Mediana) #wybieramy wiersz o najmniejszej medianie
# Odp: bialy-metalic

# 3.Gdy ograniczyæ siê tylko do aut wyprodukowanych w 2007, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Rok.produkcji==2007) %>% 
  count(Marka) %>% 
  slice_max(n)
# Odp: Volkswagen

# 4.Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtañsza?
#tutaj mozna inaczej rozumiec co znaczy taniosc marki, ja dalem srednia po wszystkich samochodach tej marki
auta2012 %>% 
  filter(Rodzaj.paliwa=="olej napedowy (diesel)",Rok.produkcji==2007) %>% 
  group_by(Marka) %>% 
  summarise(srednia_cena=mean(as.numeric(Cena.w.PLN),na.rm = TRUE)) %>% 
  slice_min(srednia_cena)
# Odp: Aixam

# 5.Spoœród aut marki Toyota, który model najbardziej straci³ na cenie pomiêdzy rokiem produkcji 2007 a 2008.
#srednia cena modeli aut dla roku 2007
cena_2007<-auta2012 %>% 
  filter(Marka=="Toyota",Rok.produkcji==2007) %>% 
  group_by(Model) %>% 
  summarise(srednia_2007=mean(Cena.w.PLN,na.rm = TRUE))
#srednia cena modeli aut dla roku 2008
cena_2008<-auta2012 %>% 
  filter(Marka=="Toyota",Rok.produkcji==2008) %>% 
  group_by(Model) %>% 
  summarise(srednia_2008=mean(Cena.w.PLN,na.rm=TRUE))
#teraz polaczymy tam gdzie mamy z obu lat i policzymy roznice
inner_join(cena_2007,cena_2008) %>% 
  mutate(roznica_cen=srednia_2008-srednia_2007) %>% 
  slice_min(roznica_cen)
# Odp: Hiace

# 6.W jakiej marce klimatyzacja jest najczêœciej obecna?
#przyjmuje ze chodzi o czesc ktora ma klimatyzacje a nie ogolna liczbe
#aby policzyc jak czesto wystepuje klimatyzacja musimy znac ogolna liczbe samochodow danej marki
ilosc_marki  <- auta2012 %>% 
  count(Marka,name="licznosc_ogolem")
auta2012 %>% 
  filter(grepl("klimatyzacja",Wyposazenie.dodatkowe,fixed=TRUE)) %>% #wybieramy tylko te gdzie w wyposazeniu wpisana jest klimatyzacja
  count(Marka,name="licznosc_z_klima") %>%
  left_join(ilosc_marki) %>% 
  mutate(czestosc_klimy=licznosc_z_klima/licznosc_ogolem) %>% 
  filter(czestosc_klimy==max(czestosc_klimy))
# Odp: ex aequo Brilliance, DFSK, GMC,Saturn,Scion,Shuanghuan,Vauxhall

# 7.Gdy ograniczyæ siê tylko do aut z silnikiem ponad 100 KM, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>% 
  filter(KM>100) %>% 
  count(Marka) %>% 
  slice_max(n)
# Odp: Volkswagen

# 8.Gdy ograniczyæ siê tylko do aut o przebiegu poni¿ej 50 000 km o silniku diesla, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Przebieg.w.km<5e+4,Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  count(Marka) %>% 
  slice_max(n)
# Odp: BMW

# 9.Spoœród aut marki Toyota wyprodukowanych w 2007 roku, który model jest œrednio najdro¿szy?
auta2012 %>% 
  filter(Marka=="Toyota",Rok.produkcji==2007) %>% 
  group_by(Model) %>% 
  summarise(srednia_cena=mean(Cena.w.PLN,na.rm=TRUE)) %>% 
  slice_max(srednia_cena)
# Odp: Land Cruiser

# 10.Spoœród aut marki Toyota, który model ma najwiêksz¹ ró¿nicê cen gdy porównaæ silniki benzynowe a diesel
#srednia cena marek z benzyna
auta_benzyna <- auta2012 %>% 
  filter(Rodzaj.paliwa=="benzyna",Marka=="Toyota") %>% 
  group_by(Model) %>% 
  summarise(cena_benzyna=mean(Cena.w.PLN,na.rm=TRUE))

#srednia cena marek z dieslem
auta_diesel <- auta2012 %>% 
  filter(Rodzaj.paliwa=="olej napedowy (diesel)",Marka=="Toyota") %>% 
  group_by(Model) %>% 
  summarise(cena_diesel=mean(Cena.w.PLN,na.rm=TRUE))

#rozpatrujemy tylko tektore sa w obu wiec inner join
inner_join(auta_benzyna,auta_diesel) %>% 
  mutate(roznica_cen=abs(cena_diesel-cena_benzyna)) %>% 
  slice_max(roznica_cen)  

# Odp: Camry