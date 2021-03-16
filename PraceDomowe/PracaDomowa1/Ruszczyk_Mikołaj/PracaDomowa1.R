install.packages("PogromcyDanych")
library(PogromcyDanych)
auta2012
head(auta2012)

##1. SprawdŸ ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
auta2012%>%
  filter(Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  filter(Rok.produkcji==2007) %>% 
  dim()
##Odp: 11621

##2. Jakiego koloru auta maj¹ najmniejszy medianowy przebieg?
auta2012 %>% 
  group_by(Kolor) %>% 
  summarize(mediana=median(Przebieg.w.km,na.rm=TRUE)) %>% 
  arrange(mediana)
##Odp: bia³y metallic

##3. Gdy ograniczyæ siê tylko do aut wyprodukowanych w 2007, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Rok.produkcji==2007) %>% 
  group_by(Marka) %>% 
  summarize(n=n()) %>% 
  arrange(-n)
##Odp: Volkswagen

##4. Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtañsza?
auta2012 %>% 
  filter(Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  filter(Rok.produkcji==2007) %>% 
  group_by(Marka) %>% 
  summarize(œrednia=mean(Cena.w.PLN)) %>% 
  arrange(œrednia)
##Odp. Aixam

##5. Spoœród aut marki Toyota, który model najbardziej straci³ na cenie pomiêdzy rokiem produkcji 2007 a 2008.
toyota_2007<-auta2012 %>% 
  filter(Marka=="Toyota") %>% 
  filter(Rok.produkcji==2007) %>% 
  group_by(Model) %>% 
  summarize(œrednia_cena_2007=mean(Cena.w.PLN))
toyota_2008<-auta2012 %>% 
  filter(Marka=="Toyota") %>% 
  filter(Rok.produkcji==2008) %>% 
  group_by(Model) %>% 
  summarize(œrednia_cena_2008=mean(Cena.w.PLN))
toyota_2007 %>% 
  inner_join(toyota_2008,by="Model") %>% 
  mutate(spadek_ceny=œrednia_cena_2008-œrednia_cena_2007) %>% 
  arrange(spadek_ceny)
  
##Odp: Najwiêkszy spadek pomiêdzy rokiem produkcji 2007 i 2008 zaliczy³ model Hiace

##6. W jakiej marce klimatyzacja jest najczêœciej obecna?

  
##7. Gdy ograniczyæ siê tylko do aut z silnikiem ponad 100 KM, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

auta2012 %>% 
  filter(KM>100) %>% 
  count(Marka) %>% 
  arrange(-n)
##Odp: Volkswagen

##8. Gdy ograniczyæ siê tylko do aut o przebiegu poni¿ej 50 000 km o silniku diesla, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?
auta2012 %>% 
  filter(Przebieg.w.km<50000) %>% 
  count(Marka) %>% 
  arrange(-n)
##Odp: Volkswagen

##9. Spoœród aut marki Toyota wyprodukowanych w 2007 roku, który model jest œrednio najdro¿szy?
auta2012 %>% 
  filter(Marka=="Toyota") %>% 
  filter(Rok.produkcji==2007) %>% 
  group_by(Model) %>% 
  summarize(œrednia_cena=mean(Cena.w.PLN)) %>% 
  arrange(-œrednia_cena)
##Odp: Land Cruiser

##10. Spoœród aut marki Toyota, który model ma najwiêksz¹ ró¿nicê cen gdy porównaæ silniki benzynowe a diesel?
toyota_benz<-auta2012 %>% 
  filter(Marka=="Toyota") %>% 
  filter(Rodzaj.paliwa=="benzyna") %>% 
  group_by(Model) %>% 
  summarize(œrednia_cena_benz=mean(Cena.w.PLN))

toyota_diesel<-auta2012 %>% 
  filter(Marka=="Toyota") %>% 
  filter(Rodzaj.paliwa=="olej napedowy (diesel)") %>% 
  group_by(Model) %>% 
  summarize(œrednia_cena_diesel=mean(Cena.w.PLN))

toyota_diesel %>% 
  inner_join(toyota_benz,by="Model") %>% 
  mutate(ró¿nica_cen =abs(œrednia_cena_diesel-œrednia_cena_benz)) %>% 
  arrange(-ró¿nica_cen)

##Odp:Camry

########################
