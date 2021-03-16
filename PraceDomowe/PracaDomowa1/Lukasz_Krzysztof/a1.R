library(PogromcyDanych)
library('dplyr')
library('tidyr')


dane <- auta2012


# 1. Sprawdź ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
dane %>%
  filter(Rodzaj.paliwa =='olej napedowy (diesel)'& Rok.produkcji==2007) %>%
  first() %>%
  length()
#odp: 11621

# 2. Jakiego koloru auta mają najmniejszy medianowy przebieg?
dane %>% 
  group_by(Kolor) %>%
  summarize(mediana_przebiegu=median(Przebieg.w.km, na.rm=TRUE)) %>% 
  arrange(mediana_przebiegu) %>% 
  head(1)
#odp: auta w kolorze bialy-metalic

# 3. Gdy ograniczyć się tylko do aut wyprodukowanych w 2007, która Marka występuje najczęściej w zbiorze danych auta2012?
dane %>% 
  filter(Rok.produkcji==2007) %>% 
  group_by(Marka) %>% 
  count() %>% 
  arrange(-n) %>% 
  head(1)
#odp: volkswagen

# 4. Spośród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtańsza?
dane %>% 
  filter(Rok.produkcji==2007 & Rodzaj.paliwa=='olej napedowy (diesel)') %>% 
  group_by(Marka) %>% 
  summarise(srednia_cena=mean(Cena))%>% 
  arrange(srednia_cena) %>% 
  head(1)
#odp: Aixam

# 5. Spośród aut marki Toyota, który model najbardziej stracił na cenie pomiędzy rokiem produkcji 2007 a 2008.
dane %>% 
  filter(Marka=='Toyota'&(Rok.produkcji==2007 | Rok.produkcji ==2008)) %>% 
  group_by(Model, Rok.produkcji) %>% 
  summarize(kolumna = mean(Cena)) %>% 
  pivot_wider(names_from=Rok.produkcji, values_from=kolumna) %>% 
  mutate(roznica=`2007`-`2008`) %>% 
  arrange(-roznica) %>% 
  head(1)
#odp: Prius

# 6. W jakiej marce klimatyzacja jest najczęściej obecna?
dane %>%
  count(Marka)->q1
dane %>% 
  filter(grepl('klimatyzacja', Wyposazenie.dodatkowe)) %>% 
  count(Marka)-> q2
left_join(q1,q2, by='Marka') %>% 
  mutate(ods.klimatyzacji=n.y/n.x) %>% 
  arrange(-ods.klimatyzacji)
#odp: Wszystkie auta marek: Brilliance, DFSK, GMC, Saturn, Scion, Shuanghuan i Vauxhall maja klimatyzacje

# 7. Gdy ograniczyć się tylko do aut z silnikiem ponad 100 KM, która Marka występuje najczęściej w zbiorze danych auta2012?
dane %>% 
  filter(KM>100) %>% 
  group_by(Marka) %>% 
  count() %>% 
  arrange(-n) %>% 
  head(1)
#odp: volkswagen

# 8. Gdy ograniczyć się tylko do aut o przebiegu poniżej 50 000 km o silniku diesla, która Marka występuje najczęściej w zbiorze danych auta2012?
dane %>% 
  filter(Przebieg.w.km<50000 & Rodzaj.paliwa=='olej napedowy (diesel)') %>% 
  group_by(Marka) %>% 
  count() %>% 
  arrange(-n) %>% 
  head(1)
#odp: bmw

# 9. Spośród aut marki Toyota wyprodukowanych w 2007 roku, który model jest średnio najdroższy?
dane %>% 
  filter(Marka=='Toyota'&Rok.produkcji==2007) %>% 
  group_by(Model) %>% 
  summarize(sr.cena = mean(Cena)) %>% 
  arrange(-sr.cena)
#odp: Prius

# 10. Spośród aut marki Toyota, który model ma największą różnicę cen gdy porównać silniki benzynowe a diesel?
dane %>% 
  filter(Marka=='Toyota') %>% 
  group_by(Model, Rodzaj.paliwa) %>% 
  summarize(kolumna = mean(Cena)) %>% 
  pivot_wider(names_from=Rodzaj.paliwa, values_from=kolumna) %>% 
  mutate(roznica=benzyna-`olej napedowy (diesel)`) %>% 
  mutate(roznica_bezwzgledna=abs(roznica)) %>% 
  arrange(-roznica_bezwzgledna) %>% 
  head(2)
#odp: Verso (ignorujemy "inny")


