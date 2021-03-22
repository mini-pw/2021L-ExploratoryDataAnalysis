## Praca domowa nr. 1
library(PogromcyDanych)
head(auta2012)
# Zadanie 1
# SprawdŸ ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
x <- auta2012 %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007)
dim(x)[1]

# Zadanie 2
# Jakiego koloru auta maj¹ najmniejszy medianowy przebieg?
x <- auta2012 %>% 
 group_by(Kolor) %>%
   summarise(mediana = median(Przebieg.w.km, na.rm = TRUE)) %>% 
    arrange(mediana ,desc = FALSE)

x[1,1]
# Zadanie 3
# Gdy ograniczyæ siê tylko do aut wyprodukowanych w 2007, która Marka wystêpuje najczêœciej w zbiorze danych auta2012?

x <- auta2012 %>% 
  filter(Rok.produkcji == 2007) %>% 
    group_by(Marka) %>% 
      summarise(n =n()) %>% 
        arrange(-n)
  
x[1,1]  

# Zadanie 4
# Spoœród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtañsza?
x <- auta2012 %>% 
      filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007) %>% 
        group_by(Marka) %>% 
          summarise(œrednia_cena = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
            arrange(œrednia_cena)
x[1,1]

# Zadanie 5
# Spoœród aut marki Toyota, który model najbardziej straci³ na cenie pomiêdzy rokiem produkcji 2007 a 2008.
x <-auta2012 %>% 
      filter( Marka == "Toyota") %>% 
        group_by(Model) %>% 
          filter(Rok.produkcji == 2007) %>% 
             summarise(œrednia_cena_2007 = mean(Cena.w.PLN, na.rm = TRUE))

y <- auta2012 %>% 
  filter( Marka == "Toyota") %>% 
    group_by(Model) %>% 
      filter(Rok.produkcji == 2008) %>% 
         summarise(œrednia_cena_2008 = mean(Cena.w.PLN, na.rm = TRUE))
y
z <- inner_join(x,y)
z <- z %>% 
  transmute(z,ró¿nica = z[,2]-z[,3]) %>% 
    arrange(-ró¿nica)
z[1,1]


# Zadanie 6
# W jakiej marce klimatyzacja jest najczêœciej obecna?

z <- auta2012 %>% 
  mutate(Wyposazenie.dodatkowe_splited = strsplit(as.character(auta2012$Wyposazenie.dodatkowe), split = ", ")) %>% 
  filter("klimatyzacja" %in% Wyposazenie.dodatkowe_splited) %>% 
  group_by(Marka) %>% 
  summarise(n =n()) %>% 
  arrange(-n)
  
z[1,1]
  

      
strsplit(as.character(auta2012$Wyposazenie.dodatkowe), split = ", ")




# Zadanie 7
# Gdy ograniczyæ siê tylko do aut z silnikiem ponad 100 KM, która Marka wystêpuje najczêœciej w zbiorze 
# danych auta2012?
x <- auta2012 %>% 
   filter(KM >= 100) %>% 
   group_by(Marka) %>% 
   summarise(n =n()) %>% 
   arrange(-n)

x[1,1]
# Zadanie 8
# Gdy ograniczyæ siê tylko do aut o przebiegu poni¿ej 50 000 km o silniku diesla,
# która Marka wystêpuje najczêœciej w zbiorze danych auta2012? 

x <- auta2012 %>% 
    filter(Przebieg.w.km <= 50000 & Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
    group_by(Marka) %>% 
    summarise(n =n()) %>% 
    arrange(-n)
    
x[1, 1]
  
# Zadanie 9
# Spoœród aut marki Toyota wyprodukowanych w 2007 roku, który model jest œrednio najdro¿szy?

x <- auta2012 %>% 
  filter(Marka == "Toyota" & Rok.produkcji == 2007) %>% 
  group_by(Model) %>% 
  summarise(œrednia_cena = mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  arrange(-œrednia_cena)

x[1,1]

# Zadanie 10
# Spoœród aut marki Toyota, który model ma najwiêksz¹ ró¿nicê cen gdy porównaæ silniki benzynowe a diesel?

x <-auta2012 %>% 
  filter( Marka == "Toyota") %>% 
  group_by(Model) %>% 
  filter(Rodzaj.paliwa == "olej napedowy (diesel)") %>% 
  summarise(œrednia_cena_diesel = mean(Cena.w.PLN, na.rm = TRUE))

y <- auta2012 %>% 
  filter( Marka == "Toyota") %>% 
  group_by(Model) %>% 
  filter(Rodzaj.paliwa == "benzyna") %>% 
  summarise(œrednia_cena_benzyna = mean(Cena.w.PLN, na.rm = TRUE))
z <- inner_join(x,y)
z
z <- z %>% 
  mutate(ró¿nica = abs(z[,2]-z[,3])) %>% 
  arrange(-ró¿nica)
z
z[1,1]





auta2012













