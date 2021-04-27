## Projekt 2
Drugi projekt będzie poświęcony tematyce diagnozy społecznej, jego celem jest przygotowanie rapotu, który będzie zawierał analizę postawionych pytań badawczych oraz prezentacji, która będzie wnioskami z wykonanych analiz.

Projekt będziemy realizować na podstawie danych Diagnozy Społecznej. 
Diagnoza Społeczna jest próbą uzupełnienia diagnozy opartej na wskaźnikach instytucjonalnych o kompleksowe dane na temat gospodarstw domowych oraz postaw, stanu ducha i zachowań osób tworzących te gospodarstwa; jest diagnozą warunków i jakości życia Polaków w ich własnym sprawozdaniu. Więcej informacji na stronie http://www.diagnoza.com/.

Podczas projektu pracujemy w grupach do 3 osób, przy założeniu, że są to inne osoby niż przy Projekcie 1.

**Na ocenę będzie się składać:**
- raport (15 pkt.)
- prezentacja na wykładzie (5 pkt.) 

### Raport:
Raport w formacie pdf, maksymalnie do 30 stron.

*Punkty:*

- 8 pkt - postawienie pytań badawczych i ich weryfikacja
- 5 pkt - jakość wizualizacji, tabel, opisów (wykresy mają wszystkie niezbędne elementy do - poprawnego odczytania danych (tytuł, podtytuł, adnotacje na osiach, legenda, jednostki, opis jak czytać wykres))
- 2 pkt - wnioski z przeprowadzonych analiz

### Prezentacja
Prezentacja wniosków z analiz podczas wykładu, oceniana w skali [0,5] pkt.

|Etap  | Termin  | Zadania do wykonania| 
|--|--|--|
| I |2021-05-11 | Podział na grupy (do 3 osób). Każda grupa wybiera obszar, w którym chce przeprowadzić badania. Konsultacje na temat wyboru obszaru badań i pomysłów pytań badawczych.|
|II|2021-05-18|Analiza danych - wstępne rezultaty (tabele, wykresy, opisy) dotyczące pytań badawczych.|
|III|2021-05-25|Zarys głównych składowych raportu.|
|V|2021-06-01|Oddanie raportu w postaci pdf. Prezentacja wyników podczas wykładu.|

### Jak wczytać dane?

#### R
```
library(haven)

path <- "nazwa_pliku.sav"
df <- read_sav(path)
```
#### Python
```
import pandas as pd

df = pd.read_spss("nazwa_pliku.sav")
```
