library("ggplot2")
library(readr)
library(data.table)
library(stringr)
library(splitstackshape)

options(scipen = 999)

budgets <- fread("budgets.csv")
movies <- fread('title.principals.tsv.gz')
titles <- fread('title.basics.tsv.gz')
names <- fread('name.basics.tsv.gz')

budgets <- budgets[,pomoc:=strsplit(ReleaseDate, ', ')]
budgets <- budgets[, title_year:=as.integer(sapply(pomoc, function(x) x[2]))]

movie_metadata <- fread("movie_metadata.csv")

mov <- fread("movie_metadata.csv")
mov <- mov[,pomoc:=strsplit(movie_imdb_link, '/')]
mov <- mov[, tconst:=sapply(pomoc, function(x) x[5])]
mov <- mov[,.(director_name, actor_1_name, actor_2_name, actor_3_name, tconst, title_year)]
mov2 <- mov[titles, on="tconst", nomatch=NULL]

budgets$ProductionBudget = gsub(",", "", budgets$ProductionBudget)
budgets$ProductionBudget = as.numeric(gsub("\\$", "", budgets$ProductionBudget))
budgets$WorldwideGross = gsub(",", "", budgets$WorldwideGross)
budgets$WorldwideGross = as.numeric(gsub("\\$", "", budgets$WorldwideGross))
budgets <- budgets[,.(Movie, ProductionBudget, WorldwideGross, title_year)]


mov3 <- merge(mov2, budgets, by.x=c("primaryTitle", "title_year"), by.y=c("Movie", "title_year"))
mov3 <- mov3[,difference:=WorldwideGross- ProductionBudget]


# Wykres zarobków aktorów pierwszoplanowych

aktor1planowy <- mov3[,sum(difference), by=actor_1_name]
aktor1planowy <- aktor1planowy[aktorzy, on=("actor_1_name==primaryName"), nomatch=0]
aktor1planowy <- setorder(aktor1planowy, -V1)


aktor1wykres <- aktor1planowy[1:20,]
ggplot(aktor1wykres, aes(x=reorder(actor_1_name, V1), y=V1))+
  geom_col()+
  coord_flip()+
  labs(title = "Zyski z filmów, w których aktor grał główną rolę",
       x="Aktor",
       y="Zyski w USD ")+
  scale_y_continuous(expand=c(0,0))


# NIEUŻYWANE

# aktor1srednia <- mov3[,median(difference), by=actor_1_name]
# aktor1srednia <- setorder(aktor1srednia, -V1)
# aktor1srwykres <- aktor1srednia[1:20,]
# 
# 
# ggplot(aktor1srwykres, aes(x=reorder(actor_1_name, -V1), y=V1))+
#   geom_col()+
#   coord_flip()+
#   labs(title = "Zyski z filmów, w których aktor grał główną rolę",
#        x="Aktor",
#        y="Średni zysk z filmów")+
#   scale_y_continuous(expand=c(0,0))


# # Wykres zarobków reżyser

rezyser <- mov3[,sum(difference), by=director_name]
rezyser <- setorder(rezyser, -V1)

rezyserwykres<- rezyser[1:20,]
ggplot(rezyserwykres, aes(x=reorder(director_name, V1), y=V1))+
  geom_col()+
  coord_flip()+
  labs(title = "Zyski z wyreżyserowanych filmów",
       x="Reżyser",
       y="Zyski w USD ")+
  scale_y_continuous(expand=c(0,0))

# NIEUŻYWANE
#
#
# rezysersredni <- mov3[,median(difference), by=director_name]
# rezysersredni <- setorder(rezysersredni, -V1)
# rezysersrwykres<- rezysersredni[1:20,]
# ggplot(rezysersrwykres, aes(x=reorder(director_name, -V1), y=V1))+
#   geom_col()+
#   coord_flip()+
#   labs(title = "Zyski z wyreżyserowanych filmów",
#        x="Reżyser",
#        y="Średni zysk z filmów")+
#   scale_y_continuous(expand=c(0,0))
# 
#
# # Wykres zarobkow aktorow drugoplanowych
# 
# aktor2planowy <- mov3[,sum(difference), by=actor_2_name]
# aktor2planowy <- setorder(aktor2planowy, -V1)
# 
# aktor2wykres <- aktor2planowy[1:20,]
# ggplot(aktor2wykres, aes(x=reorder(actor_2_name, -V1), y=V1))+
#   geom_col()+
#   coord_flip()+
#   labs(title = "Zyski z filmów, w których aktor grał drugą rolę",
#        x="Aktor",
#        y="Zyski w USD ")+
#   scale_y_continuous(expand=c(0,0))
# 
# aktor3planowy <- mov3[,sum(difference), by=actor_3_name]
# aktor3planowy <- setorder(aktor3planowy, -V1)
# 
# aktor3wykres <- aktor3planowy[1:20,]
# ggplot(aktor3wykres, aes(x=reorder(actor_3_name, -V1), y=V1))+
#   geom_col()+
#   coord_flip()+
#   labs(title = "Zyski z filmów, w których aktor grał trzecią rolę",
#        x="Aktor",
#        y="Zyski w USD ")+
#   scale_y_continuous(expand=c(0,0))



aktorzy <- unique(names[grepl("actor", primaryProfession, ignore.case = TRUE),.(primaryName)])

# Wykres zarobków aktorek pierwszoplanowych

aktorki1planowe <- aktor1planowy[!aktorzy, on=("actor_1_name==primaryName")]
aktorki1wykres <- aktorki1planowe[1:20,]
ggplot(aktorki1wykres, aes(x=reorder(actor_1_name, V1), y=V1))+
  geom_col()+
  coord_flip()+
  labs(title = "Zyski z filmów, w których aktorka grała główną rolę",
       x="Aktorka",
       y="Zyski w USD ")+
  scale_y_continuous(expand=c(0,0))


# aktorki2planowe <- aktor2planowy[!aktorzy, on=("actor_2_name==primaryName")]
# aktorki2wykres <- aktorki2planowe[1:20,]
# ggplot(aktorki2wykres, aes(x=reorder(actor_2_name, -V1), y=V1))+
#   geom_col()+
#   coord_flip()+
#   labs(title = "Zyski z filmów, w których aktorka grała drugą rolę",
#        x="Aktorka",
#        y="Zyski w USD ")+
#   scale_y_continuous(expand=c(0,0))
# 
# 
# 
# aktorki3planowe <- aktor3planowy[!aktorzy, on=("actor_3_name==primaryName")]
# aktorki3wykres <- aktorki3planowe[1:20,]
# ggplot(aktorki3wykres, aes(x=reorder(actor_3_name, -V1), y=V1))+
#   geom_col()+
#   coord_flip()+
#   labs(title = "Zyski z filmów, w których aktorka grała trzecią rolę",
#        x="Aktorka",
#        y="Zyski w USD ")+
#   scale_y_continuous(expand=c(0,0))
# 
# akt1plan <- aktor1planowy
# setnames(akt1plan, c("actor_1_name", "actor_1_earning"))
# akt2plan <- aktor2planowy
# setnames(akt2plan, c("actor_2_name", "actor_2_earning"))
# akt3plan <- aktor3planowy
# setnames(akt3plan, c("actor_3_name", "actor_3_earning"))
# 
# aktorsuma <- akt1plan[akt2plan, on="actor_1_name==actor_2_name"]
# aktorsuma <- aktorsuma[akt3plan, on="actor_1_name==actor_3_name"]
# setnafill(aktorsuma, fill = 0, cols = c("actor_1_earning", "actor_2_earning", "actor_3_earning"))
# aktorrazem <- aktorsuma[,razem:=0.5*actor_1_earning+0.25*actor_2_earning+0.125*actor_3_earning]
# setorder(aktorrazem, -razem)
# 
# aktorsum <-  aktorsuma[,suma:=actor_1_earning+actor_2_earning+actor_3_earning]
# setorder(aktorsum, -suma)
# aktorsumwykres <- aktorsum[1:20,]
# ggplot(aktorsumwykres, aes(x=reorder(actor_1_name, -suma), y=suma))+
#   geom_col()+
#   coord_flip()+
#   labs(title = "Zyski z filmów, w których aktor grał",
#        x="Aktor",
#        y="Zyski w USD ")+
#   scale_y_continuous(expand=c(0,0))
# 
# 
# 
# aktorrazemwykres <- aktorrazem[1:20,]
# ggplot(aktorrazemwykres, aes(x=reorder(actor_1_name, -razem), y=razem))+
#   geom_col()+
#   coord_flip()+
#   labs(title = "Zyski z filmów, w których aktor grał",
#        x="Aktor",
#        y="Zyski w USD ")+
#   scale_y_continuous(expand=c(0,0))
# 
# #BOXPLOTY
# 
# aktorkisum <- aktorsum[!aktorzy, on=("actor_1_name==primaryName"), gender:="female"]
# aktorzysum <- aktorsum[aktorzy, on=("actor_1_name==primaryName"), gender:="male"]
# aktorowiesum <- merge(aktorkisum, aktorzysum)
# 
# ggplot(aktorowiesum, aes(x=gender, y=suma))+
#   geom_boxplot()
#   
# ggplot(aktorowiesum, aes(x=gender, y=suma))+
#   geom_violin()
# 
# ggplot(aktorowiesum, aes(x=suma))+
#   geom_histogram()
# 
# 
# 
# 
# movie_metadata <- cSplit(movie_metadata, "genres", sep="|", direction = "long")
# 
# movie <- movie_metadata[,pomoc:=strsplit(movie_imdb_link, '/')]
# movie <- movie[, tconst:=sapply(pomoc, function(x) x[5])]
# movie <- movie[,.(director_name, actor_1_name, actor_2_name, actor_3_name, tconst, title_year, genres)]
# movie2 <- movie[titles, on="tconst", nomatch=NULL]
# movie3 <- merge(movie2, budgets, by.x=c("primaryTitle", "title_year"), by.y=c("Movie", "title_year"))
# movie3 <- movie3[,difference:=WorldwideGross- ProductionBudget]

# # Wykres zarobków różnych rodzai filmów
# 
# rodzajwykres <- movie3[,median(difference), by=genres]
# rodzajwykres <- setorder(rodzajwykres, -V1)
# ggplot(rodzajwykres, aes(x=reorder(genres, -V1), y=V1))+
#   geom_col()+
#   coord_flip()+
#   labs(title = "Zyski z filmów, w których aktor grał główną rolę",
#        x="Aktor",
#        y="Zyski w USD ")+
#   scale_y_continuous(expand=c(0,0))



