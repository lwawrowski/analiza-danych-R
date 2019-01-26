library(tidyverse)
library(readxl)

rossmann <- read_xlsx("rossmann.xlsx") 

sklep77 <- rossmann %>%
  filter(sklep_id == 77 & czy_otwarty == "Tak")

# korelacja cech jakościowych
sklep77 <- sklep77 %>%
  mutate(sprzedaz_srednia=ifelse(sprzedaz > mean(sprzedaz),
                                 "Powyżej średniej",
                                 "Poniżej średniej"))

# tablica kontyngencji (krzyżowa)
tablica <- table(sklep77$czy_promocja, sklep77$sprzedaz_srednia)
tablica

# pomiędzy promocją a sprzedażą występuje zależność
chisq.test(tablica)

chi2 <- chisq.test(tablica)$statistic
chi2

# V-Cramer
# umiarkowana zależność
sqrt(chi2/nrow(sklep77)*min(2-1,2-1))

# korelacja cech ilościowych
# wykres rozrzutu
plot(sklep77$sprzedaz, sklep77$liczba_klientow)

# pomiędzy sprzedeżą a liczbą klientów występuje
# silna korelacja dodatnia
cor(sklep77$sprzedaz, sklep77$liczba_klientow) 














