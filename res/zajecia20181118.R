library(tidyverse)
library(readxl)
library(e1071)

# wczytanie zbioru danych
rossmann <- read_xlsx("rossmann.xlsx")

# liczba sklepów wed³ug typu asortymentu
n_sklep_asort <- count(rossmann, sklep_asort)

# liczba sklepów wed³ug typu asortymentu - równowa¿ny zapis
count(rossmann, sklep_asort)

# liczba sklepów wed³ug typu asortymentu - równowa¿ny zapis
rossmann %>%
  count(sklep_asort)

# analiza struktury

# wybór dwóch sklepów ze zbioru do analizy
rossmann2 <- rossmann %>%
  filter(sklep_id == 842 | sklep_id == 15) %>%
  filter(czy_otwarty == "Tak")

# równowa¿ny zapis
rossmann2 <- rossmann %>%
  filter(sklep_id == 842 | sklep_id == 15,
         czy_otwarty == "Tak")

# równowa¿ny zapis
rossmann2 <- filter(filter(rossmann, sklep_id == 842 | sklep_id == 15),czy_otwarty == "Tak") 

# przeprowadzenie analizy struktury - miary klasyczne
rossmann2 %>%
  group_by(sklep_id) %>%
  summarise(liczebnosc = n(),
            srednia = mean(sprzedaz),
            odchylenie = sd(sprzedaz),
            wspolczynnik_zm = odchylenie/srednia*100,
            skosnosc = skewness(sprzedaz),
            kurtoza = kurtosis(sprzedaz))

# przeprowadzenie analizy struktury - miary pozycyjne
rossmann2 %>%
  group_by(sklep_id) %>%
  summarise(mediana = median(sprzedaz),
            kwartyl1 = quantile(sprzedaz, 0.25),
            kwartyl3 = quantile(sprzedaz, 0.75))

# wykres pude³kowy
boxplot(sprzedaz ~ sklep_id, data = rossmann2, 
        horizontal = TRUE)

# test porównuj¹cy dwie œrednie
t.test(sprzedaz ~ sklep_id, data = rossmann2)

# zadanie - analiza struktury wed³ug typu sklepu
rossmann3 <- rossmann %>%
  filter(czy_otwarty == "Tak")

rossmann3 %>%
  group_by(sklep_typ) %>%
  summarise(liczebnosc = n(),
            srednia = mean(liczba_klientow),
            odchylenie = sd(liczba_klientow),
            wspolczynnik_zm = odchylenie/srednia*100,
            skosnosc = skewness(liczba_klientow),
            kurtoza = kurtosis(liczba_klientow))

boxplot(liczba_klientow ~ sklep_typ, data = rossmann3, 
        horizontal = TRUE)

# test porównuj¹cy wiêcej ni¿ dwie œrednie
anova <- aov(liczba_klientow ~ sklep_typ, data = rossmann3)
summary(anova)

# test parami
TukeyHSD(anova)
