library(tidyverse)
library(readxl)

wybory <- read_xlsx(path = "wybory2018.xlsx")

count(wybory, szczebel)

n_szczebel <- wybory %>%
  count(szczebel) %>%
  arrange(n)

write.table(x = n_szczebel, file = "szczebel.csv", 
            sep = ";", row.names = FALSE)

sejmiki <- wybory %>%
  filter(szczebel == "Sejmik wojewódzki") %>%
  mutate(wyborcy_mieszk=wyborcy/mieszkancy,
         kandydat_mieszk=liczba_kandydatow/mieszkancy*10000)

hist(sejmiki$liczba_mandatow, 
     ylim = c(0,50))

hist(sejmiki$liczba_kandydatow)

hist(sejmiki$kandydat_mieszk)
summary(sejmiki$kandydat_mieszk)
