library(tidyverse)
library(readxl)

pensje <- read_xlsx("salary.xlsx")

model1 <- lm(Salary ~ YearsExperience, pensje)
model1
summary(model1)

doswiadczenie <- data.frame(YearsExperience=c(1.5, 5, 13))

predict(model1, doswiadczenie)

# pracownicy --------------------------------------------------------------

pracownicy <- read_xlsx("pracownicy.xlsx")

summary(pracownicy)

pracownicy2 <- pracownicy %>%
  filter(!is.na(wiek)) %>%
  select(-id, -data_urodz) %>%
  mutate(plec=as.factor(plec),
         kat_pracownika=as.factor(kat_pracownika),
         zwiazki=as.factor(zwiazki))
summary(pracownicy2)

model2 <- lm(bwynagrodzenie ~ ., pracownicy2)
summary(model2)

library(corrplot)

korelacje <- pracownicy2 %>%
  select(-plec, -kat_pracownika, -zwiazki) %>%
  cor()

corrplot(korelacje, method = "number", 
         type = "upper")

