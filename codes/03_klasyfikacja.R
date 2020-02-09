library(tidyverse)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)

homes <- read.csv("data/part_1_data.csv")

podzial <- sample.split(Y = homes$in_sf, SplitRatio = 0.75)

train <- homes[podzial,]
test <- homes[!podzial,]

drzewo <- rpart(in_sf ~ ., data = train, method = "class")

rpart.plot(drzewo)

printcp(drzewo)

in_sf_test <- predict(drzewo, newdata = test, 
                      type = "class")
in_sf_test

cm <- table(test$in_sf, in_sf_test)
cm

confusionMatrix(cm)

# zbiór uczący

in_sf_train <- predict(drzewo, newdata = train, 
                       type = "class")

table(train$in_sf, in_sf_train)

# obcięcie

optimum <- drzewo$cptable[which.min(drzewo$cptable[,"xerror"]),"CP"]
optimum

drzewo_opt <- prune(drzewo, cp = optimum)

rpart.plot(drzewo_opt)

in_sf_train <- predict(drzewo_opt, newdata = train, 
                       type = "class")

table(train$in_sf, in_sf_train)

in_sf_test <- predict(drzewo_opt, newdata = test, 
                      type = "class")

table(test$in_sf, in_sf_test)
