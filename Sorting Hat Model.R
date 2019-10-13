library(caret)
library(tidyverse)

harrypotter <- readRDS("harrypotter.rds")
harrypotter$TargetHouse <- as.factor(harrypotter$TargetHouse)
harrypotter$character <- NULL


smoteTrain <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = "multiClassSummary",
                           metric = "logLoss",
                           sampling = "smote")



colSums(harrypotter %>% is.na())


harrypotter$acromantula

test <- train(TargetHouse~., data = harrypotter, method = "glm")
