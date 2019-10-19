library(tidyverse)
library(caret)
library(doParallel)

df <- readRDS("harrypotter.rds")
df <- df %>% select(-character)
df$TargetHouse <- as.factor(df$TargetHouse)

modelData <- upSample(x = df %>% select(-TargetHouse), y = df$TargetHouse)
trainIndex <- createDataPartition(modelData$Class, p = .8, list = FALSE)
trainData <- modelData[trainIndex,]
testData <- modelData[-trainIndex,]

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

baseLineModel <- train(Class~., data = trainData, method = "earth", 
                       trControl = trainControl(method = "repeatedcv", 
                                                repeats = 10, 
                                                number = 10, 
                                                allowParallel = T))
stopCluster(cl)