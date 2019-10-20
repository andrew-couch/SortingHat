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

testAccuracy <- function (model) {
  model %>% 
    predict(df) %>% 
    cbind(df$TargetHouse) %>% 
    as.data.frame() %>% 
    rename(., "Predicted" = ., "Actual" = V2) %>% 
    mutate(Correct = if_else(Actual == Predicted, "Correct", "Wrong")) %>% 
    filter(Correct == "Correct") %>%
    nrow() / nrow(df)
}


cl <- makePSOCKcluster(7)
registerDoParallel(cl)

xgbDARTBase <- train(Class~., 
                       data = trainData, 
                       method = "xgbDART", 
                       trControl = trainControl(method = "repeatedcv", 
                                                repeats = 10, 
                                                number = 10, 
                                                allowParallel = T),
                     tuneLength = 10)
xgbLinear <- train(Class~., 
                     data = trainData, 
                     method = "xgbLinear", 
                     trControl = trainControl(method = "repeatedcv", 
                                              repeats = 10, 
                                              number = 10, 
                                              allowParallel = T),
                   tuneLength = 10)
xgbTree <- train(Class~., 
                     data = trainData, 
                     method = "xgbTree", 
                     trControl = trainControl(method = "repeatedcv", 
                                              repeats = 10, 
                                              number = 10, 
                                              allowParallel = T),
                 tuneLength = 10)

stopCluster(cl)

XgboostModels <- resamples(list("DART" = xgbDARTBase, "Linear" = xgbLinear, "Tree" = xgbTree))
summary(XgboostModels)

testAccuracy(xgbDARTBase)
testAccuracy(xgbLinear)
testAccuracy(xgbTree)