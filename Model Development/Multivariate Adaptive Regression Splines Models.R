library(tidyverse)
library(caret)
library(doParallel)
require("earth")

df <- readRDS("harrypotter.rds")
df <- df %>% select(-character)
df$TargetHouse <- as.factor(df$TargetHouse)

modelData <- upSample(x = df %>% select(-TargetHouse), y = df$TargetHouse)
trainIndex <- createDataPartition(modelData$Class, p = .8, list = FALSE)
trainData <- modelData[trainIndex,]
testData <- modelData[-trainIndex,]


baseLineModel <- train(Class~., data = trainData, method = "earth", 
                       trControl = trainControl(method = "repeatedcv", 
                                                repeats = 10, 
                                                number = 10, 
                                                allowParallel = T))
cl <- makePSOCKcluster(7)
registerDoParallel(cl)


BoxCoxModel <- train(Class~., data = trainData, method = "earth",
                     preProc = c("BoxCox"),
                     trControl = trainControl(method = "repeatedcv", 
                                              repeats = 10, 
                                              number = 10, 
                                              allowParallel = T))

YeoJohnsonModel <- train(Class~., data = trainData, method = "earth", 
                         preProc = c("YeoJohnson"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  repeats = 10, 
                                                  number = 10, 
                                                  allowParallel = T))

CenterScaleModel <- train(Class~., data = trainData, method = "earth", 
                          preProc = c("center","scale"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   repeats = 10, 
                                                   number = 10, 
                                                   allowParallel = T))

stopCluster(cl)


SamplingComparison <- resamples(list("BoxCox" = BoxCoxModel, "YeoJohnson" = YeoJohnsonModel, "CenterScale" = CenterScaleModel, "BaseLine" = baseLineModel))
summary(SamplingComparison)
bwplot(SamplingComparison)
dotplot(SamplingComparison)
splom(SamplingComparison)

YeoJohnsonModel %>% 
  predict(df) %>% 
  cbind(df$TargetHouse) %>% 
  as.data.frame() %>% 
  rename(., "Predicted" = ., "Actual" = V2) %>% 
  mutate(Correct = if_else(Predicted == Actual, "Correct", "Wrong")) %>% 
  filter(Correct == "Correct") %>% 
  nrow() / nrow(df)

BoxCoxModel %>% 
  predict(df) %>% 
  cbind(df$TargetHouse) %>% 
  as.data.frame() %>% 
  rename(., "Predicted" = ., "Actual" = V2) %>% 
  mutate(Correct = if_else(Predicted == Actual, "Correct", "Wrong")) %>% 
  filter(Correct == "Correct") %>% 
  nrow() / nrow(df)

CenterScaleModel %>% 
  predict(df) %>% 
  cbind(df$TargetHouse) %>% 
  as.data.frame() %>% 
  rename(., "Predicted" = ., "Actual" = V2) %>% 
  mutate(Correct = if_else(Predicted == Actual, "Correct", "Wrong")) %>% 
  filter(Correct == "Correct") %>% 
  nrow() / nrow(df)


summary(SamplingComparison)


cl <- makePSOCKcluster(7)
registerDoParallel(cl)

PCAYeoJohnsonModel <- train(Class~., data = modelData, method = "earth", 
                         preProc = c("YeoJohnson", "pca"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  repeats = 10, 
                                                  number = 10, 
                                                  allowParallel = T))

FullYeoJohnsonModel <- train(Class~., data = modelData, method = "earth", 
                            preProc = c("YeoJohnson"),
                            trControl = trainControl(method = "repeatedcv", 
                                                     repeats = 10, 
                                                     number = 10, 
                                                     allowParallel = T))
stopCluster(cl)

modelTuning <- resamples(list("PCA" = PCAYeoJohnsonModel, "YeoJohnson" = FullYeoJohnsonModel))
summary(modelTuning)

PCAYeoJohnsonModel
FullYeoJohnsonModel

PCAYeoJohnsonModel %>% 
  predict(df) %>% 
  cbind(df$TargetHouse) %>% 
  as.data.frame() %>% 
  rename(., "Predicted" = ., "Actual" = V2) %>% 
  mutate(Correct = if_else(Predicted == Actual, "Correct", "Wrong")) %>% 
  filter(Correct == "Correct") %>% 
  nrow() / nrow(df)

FullYeoJohnsonModel %>% 
  predict(df) %>% 
  cbind(df$TargetHouse) %>% 
  as.data.frame() %>% 
  rename(., "Predicted" = ., "Actual" = V2) %>% 
  mutate(Correct = if_else(Predicted == Actual, "Correct", "Wrong")) %>% 
  filter(Correct == "Correct") %>% 
  nrow() / nrow(df)

saveRDS(FullYeoJohnsonModel, "MARSModel.rds")