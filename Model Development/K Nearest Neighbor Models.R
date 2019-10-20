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

baseLineModel <- train(Class~., data = trainData, method = "knn", 
                       trControl = trainControl(method = "repeatedcv", 
                                                repeats = 10, 
                                                number = 10, 
                                                allowParallel = T))
stopCluster(cl)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

boxCoxModel <- train(Class~., 
                     data = trainData,
                     method = "knn", 
                     preProc = c("BoxCox"),
                     trControl = trainControl(method = "repeatedcv", 
                                              repeats = 10, 
                                              number = 10, 
                                              allowParallel = T))
YeoJohnsonModel <- train(Class~., 
                     data = trainData,
                     method = "knn", 
                     preProc = c("YeoJohnson"),
                     trControl = trainControl(method = "repeatedcv", 
                                              repeats = 10, 
                                              number = 10, 
                                              allowParallel = T))

CenterScaleModel <- train(Class~., 
                         data = trainData,
                         method = "knn", 
                         preProc = c("center","scale"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  repeats = 10, 
                                                  number = 10, 
                                                  allowParallel = T))

stopCluster(cl)


samplingComparison <- resamples(list("BoxCox" = boxCoxModel, "YeoJohnson" = YeoJohnsonModel, "CenterScale" = CenterScaleModel, "BaseLine" = baseLineModel))
summary(samplingComparison)

baseLineModel %>% 
  predict(df) %>% 
  cbind(df$TargetHouse) %>% 
  as.data.frame() %>% 
  rename(., "Predicted" = ., "Actual" = V2) %>% 
  mutate(Correct = if_else(Actual == Predicted, "Correct", "Wrong")) %>% 
  filter(Correct == "Correct") %>%
  nrow() / nrow(df)

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


testAccuracy(baseLineModel)
testAccuracy(boxCoxModel)
testAccuracy(YeoJohnsonModel)
testAccuracy(CenterScaleModel)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

PCAboxCoxModel <- train(Class~., 
                     data = modelData,
                     method = "knn", 
                     preProc = c("BoxCox", "pca"),
                     trControl = trainControl(method = "repeatedcv", 
                                              repeats = 10, 
                                              number = 10, 
                                              allowParallel = T))

FullBoxCoxModel <- train(Class~., 
                        data = modelData,
                        method = "knn", 
                        preProc = c("BoxCox"),
                        trControl = trainControl(method = "repeatedcv", 
                                                 repeats = 10, 
                                                 number = 10, 
                                                 allowParallel = T))
stopCluster(cl)

finalModelComparison <- resamples(list("PCA" = PCAboxCoxModel, "BoxCox" = FullBoxCoxModel))
summary(finalModelComparison)

testAccuracy(PCAboxCoxModel)
testAccuracy(FullBoxCoxModel)

saveRDS(PCAboxCoxModel, "KnnModel.rds")