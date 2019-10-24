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

baseLineSVMLinearModel <- train(Class~., data = trainData, method = "svmLinear", 
                       trControl = trainControl(method = "repeatedcv", 
                                                repeats = 10, 
                                                number = 10, 
                                                allowParallel = T))

baseLineSVMPolyModel <- train(Class~., data = trainData, method = "svmPoly", 
                              trControl = trainControl(method = "repeatedcv", 
                                                       repeats = 10, 
                                                       number = 10, 
                                                       allowParallel = T))

baseLineSVMRadialModel <- train(Class~., data = trainData, method = "svmRadial", 
                                trControl = trainControl(method = "repeatedcv", 
                                                         repeats = 10, 
                                                         number = 10, 
                                                         allowParallel = T))
stopCluster(cl)

baseLineKernelComparison <- resamples(list("Linear" = baseLineSVMLinearModel, "Poly" = baseLineSVMPolyModel, "Radial" = baseLineSVMRadialModel))
summary(baseLineKernelComparison)
bwplot(baseLineKernelComparison)


cl <- makePSOCKcluster(7)
registerDoParallel(cl)

BoxCoxModel <- train(Class~., 
                     data = trainData, 
                     method = "svmLinear", 
                     preProc = c("BoxCox"),
                                trControl = trainControl(method = "repeatedcv", 
                                                         repeats = 10, 
                                                         number = 10, 
                                                         allowParallel = T))
YeoJohnsonModel <- train(Class~., 
                     data = trainData, 
                     method = "svmLinear", 
                     preProc = c("YeoJohnson"),
                     trControl = trainControl(method = "repeatedcv", 
                                              repeats = 10, 
                                              number = 10, 
                                              allowParallel = T))
CenterScale <- train(Class~., 
                     data = trainData, 
                     method = "svmLinear", 
                     preProc = c("center","scale"),
                     trControl = trainControl(method = "repeatedcv", 
                                              repeats = 10, 
                                              number = 10, 
                                              allowParallel = T))
stopCluster(cl)

sampleComparisons <- resamples(list("BoxCox" = BoxCoxModel, "YeoJohnson" = YeoJohnsonModel, "CenterScale" = CenterScale))

summary(sampleComparisons)
bwplot(sampleComparisons)



cl <- makePSOCKcluster(7)
registerDoParallel(cl)

PCACenterScaleModel <- train(Class~., 
                     data = modelData, 
                     method = "svmLinear", 
                     preProc = c("center","scale", "pca"),
                     trControl = trainControl(method = "repeatedcv", 
                                              repeats = 10, 
                                              number = 10, 
                                              allowParallel = T))

CenterScaleModel <- train(Class~., 
                             data = modelData, 
                             method = "svmLinear", 
                             preProc = c("center","scale"),
                             trControl = trainControl(classProbs = TRUE))
stopCluster(cl)


PCACenterScaleModel %>% 
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

CenterScaleModel %>% predict(modelData, type = "prob")

saveRDS(CenterScaleModel, "SupportVectorMachineModel.rds")
