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

baseLineModel <- train(Class~., data = trainData, method = "naive_bayes")

stopCluster(cl)


cl <- makePSOCKcluster(7)
registerDoParallel(cl)

BoxCoxModel <- train(Class~., 
      data = trainData, 
      method = "naive_bayes", 
      preProc = c("BoxCox"),
      trControl = trainControl(method = "repeatedcv", 
                               repeats = 10, 
                               number = 10))
YeoJohnsonModel <- train(Class~., 
      data = trainData, 
      method = "naive_bayes", 
      preProc = c("YeoJohnson"),
      trControl = trainControl(method = "repeatedcv", 
                               repeats = 10, 
                               number = 10))
CenterScaleModel <- train(Class~., 
      data = trainData, 
      method = "naive_bayes", 
      preProc = c("center","scale"),
      trControl = trainControl(method = "repeatedcv", 
                               repeats = 10, 
                               number = 10))

stopCluster(cl)

centeringList <- resamples(list("BoxCox" = BoxCoxModel, "YeoJohnson" = YeoJohnsonModel, "CenterScale" = CenterScaleModel))
summary(centeringList)
dotPlot(centeringList)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

BoxCoxNzv <- train(Class~., 
                   data = trainData, 
                   method = "naive_bayes", 
                   preProc = c("BoxCox", "nzv"),
                   trControl = trainControl(method = "repeatedcv", 
                                            repeats = 10, 
                                            number = 10))
BoxCoxZv <- train(Class~., 
      data = trainData, 
      method = "naive_bayes", 
      preProc = c("BoxCox", "zv"),
      trControl = trainControl(method = "repeatedcv", 
                               repeats = 10, 
                               number = 10))
stopCluster(cl)

BoxCoxNzv
BoxCoxZv

preProcessModels <- resamples(list("NZV" = BoxCoxNzv, "ZV" = BoxCoxZv))
summary(preProcessModels)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

NaiveBayesModel <- train(Class~., 
                         data = modelData, 
                         method = "naive_bayes", 
                         preProc = c("BoxCox", "nzv"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  repeats = 10, 
                                                  number = 10))
stopCluster(cl)

testing <- predict(NaiveBayesModel, df) %>% cbind(df$TargetHouse) 
colnames(testing) <- c("predictions","actual")

testing <- as.data.frame(testing)
testing %>% 
  mutate(correct = if_else(predictions == actual, "Correct","Wrong")) %>% 
  group_by(correct, actual) %>% 
  filter(correct == "Correct") %>% 
  nrow() / nrow(df)

testing1 <- BoxCoxModel %>% predict(df) %>% cbind(df$TargetHouse) %>% as.data.frame()
colnames(testing1) <- c("Predicted","Actual")
testing1 %>% mutate(correct = if_else(Predicted == Actual, "Correct","Wrong")) %>% 
  filter(correct == "Correct") %>% 
  nrow() / nrow(df)


saveRDS(NaiveBayesModel, "NaiveBayesModel.rds")