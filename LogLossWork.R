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

train(Class~., 
      data = modelData, 
      metric = "logLoss", 
      preProc = c("BoxCox","center","scale", "YeoJohnson","pca"),
      trControl = trainControl(method = "repeatedcv",
                               number = 10,
                               repeats = 10,
                               returnData = FALSE,
                               returnResamp = 'none',
                               savePredictions = "final",
                               classProbs = TRUE, 
                               summaryFunction = get("multiClassSummary"),
                               allowParallel = T))

stopCluster(cl)