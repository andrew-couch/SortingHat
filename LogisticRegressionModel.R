library(tidyverse)
library(caret)

df <- readRDS("harrypotter.rds")
df$character <- NULL
df$TargetHouse <- as.factor(df$TargetHouse)

upSample <- upSample(df %>% select(-TargetHouse), df$TargetHouse)

trainIndex <- createDataPartition(upSample$Class, p = 2/3, list = FALSE)

trainData <- upSample[trainIndex,]
testData <- upSample[-trainIndex,]

baseLineModel <- train(Class~., data = trainData, method = 'multinom')


CenterScaleModel <- train(CLass., 
                          data = upSample, 
                          method = "multinom", 
                          preProc = c("center","scale"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   repeats = 10, 
                                                   number = 10))
BoxCoxModel <- train(CLass., 
                          data = upSample, 
                          method = "multinom", 
                          preProc = c("BoxCox"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   repeats = 10, 
                                                   number = 10))
YeoJohnsonModel <- train(CLass., 
                          data = upSample, 
                          method = "multinom", 
                          preProc = c("YeoJohnson"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   repeats = 10, 
                                                   number = 10))

