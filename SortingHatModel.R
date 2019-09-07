library(tidyverse)
library(caret)

df <- readRDS("HarryPotter.rds")
df <- df %>% select(-Character)

trainIndex <- createDataPartition(df$House, p = .8, list = FALSE)
trainData <- df[trainIndex,]
testData <- df[-trainIndex,]

rfModel <- train(House~., 
                 data = trainData, 
                 method = "rf",
                 trControl = trainControl(method = "repeatedcv", number = 10 , repeats = 10,
                                          sampling = "up"))

neuralNetworkModel <- train(House~., 
                 data = trainData, 
                 method = "nnet",
                 trControl = trainControl(method = "repeatedcv", number = 10 , repeats = 10,
                                          sampling = "up"))

neuralNetworkModelPCA <-train(House~., 
                           data = trainData,
                           method = "pcaNNet", 
                           trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10,
                                                    sampling = "up"))

neuralNetworkMultiPerceptron <- train(House~., 
                                    data = trainData,
                                    method = "mlpWeightDecay", 
                                    trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10,
                                                             sampling = "up"))


xgboostModel <- train(House~., 
                       data = trainData,
                       method = "xgbTree", 
                       trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10,
                                                sampling = "up"))

comparison <- resamples(list(baselineNeuralNet = neuralNetworkModel, 
                        PCANeuranNet = neuralNetworkModelPCA, 
                        perceptronNeutralNetwork = neuralNetworkMultiPerceptron, 
                        randomForest = rfModel, 
                        XGBoost = xgboostModel))
bwplot(comparison)
dotplot(comparison)
summary(comparison)