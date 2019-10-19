library(tidyverse)
library(caret)
library(doParallel)

df <- readRDS("harrypotter.rds")
df$character <- NULL
df$TargetHouse <- as.factor(df$TargetHouse)

modelData <- upSample(x = df[,2:285], y = df$TargetHouse)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

BoostedLogisticRegression <- train(Class~., 
                       data = modelData, 
                       method = 'LogitBoost', 
                       trControl = trainControl(method = "repeatedcv", 
                                                repeats = 10, 
                                                number = 10))

stopCluster(cl)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

BoxCoxModel <- train(Class~., 
     data = modelData, 
     method = 'LogitBoost', 
     preProc = c("BoxCox"),
     trControl = trainControl(method = "repeatedcv", 
                              repeats = 10, 
                              number = 10))
YeoJohnsonModel <- train(Class~., 
                         data = modelData, 
                         method = 'LogitBoost', 
                         preProc = c("YeoJohnson"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  repeats = 10, 
                                                  number = 10))

CenterScale <- train(Class~., 
                     data = modelData, 
                     method = 'LogitBoost', 
                     preProc = c("center","scale"),
                     trControl = trainControl(method = "repeatedcv", 
                                              repeats = 10, 
                                              number = 10))

stopCluster(cl)

BoxCoxModel
YeoJohnsonModel
CenterScale

scalingList <- resamples(list("BoxCox" = BoxCoxModel, "YeoJohnson" = YeoJohnsonModel, "CenterScale" = CenterScale))
summary(scalingList)
bwplot(scalingList)
dotplot(scalingList)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

PCAYeoJohnson <- train(Class~., 
                       data = modelData, 
                       method = 'LogitBoost', 
                       preProc = c("YeoJohnson", "pca"),
                       trControl = trainControl(method = "repeatedcv", 
                                                repeats = 10, 
                                                number = 10))
stopCluster(cl)

PCAYeoJohnson
comparison <- resamples(list("YeoJohnson" = YeoJohnsonModel, "PCA" = YeoJohnsonModel))

summary(comparison)
dotplot(comparison)

saveRDS(YeoJohnsonModel, "LogisticRegressionModel.rds")