library(caret)
library(tidyverse)
library(doParallel)
library(ROSE)

harrypotter <- readRDS("harrypotter.rds")
harrypotter$TargetHouse <- as.factor(harrypotter$TargetHouse)
harrypotter$character <- NULL

hpUp <- upSample(harrypotter[,2:1591],harrypotter$TargetHouse)
hpDown <- downSample(harrypotter[,2:1591],harrypotter$TargetHouse)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

upModel <- train(Class~., 
                 data = hpUp, 
                 method = "rf",
                 preProc = c("BoxCox","pca"),
                 trControl = trainControl(method = "repeatedcv",
                                          repeats = 10, 
                                          number = 10))

downModel <- train(Class~., 
                   data = hpDown, 
                   method = "rf",
                   preProc = c("BoxCox","pca"),
                   trControl = trainControl(method = "repeatedcv",
                                            repeats = 10, 
                                            number = 10))

regularModel <- train(TargetHouse~., 
                      data = harrypotter, 
                      method = "rf", 
                      preProc = c("BoxCox"), 
                      trControl = trainControl(method = "repeatedcv", 
                                               repeats = 10, 
                                               number = 10))
stopCluster(cl)