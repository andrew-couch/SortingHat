library(caret)
library(tidyverse)
library(doParallel)

#Read in data 
harrypotter <- readRDS("harrypotter.rds")
harrypotter$TargetHouse <- as.factor(harrypotter$TargetHouse)
#Remove character feature
harrypotter$character <- NULL
#Creates an upsampled dataset to deal with class imbalances
hpUp <- upSample(harrypotter[,2:1591],harrypotter$TargetHouse)

#Creates an 80/20 split for training and testing 
trainIndex <- createDataPartition(hpUp$Class, p = .8, list = FALSE)
trainData <- hpUp[trainIndex,]
testData <- hpUp[-trainIndex,]

#Enable parallel processing for faster modeling
cl <- makePSOCKcluster(7)
registerDoParallel(cl)


#Test which scaling method to use
BoxCoxModel <- train(Class~., 
                 data = trainData, 
                 method = "rf",
                 preProc = c("BoxCox","pca"),
                 trControl = trainControl(method = "repeatedcv",
                                          repeats = 10, 
                                          number = 10))
YeoJohnsonModel <- train(Class~., 
                         data = trainData, 
                         method = "rf",
                         preProc = c("YeoJohnson","pca"),
                         trControl = trainControl(method = "repeatedcv",
                                                  repeats = 10, 
                                                  number = 10))
CenterScaleModel <- train(Class~., 
                          data = trainData, 
                          method = "rf",
                          preProc = c("center","scale","pca"),
                          trControl = trainControl(method = "repeatedcv",
                                                   repeats = 10, 
                                                   number = 10))

stopCluster(cl)

#View Model results 
BoxCoxModel
YeoJohnsonModel
CenterScaleModel

#Test the models with the test set
BoxCoxTest <- predict(BoxCoxModel, testData) %>% cbind(testData$Class) %>% as.data.frame() %>% mutate(type = "BoxCox")
YeoJohnsonTest <- predict(YeoJohnsonModel, testData) %>% cbind(testData$Class) %>% as.data.frame() %>% mutate(type = "YeoJohnson")
CenterScaleTest <- predict(CenterScaleModel, testData) %>% cbind(testData$Class) %>% as.data.frame() %>% mutate(type = "CenterScale")
testData <- rbind(BoxCoxTest, YeoJohnsonTest, CenterScaleTest)
colnames(testData) <- c("Predicted","Actual","Type")

#View what houses the models are misclassifying 
testData %>% 
  mutate("Result" = if_else(Predicted == Actual, "Correct","Wrong")) %>% 
  group_by(Type, Result, Actual) %>% 
  count(Result) %>% 
  filter(Result == "Wrong") %>% 
  arrange(Type,-n)

#Create model comparison. YeoJohnson on average will have better performance however has higher variance
comparison <- resamples(list("BoxCox" = BoxCoxModel, "YeoJohnson" = YeoJohnsonModel, "CenterScale" = CenterScaleModel))
summary(comparison)
dotplot(comparison)

#View which classes it chooses incorrectly with the original non upsampled dataset 
predict(YeoJohnsonModel, harrypotter) %>% 
  cbind(harrypotter$TargetHouse) %>% 
  as.data.frame() %>% 
  mutate("Result" = if_else(. == V2, "Correct","Wrong")) %>% 
  group_by(Result,V2) %>% 
  count(Result) %>% 
  filter(Result == "Wrong") %>% 
  arrange(-n)

#Compute accuracy of model with non upsampled dataset
predict(YeoJohnsonModel, harrypotter) %>% 
  cbind(harrypotter$TargetHouse) %>% 
  as.data.frame() %>% 
  mutate("Result" = if_else(. == V2, "Correct","Wrong")) %>% 
  filter(Result == "Correct") %>% 
  nrow() / nrow(harrypotter)
#Very good results, will train the entire model off of the entire upsampled dataset

#Create final RandomForest model using YeoJohnson
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
RandomForestModel <- train(Class~., 
                           data = hpUp, 
                           method = "rf",
                           preProc = c("YeoJohnson","pca"),
                           trControl = trainControl(method = "repeatedcv",
                                                    repeats = 10, 
                                                    number = 10))
stopCluster(cl)

RandomForestModel
saveRDS(RandomForestModel, "RandomForestModel.rds")