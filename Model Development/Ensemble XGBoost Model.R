library(tidyverse)
library(caret)
library(doParallel)
library(beepr)

df <- readRDS("harrypotter.rds")
df$character <- NULL
df$TargetHouse <- as.factor(df$TargetHouse)
testData <- df
trainData <- upSample(df %>% select(-TargetHouse), df$TargetHouse)

LogisticRegressionModel <- readRDS("LogisticRegressionModel.rds")
NaiveBayesModel <- readRDS("NaiveBayesModel.rds")
L1Model <- readRDS("L1Model.rds")
L2Model <- readRDS("L2Model.rds")
ElasticNetModel <- readRDS("ElasticNetModel.rds")
MARSModel <- readRDS("MARSModel.rds")
KnnModel <- readRDS("KnnModel.rds")
RandomForestModel <- readRDS("RandomForestModel.rds")
SVMModel <- readRDS("SupportVectorMachineModel.rds")

ensembleTrain <- cbind(predict(LogisticRegressionModel, trainData),
                 predict(NaiveBayesModel, trainData),
                 predict(L1Model, trainData),
                 predict(L2Model,trainData),
                 predict(ElasticNetModel, trainData),
                 predict(MARSModel, trainData),
                 predict(KnnModel, trainData),
                 predict(RandomForestModel, trainData),
                 predict(SVMModel, trainData), trainData$Class) %>% 
  as.data.frame()
colnames(ensembleTrain) <- c("Logistic","NaiveBayes","L1","L2","ElasticNet","MARS","Knn","RandomForest","SVM", "Actual")


ensembleTest <- cbind(predict(LogisticRegressionModel, testData),
                       predict(NaiveBayesModel, testData),
                       predict(L1Model, testData),
                       predict(L2Model,testData),
                       predict(ElasticNetModel, testData),
                       predict(MARSModel, testData),
                       predict(KnnModel, testData),
                       predict(RandomForestModel, testData),
                       predict(SVMModel, testData), testData$TargetHouse) %>% 
  as.data.frame()
colnames(ensembleTest) <- c("Logistic","NaiveBayes","L1","L2","ElasticNet","MARS","Knn","RandomForest","SVM", "Actual")
ensembleTrain$Actual <- as.factor(ensembleTrain$Actual)

ensembleModel <- train(Actual~., data = ensembleTrain, method = "xgbLinear")
beep(3)


predict(ensembleModel, ensembleTest) %>% 
  cbind(ensembleTest$Actual) %>% 
  as.data.frame() %>% 
  rename(., "predicted" = ., "actual" = V2) %>% 
  mutate(results = if_else(predicted == actual, "correct","wrong")) %>% 
  filter(results == "correct") %>% 
  nrow() / nrow(ensembleTest)


saveRDS(ensembleModel, "EnsembleModel.rds")