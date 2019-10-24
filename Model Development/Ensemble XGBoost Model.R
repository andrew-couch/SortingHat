library(tidyverse)
library(caret)
library(doParallel)
library(beepr)

df <- readRDS("harrypotter.rds")
df$character <- NULL
df$TargetHouse <- as.factor(df$TargetHouse)
testData <- df
trainData <- upSample(df %>% select(-TargetHouse), df$TargetHouse)

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

LogisticRegressionModel <- readRDS("LogisticRegressionModel.rds")
NaiveBayesModel <- readRDS("NaiveBayesModel.rds")
L1Model <- readRDS("L1Model.rds")
L2Model <- readRDS("L2Model.rds")
ElasticNetModel <- readRDS("ElasticNetModel.rds")
MARSModel <- readRDS("MARSModel.rds")
KnnModel <- readRDS("KnnModel.rds")
RandomForestModel <- readRDS("RandomForestModel.rds")
SVMModel <- readRDS("SupportVectorMachineModel.rds")

ensembleTrainData <- cbind(predict(LogisticRegressionModel, trainData, type = "prob"),
      predict(NaiveBayesModel, trainData, type = "prob"),
      predict(L1Model, trainData, type = "prob"),
      predict(L2Model,trainData, type = "prob"),
      predict(ElasticNetModel, trainData, type = "prob"),
      predict(MARSModel, trainData, type = "prob"),
      predict(KnnModel, trainData, type = "prob"),
      predict(RandomForestModel, trainData, type = "prob"),
      predict(SVMModel, trainData, type = "prob"),
      trainData$Class)
colnames(ensembleTrainData) <- c("LogisticGryffindor","LogisticHufflepuff","LogisticRavenclaw","LogisticSlytherin",
                                 "NaiveBayesGryffindor","NaiveBayesHufflepuff","NaiveBayesRavenclaw","NaiveBayesSlytherin",
                                 "L1Gryffindor","L1Hufflepuff","L1Ravenclaw","L1Slytherin",
                                 "L2Gryffindor","L2Hufflepuff","L2Ravenclaw","L2Slytherin",
                                 "ElasticNetGryffindor","ElasticNetHufflepuff","ElasticNetRavenclaw","ElasticNetSlytherin",
                                 "MARSGryffindor","MARSHufflepuff","MARSRavenclaw","MARSSlytherin",
                                 "KNNGryffindor","KNNHufflepuff","KNNRavenclaw","KNNSlytherin",
                                 "RandomForestGryffindor","RandomForestHufflepuff","RandomForestRavenclaw","RandomForestSlytherin",
                                 "SVMGryffindor","SVMHufflepuff","SVMRavenclaw","SVMSlytherin",
                                 "Class")
ensembleTrainData$Class <- as.factor(ensembleTrainData$Class)


ensembleTestData <- cbind(predict(LogisticRegressionModel, testData, type = "prob"),
                      predict(NaiveBayesModel, testData, type = "prob"),
                      predict(L1Model, testData, type = "prob"),
                      predict(L2Model,testData, type = "prob"),
                      predict(ElasticNetModel, testData, type = "prob"),
                      predict(MARSModel, testData, type = "prob"),
                      predict(KnnModel, testData, type = "prob"),
                      predict(RandomForestModel, testData, type = "prob"),
                      predict(SVMModel, testData, type = "prob"))



colnames(ensembleTestData) <- c("LogisticGryffindor","LogisticHufflepuff","LogisticRavenclaw","LogisticSlytherin",
                            "NaiveBayesGryffindor","NaiveBayesHufflepuff","NaiveBayesRavenclaw","NaiveBayesSlytherin",
                            "L1Gryffindor","L1Hufflepuff","L1Ravenclaw","L1Slytherin",
                            "L2Gryffindor","L2Hufflepuff","L2Ravenclaw","L2Slytherin",
                            "ElasticNetGryffindor","ElasticNetHufflepuff","ElasticNetRavenclaw","ElasticNetSlytherin",
                            "MARSGryffindor","MARSHufflepuff","MARSRavenclaw","MARSSlytherin",
                            "KNNGryffindor","KNNHufflepuff","KNNRavenclaw","KNNSlytherin",
                            "RandomForestGryffindor","RandomForestHufflepuff","RandomForestRavenclaw","RandomForestSlytherin",
                            "SVMGryffindor","SVMHufflepuff","SVMRavenclaw","SVMSlytherin")



ensembleTrainData
ensembleTestData


cl <- makePSOCKcluster(7)
registerDoParallel(cl)

xgbDARTBase <- train(Class~., 
                     data = ensembleTrainData, 
                     method = "xgbDART")
xgbLinear <- train(Class~., 
                   data = ensembleTrainData, 
                   method = "xgbLinear")
xgbTree <- train(Class~., 
                 data = ensembleTrainData, 
                 method = "xgbTree")

stopCluster(cl)

XgboostModels <- resamples(list("DART" = xgbDARTBase, "Linear" = xgbLinear, "Tree" = xgbTree))
summary(XgboostModels)

testAccuracy(xgbDARTBase)
testAccuracy(xgbLinear)
testAccuracy(xgbTree)

xgbLinear %>% confusionMatrix()
predict(xgbLinear, ensembleTestData) %>% cbind(df$TargetHouse) %>% as.data.frame() %>% rename(.,"predicted" = ., "actual" = V2) %>% mutate(output = if_else(predicted == actual, "correct","wrong")) %>% filter(output == "correct") %>% nrow() / nrow(df)
saveRDS(xgbLinear, "EnsembleModel.rds")