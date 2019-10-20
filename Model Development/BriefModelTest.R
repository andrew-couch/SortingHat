library(tidyverse)
library(caret)

df <- iris
set.seed(40)

MultinomialLogReg <- train(Species~., method = "multinom", data = df)

NaiveBayesClassifier <- train(Species~., method = "naive_bayes", data = df)

RegularizedL1Regression <- train(Species~., method = "glmnet", data = df,
                                 tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-3,3, length = 100)))

RegularizedL2Regression <- train(Species~., method = "glmnet", data = df,
                                 tuneGrid = expand.grid(alpha = 0, lambda = 10^seq(-3,3, length = 100)))

ElasticNet <- train(Species~., method = "glmnet", data = df,
                    tuneLength = 50)

SVM <- train(Species~., method = "svmLinear", data = df)

XGBoost <- train(Species~., method = "xgbLinear", data = df)

MARS <- train(Species~., method = "fda", data = df)


comparisons <- resamples(list("LogisticReg" = MultinomialLogReg, "NaiveBayes" = NaiveBayesClassifier, "L1" = RegularizedL1Regression, "L2" = RegularizedL2Regression, "ElasticNets" = ElasticNet, "SVM" = SVM, "XGBoost" = XGBoost, "MARS" = MARS))

summary(comparisons)
bwplot(comparisons)

varImp(RegularizedL1Regression)
varImp(RegularizedL2Regression)
varImp(ElasticNet)

RegularizationComparison <- resamples(list("L1" = RegularizedL1Regression,
                                           "L2" = RegularizedL2Regression,
                                           "Elastic" = ElasticNet))
summary(RegularizationComparison)
bwplot(RegularizationComparison)
dotplot(RegularizationComparison)
varImp(RegularizedL1Regression)
varImp(RegularizedL2Regression)
