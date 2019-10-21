library(tidyverse)
library(caret)
library(doParallel)
library(beepr)

df <- readRDS("harrypotter.rds")
df <- df %>% select(-character)
df$TargetHouse <- as.factor(df$TargetHouse)

modelData <- upSample(x = df %>% select(-TargetHouse), y = df$TargetHouse)

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


cl <- makePSOCKcluster(7)
registerDoParallel(cl)

baseLineModel <- train(Class~., 
                       data = modelData, 
                       method = "bayesglm")
beep(3)
stopCluster(cl)

#saveRDS(baseLineModel, "LogisticRegressionModel.rds")

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

boxCoxModel <- train(Class~., 
                     data = trainData,
                     method = "gpls", 
                     preProc = c("BoxCox"))
YeoJohnsonModel <- train(Class~., 
                         data = trainData,
                         method = "gpls", 
                         preProc = c("YeoJohnson"))

CenterScaleModel <- train(Class~., 
                          data = trainData,
                          method = "gpls", 
                          preProc = c("center","scale"))
stopCluster(cl)

samplingComparison <- resamples(list("BoxCox" = boxCoxModel, "YeoJohnson" = YeoJohnsonModel, "CenterScale" = CenterScaleModel, "BaseLine" = baseLineModel))
summary(samplingComparison)

testAccuracy(baseLineModel)
testAccuracy(boxCoxModel)
testAccuracy(YeoJohnsonModel)
testAccuracy(CenterScaleModel)