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
                       method = "lda")
boxCoxModel <- train(Class~., 
                     data = modelData,
                     method = "lda", 
                     preProc = c("BoxCox"))
YeoJohnsonModel <- train(Class~., 
                         data = modelData,
                         method = "lda", 
                         preProc = c("YeoJohnson"))

CenterScaleModel <- train(Class~., 
                          data = modelData,
                          method = "lda", 
                          preProc = c("center","scale"))
beep(3)
stopCluster(cl)




samplingComparison <- resamples(list("BoxCox" = boxCoxModel, "YeoJohnson" = YeoJohnsonModel, "CenterScale" = CenterScaleModel))
summary(samplingComparison)

testAccuracy(baseLineModel)
testAccuracy(boxCoxModel)
testAccuracy(YeoJohnsonModel)
testAccuracy(CenterScaleModel)


saveRDS(YeoJohnsonModel, "LogisticRegressionModel.rds")