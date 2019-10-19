library(tidyverse)
library(doParallel)
library(caret)

df <- readRDS("harrypotter.rds")
df <- df %>% select(-character)
df$TargetHouse <- as.factor(df$TargetHouse)
modelData <- upSample(df %>% select(-TargetHouse), df$TargetHouse)


cl <- makePSOCKcluster(7)
registerDoParallel(cl)

L1BaseLineModel <- train(Class~., 
                         data = modelData, 
                         method = "glmnet", 
                         tuneGrid = expand.grid(alpha = 1, 
                                                lambda = 10^seq(-3,3, length = 100)))
L2BaseLineModel <- train(Class~., 
                         data = modelData, 
                         method = "glmnet", 
                         tuneGrid = expand.grid(alpha = 0, 
                                                lambda = 10^seq(-3,3, length = 100)))
ElasticNetModel <- train(Class~., 
                         data = modelData, 
                         method = "glmnet", 
                         tuneLength = 20)
stopCluster(cl)

RegularizationBaseModelSamples <- resamples(list("L1" = L1BaseLineModel, "L2" = L2BaseLineModel, "Elastic Net" = ElasticNetModel))
plot(RegularizationBaseModelSamples)
summary(RegularizationBaseModelSamples)
bwplot(RegularizationBaseModelSamples)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

BoxCoxL1BaseLineModel <- train(Class~., 
                         data = modelData, 
                         method = "glmnet", 
                         preProc = c("BoxCox"),
                         tuneGrid = expand.grid(alpha = 1, 
                                                lambda = 10^seq(-3,3, length = 100)))
YeoJohnsonL1BaseLineModel <- train(Class~., 
                         data = modelData, 
                         method = "glmnet",
                         preProc = c("YeoJohnson"),
                         tuneGrid = expand.grid(alpha = 1, 
                                                lambda = 10^seq(-3,3, length = 100)))
CenterScaleL1BaseLineModel <- train(Class~., 
                         data = modelData, 
                         method = "glmnet", 
                         preProc = c("center","scale"),
                         tuneGrid = expand.grid(alpha = 1, 
                                                lambda = 10^seq(-3,3, length = 100)))
BoxCoxL2BaseLineModel <- train(Class~., 
                               data = modelData, 
                               method = "glmnet", 
                               preProc = c("BoxCox"),
                               tuneGrid = expand.grid(alpha = 0, 
                                                      lambda = 10^seq(-3,3, length = 100)))
YeoJohnsonL2BaseLineModel <- train(Class~., 
                                   data = modelData, 
                                   method = "glmnet",
                                   preProc = c("YeoJohnson"),
                                   tuneGrid = expand.grid(alpha = 0, 
                                                          lambda = 10^seq(-3,3, length = 100)))
CenterScaleL2BaseLineModel <- train(Class~., 
                                    data = modelData, 
                                    method = "glmnet", 
                                    preProc = c("center","scale"),
                                    tuneGrid = expand.grid(alpha = 0, 
                                                           lambda = 10^seq(-3,3, length = 100)))
BoxCoxElasticNetModel <- train(Class~., 
                         data = modelData, 
                         method = "glmnet",
                         preProc = c("BoxCox"),
                         tuneLength = 20)

YeoJohnsonElasticNetModel <- train(Class~., 
                               data = modelData, 
                               method = "glmnet",
                               preProc = c("YeoJohnson"),
                               tuneLength = 20)

CenterScaleElasticNetModel <- train(Class~., 
                               data = modelData, 
                               method = "glmnet",
                               preProc = c("center","scale"),
                               tuneLength = 20)

stopCluster(cl)

preProcessL1 <- resamples(list("BoxCox" = BoxCoxL1BaseLineModel, "YeoJohnson" = YeoJohnsonL1BaseLineModel, "CenterScale" = CenterScaleL1BaseLineModel))

preProcessL2 <- resamples(list("BoxCox" = BoxCoxL2BaseLineModel, "YeoJohnson" = YeoJohnsonL2BaseLineModel, "CenterScale" = CenterScaleL2BaseLineModel))

preProceessElasticNet <- resamples(list("BoxCox" = BoxCoxElasticNetModel, "YeoJohnson" = YeoJohnsonElasticNetModel, "CenterScale" = CenterScaleElasticNetModel))

summary(preProcessL1)
bwplot(preProcessL1)

summary(preProcessL2)
bwplot(preProcessL2)

summary(preProceessElasticNet)
bwplot(preProceessElasticNet)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)

PCACenterScaleL1BaseLineModel <- train(Class~., 
                                    data = modelData, 
                                    method = "glmnet", 
                                    preProc = c("center","scale", "pca"),
                                    tuneGrid = expand.grid(alpha = 1, 
                                                           lambda = 10^seq(-3,3, length = 100)),
                                    trControl = trainControl(method = "repeatedcv", 
                                                             repeats = 10, 
                                                             number = 10))
PCACenterScaleL2BaseLineModel <- train(Class~., 
                                    data = modelData, 
                                    method = "glmnet", 
                                    preProc = c("center","scale", "pca"),
                                    tuneGrid = expand.grid(alpha = 0, 
                                                           lambda = 10^seq(-3,3, length = 100)),
                                    trControl = trainControl(method = "repeatedcv", 
                                                             repeats = 10, 
                                                             number = 10))
PCABoxCoxElasticNetModel <- train(Class~., 
                               data = modelData, 
                               method = "glmnet",
                               preProc = c("BoxCox", "pca"),
                               tuneLength = 20,
                               trControl = trainControl(method = "repeatedcv", 
                                                        repeats = 10, 
                                                        number = 10))
BaseLineCenterScaleL1BaseLineModel <- train(Class~., 
                                       data = modelData, 
                                       method = "glmnet", 
                                       preProc = c("center","scale", "pca"),
                                       tuneGrid = expand.grid(alpha = 1, 
                                                              lambda = 10^seq(-3,3, length = 100)),
                                       trControl = trainControl(method = "repeatedcv", 
                                                                repeats = 10, 
                                                                number = 10))
BaseLineCenterScaleL2BaseLineModel <- train(Class~., 
                                       data = modelData, 
                                       method = "glmnet", 
                                       preProc = c("center","scale", "pca"),
                                       tuneGrid = expand.grid(alpha = 0, 
                                                              lambda = 10^seq(-3,3, length = 100)),
                                       trControl = trainControl(method = "repeatedcv", 
                                                                repeats = 10, 
                                                                number = 10))
BaseLineBoxCoxElasticNetModel <- train(Class~., 
                                  data = modelData, 
                                  method = "glmnet",
                                  preProc = c("BoxCox", "pca"),
                                  tuneLength = 20,
                                  trControl = trainControl(method = "repeatedcv", 
                                                           repeats = 10, 
                                                           number = 10))

stopCluster(cl)

FinalL1Comp <- resamples(list("PCA" = PCACenterScaleL1BaseLineModel, "L1" = BaseLineCenterScaleL1BaseLineModel))
FinalL2Comp <- resamples(list("PCA" = PCACenterScaleL2BaseLineModel, "L2" = BaseLineCenterScaleL2BaseLineModel))
FinalElasticComp <- resamples(list("PCA" = PCABoxCoxElasticNetModel, "Elastic" = BaseLineBoxCoxElasticNetModel))

