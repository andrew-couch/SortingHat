library(caret)
library(tidyverse)
library(GGally)

harrypotter <- readRDS("harrypotter.rds")

upModel <- readRDS("upModel.rds")

PCAExtra <- predict(preProcess(harrypotter, method = c("BoxCox","corr", "pca")), harrypotter)

CorrTrans <- predict(preProcess(harrypotter, method = c("corr")), harrypotter) 

CorrPCA <- predict(preProcess(harrypotter, method = c("corr", "pca")), harrypotter)

spatial <- predict(preProcess(harrypotter, method = c("spatialSign")), harrypotter)

BigCombo <- predict(preProcess(harrypotter, method = c("center","scale","YeoJohnson","nzv")))

CorrNonZeroVariance <- predict(preProcess(harrypotter, method = c("center","scale","YeoJohnson", "corr", "nzv")), harrypotter)