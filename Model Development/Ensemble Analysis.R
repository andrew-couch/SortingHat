library(tidyverse)

df <- readRDS("harrypotter.rds")

LogisticRegressionModel <- readRDS("LogisticRegressionModel.rds")
NaiveBayesModel <- readRDS("NaiveBayesModel.rds")
L1Model <- readRDS("L1Model.rds")
L2Model <- readRDS("L2Model.rds")
ElasticNetModel <- readRDS("ElasticNetModel.rds")
MARSModel <- readRDS("MARSModel.rds")
KnnModel <- readRDS("KnnModel.rds")
RandomForestModel <- readRDS("RandomForestModel.rds")
SVMModel <- readRDS("SupportVectorMachineModel.rds")

startTime <- Sys.time()

results <- cbind(predict(LogisticRegressionModel, df),
      predict(NaiveBayesModel, df),
      predict(L1Model, df),
      predict(L2Model,df),
      predict(ElasticNetModel, df),
      predict(MARSModel, df),
      predict(KnnModel, df),
      predict(RandomForestModel, df),
      predict(SVMModel, df), df$TargetHouse) %>% 
  as.data.frame()

colnames(results) <- c("Logistic","NaiveBayes","L1","L2","ElasticNet","MARS","Knn","RandomForest","SVM", "Actual")

endTime <- Sys.time()
startTime-endTime

results <- results %>% select(-Logistic) 
results$Actual <- results$Actual %>% as.integer()


results <- results %>% mutate(n = row_number())
Actual <- results %>% select(Actual, n)
results <- results %>% select(-Actual)
Actual$Actual <- Actual$Actual %>% as.integer()
Actual$n <- Actual$n %>% as.integer()


results <- results %>% gather(key = "Model", value = "value", -n) %>% 
  arrange(Model, n)
results$value <- as.integer(results$value)

results %>% 
  left_join(actual, by = c("n" = "n")) %>% 
  select(-n) %>% 
  mutate(correct = if_else(value == Actual, "Correct", "Wrong")) %>% 
  group_by(Model, correct, Actual, value) %>% 
  count(correct) %>% 
  filter(correct == "Wrong") %>% 
  arrange(-n) %>% 
  ungroup() %>% 
  group_by(Model) %>% 
  ggplot(aes(x = value, y = Actual,group = Model)) + 
  geom_jitter(width = 0, height = .1) + 
  facet_wrap(~Model, scales = "free") + ggthemes::theme_economist()


results %>% 
  left_join(actual, by = c("n" = "n")) %>% 
  select(-n) %>% 
  mutate(correct = if_else(value == Actual, "Correct", "Wrong")) %>% 
  group_by(Model, correct, Actual, value) %>% 
  filter(correct == "Wrong") %>% 
  ungroup() %>% 
  group_by(Model) %>% 
  ggplot(aes(x = value, y = Actual,group = Model)) + 
  geom_jitter(width = 0, height = .1) + 
  geom_smooth(se = FALSE) + 
  labs(x = "Predicted", y = "Actual") + 
  facet_wrap(~Model, scales = "free") + ggthemes::theme_economist()

results %>% group_by(n) %>% 
  count(n, value) %>% 
  filter(value == max(value)) %>% 
  select(n,value) %>% 
  inner_join(Actual, by = c("n" = "n")) %>% 
  mutate(correct = if_else(value == Actual, "correct","wrong")) %>% 
  ungroup() %>% 
  select(-n) %>% 
  filter(correct == "wrong") %>% 
  ggplot(aes(x = value , y = Actual)) + 
  geom_jitter(width = 0, height = .1, alpha = .5)