library(tidyverse)
library(rtweet)
library(tidytext)
library(sentimentr)
library(doParallel)
library(caret)
library(ggthemes)

cl <- makePSOCKcluster(7)
registerDoParallel(cl)
StartTime <- Sys.time()

userName <- "_andrewcouch"

bow <- read.csv("bowlist.csv", header = TRUE,stringsAsFactors = FALSE)
bigram <- read.csv("bigramlist.csv", header = TRUE,stringsAsFactors = FALSE)
trigram <- read.csv("trigramlist.csv", header = TRUE,stringsAsFactors = FALSE)
token <- readRDS("twitter_token.rds")

myTweets <- get_timeline(userName, n = 3200)

tweetData <- myTweets %>% 
  filter(is_retweet == FALSE) %>% 
  select(text) %>% 
  mutate(userName = userName)
tweetData$text <- str_trim(gsub('http\\S+\\s*',"", tweetData$text))
tweetData$text <- gsub("(^|[^@\\w])@(\\w{1,15})\\b", "", tweetData$text)
tweetData <- tweetData %>%
  filter(!text %in% c(" ", "", "   "))

bowFeatures <- tweetData %>% 
  unnest_tokens(word, "text") %>% 
  right_join(bow, by = c("word" = "bow")) %>% 
  count(word, word) %>%
  mutate(n = n-1) %>% 
  spread(word, n)

bigramFeatures <- tweetData %>% 
  unnest_tokens(bigram, "text", token = "ngrams", n = 2) %>% 
  right_join(bigram, by = c("bigram" = "bigram")) %>% 
  count(bigram, bigram) %>%
  mutate(n = n-1) %>% 
  spread(bigram, n)

trigramFeatures <- tweetData %>% 
  unnest_tokens(trigram, "text", token = "ngrams", n = 3) %>% 
  right_join(trigram, by = c("trigram" = "trigram")) %>% 
  count(trigram, trigram) %>%
  mutate(n = n-1) %>% 
  spread(trigram, n)

sentences <- tweetData %>% select(text) %>% get_sentences()

sentiments <- cbind(
  sentences %>% 
    sentiment((lexicon::hash_sentiment_huliu)) %>% 
    select(sentiment) %>% 
    rename("huliu" = sentiment),
  sentences %>% 
    sentiment(lexicon::hash_sentiment_jockers_rinker) %>% 
    select(sentiment) %>% 
    rename("jockers_rinker" = sentiment),
  sentences %>% 
    sentiment(lexicon::hash_sentiment_nrc) %>% 
    select(sentiment) %>% 
    rename("nrc" = sentiment),
  sentences %>% 
    sentiment(lexicon::hash_sentiment_senticnet) %>% 
    select(sentiment) %>% 
    rename("senticnet" = sentiment),
  sentences %>% 
    sentiment(lexicon::hash_sentiment_sentiword) %>% 
    select(sentiment) %>% 
    rename("sentiword" = sentiment),
  sentences %>% 
    sentiment(lexicon::hash_sentiment_slangsd) %>% 
    select(sentiment) %>% 
    rename("slangsd" = sentiment),
  sentences %>% 
    sentiment(lexicon::hash_sentiment_socal_google) %>% 
    select(sentiment) %>% 
    rename("socal_google" = sentiment)) 

sentiments <-  sentiments %>% 
  gather(key = "sentiment", value = "score") %>% 
  group_by(sentiment) %>% 
  summarise(score = mean(score)) %>% 
  spread(key = sentiment, value = score)

emotions <- lexicon::nrc_emotions

emotionFeatures <- sentences %>% 
  unnest_tokens(word, "text") %>% 
  filter(word %in% emotions$term) %>% 
  left_join(emotions, by = c("word" = "term")) %>% 
  select(-word,-element_id, -sentence_id) %>% 
  summarise_each(funs(sum)) %>% 
  gather(key = "sentiment", value = "score") %>% 
  mutate(score = score / sentences %>% unnest_tokens(word, "text") %>% nrow()) %>% 
  spread(sentiment, score) %>% 
  rename("anger.emotion" = anger,
        "anticipation.emotion" = anticipation,
        "digust.emotion" = disgust,
        "fear.emotion" = fear,
        "joy.emotion" = joy,
        "sadness.emotion" = sadness,
        "surprise.emotion" = surprise,
        "trust.emotion" = trust)

df <- cbind(bowFeatures, bigramFeatures, trigramFeatures, sentiments, emotionFeatures)

LogisticRegressionModel <- readRDS("LogisticRegressionModel.rds")
NaiveBayesModel <- readRDS("NaiveBayesModel.rds")
L1Model <- readRDS("L1Model.rds")
L2Model <- readRDS("L2Model.rds")
ElasticNetModel <- readRDS("ElasticNetModel.rds")
MARSModel <- readRDS("MARSModel.rds")
KnnModel <- readRDS("KnnModel.rds")
RandomForestModel <- readRDS("RandomForestModel.rds")
SVMModel <- readRDS("SupportVectorMachineModel.rds")
EnsembleModel <- readRDS("EnsembleModel.rds")


ensembleData <- cbind(predict(LogisticRegressionModel, df),
                          predict(NaiveBayesModel, df),
                          predict(L1Model, df),
                          predict(L2Model,df),
                          predict(ElasticNetModel, df),
                          predict(MARSModel, df),
                          predict(KnnModel, df),
                          predict(RandomForestModel, df),
                          predict(SVMModel,df)) %>% 
  as.data.frame()

colnames(ensembleData) <- c("Logistic","NaiveBayes","L1","L2","ElasticNet","MARS","Knn","RandomForest","SVM")

HousePrediction <- predict(EnsembleModel, ensembleData, type = "prob")
colnames(HousePrediction) <- c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")

stopCluster(cl)
EndTime <- Sys.time()
StartTime - EndTime

HousePrediction %>% 
  gather(key = "House", value = "Percentage") %>% 
  ggplot(aes(x = House, y = Percentage, color = House, fill = House)) + geom_col() + 
  scale_y_continuous(labels = scales::percent) + ggtitle(paste(userName, "'s House Assignment", sep = "")) + theme(plot.title = element_text(hjust = .5)) + theme_economist()

HousePrediction