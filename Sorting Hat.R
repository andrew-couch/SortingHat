#install.packages(tidyverse)
#install.packages(rtweet)
#install.packages(tidytext)
#install.packages(sentimentr)
#install.packages(doParallel)
#install.packages(caret)
#install.packages(ggthemes)
library(tidyverse)
library(rtweet)
library(tidytext)
library(sentimentr)
library(doParallel)
library(ggrepel)
library(ggthemes)
rm(list = ls())
get_token()


#Twitter username for sorting hat 
userName <- "senwarren"

#Reads in Document Term Matrix
bow <- read.csv("bowlist.csv", header = TRUE,stringsAsFactors = FALSE)
bigram <- read.csv("bigramlist.csv", header = TRUE,stringsAsFactors = FALSE)
trigram <- read.csv("trigramlist.csv", header = TRUE,stringsAsFactors = FALSE)

#Retrieves tweets from username 
myTweets <- get_timeline(userName, n = 3200)

#Enables parallel computing for faster compile times
cl <- makePSOCKcluster(7)
registerDoParallel(cl)
StartTime <- Sys.time()


#Cleans data by removing @Names, links, and Retweets
tweetData <- myTweets %>% 
  filter(is_retweet == FALSE) %>% 
  select(text) %>% 
  mutate(userName = userName)
tweetData$text <- str_trim(gsub('http\\S+\\s*',"", tweetData$text))
tweetData$text <- gsub("(^|[^@\\w])@(\\w{1,15})\\b", "", tweetData$text)
tweetData <- tweetData %>%
  filter(!text %in% c(" ", "", "   "))


#Generate BOW, Bigram,and Trigram Features
bowFeatures <- tweetData %>% 
  unnest_tokens(word, "text") %>% 
  filter(word != "tweet") %>% 
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


#Generate positive negative sentiment features
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

#Generates emotion sentiment features such as anger, anticipation, disgust, fear, joy, sadness, surprise, and trust
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

#Combines BOW, Bigram, Trigram, Sentiment, and Emotion features
df <- cbind(bowFeatures, bigramFeatures, trigramFeatures, sentiments, emotionFeatures)

#Reads in models 
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


#Generates data for esemble model
ensembleData <- cbind(predict(LogisticRegressionModel, df, type = "prob"),
                          predict(NaiveBayesModel, df, type = "prob"),
                          predict(L1Model, df, type = "prob"),
                          predict(L2Model,df, type = "prob"),
                          predict(ElasticNetModel, df, type = "prob"),
                          predict(MARSModel, df, type = "prob"),
                          predict(KnnModel, df, type = "prob"),
                          predict(RandomForestModel, df, type = "prob"),
                          predict(SVMModel, df, type = "prob"))
colnames(ensembleData) <- c("LogisticGryffindor","LogisticHufflepuff","LogisticRavenclaw","LogisticSlytherin",
                                "NaiveBayesGryffindor","NaiveBayesHufflepuff","NaiveBayesRavenclaw","NaiveBayesSlytherin",
                                "L1Gryffindor","L1Hufflepuff","L1Ravenclaw","L1Slytherin",
                                "L2Gryffindor","L2Hufflepuff","L2Ravenclaw","L2Slytherin",
                                "ElasticNetGryffindor","ElasticNetHufflepuff","ElasticNetRavenclaw","ElasticNetSlytherin",
                                "MARSGryffindor","MARSHufflepuff","MARSRavenclaw","MARSSlytherin",
                                "KNNGryffindor","KNNHufflepuff","KNNRavenclaw","KNNSlytherin",
                                "RandomForestGryffindor","RandomForestHufflepuff","RandomForestRavenclaw","RandomForestSlytherin",
                                "SVMGryffindor","SVMHufflepuff","SVMRavenclaw","SVMSlytherin")

#Uses ensemble model to make final prediction
HousePrediction <- predict(EnsembleModel, ensembleData, type = "prob")
colnames(HousePrediction) <- c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")

#Stops parallel computing
stopCluster(cl)
EndTime <- Sys.time()
StartTime - EndTime

#Plots predictions as probabilities/percentage 
HousePrediction %>% 
  gather(key = "House", value = "Percentage") %>% 
  ggplot(aes(x = House, 
             y = Percentage, 
             color = House, 
             fill = House)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::percent) + 
  ggtitle(paste(userName, "'s House Assignment", sep = "")) + 
  theme(plot.title = element_text(hjust = .5)) + 
  theme_economist()

HousePrediction %>% 
  mutate("Name" = userName) %>%
  write.table("SortingHatList.csv",
              row.names = FALSE, 
              col.names = FALSE, 
              sep = ",",
              append = TRUE)

sortedList <- read.csv("SortingHatList.csv")


exampleNames <- sortedList %>% 
  gather(key = "house", value = "value", -Name) %>% 
  group_by(Name) %>% 
  top_n(value, n =1) %>% 
  select(Name, house) %>% 
  inner_join(sortedList) %>% 
  mutate(x = Hufflepuff - Ravenclaw, 
         y = Gryffindor - Slytherin) %>%
  select(Name, x , y, house) %>% 
  filter(Name %in% c("Book_Harry", "Book_Malfoy" ,userName))

sortedList %>% 
  gather(key = "house", value = "value", -Name) %>% 
  group_by(Name) %>% 
  top_n(value, n =1) %>% 
  select(Name, house) %>% 
  inner_join(sortedList) %>% 
  mutate(x = Hufflepuff - Ravenclaw, 
         y = Gryffindor - Slytherin) %>%
  select(Name, x , y, house) %>% 
  ggplot(aes(x = x, y = y, label = Name)) + 
  geom_vline(xintercept = 0, linetype = "dashed", alpha = .5) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = .5) +
  geom_point(aes(color = house)) + 
  geom_text_repel(data = exampleNames, 
                  mapping = aes(x = x, y = y, label = Name),
                  nudge_x = .1) +
  theme_fivethirtyeight() +
  theme(aspect.ratio = 1, axis.title = element_text()) +
  ylab(expression("More likely Slytherin  " %<->% " More likely Gryffindor")) +
  xlab(expression("More likely Ravenclaw  " %<->% " More likely Hufflepuff"))