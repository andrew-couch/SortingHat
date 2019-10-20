library(tidyverse)
library(rtweet)
library(tidytext)
library(sentimentr)

userName <- "_AndrewCouch"

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
