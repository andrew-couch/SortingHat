library(tidyverse)
library(tidytext)
library(sentimentr)

df <- read.csv("harrypotter.csv", stringsAsFactors = FALSE)
df <- df %>% filter(house != "No Entry")
nameList <- read.csv("namelist.csv")


#writes bag of words list 
bow <- df %>% 
  filter(house != "No Entry") %>% 
  get_sentences() %>% 
  unnest_tokens(word, "text") %>% 
  filter(!word %in% nameList$value,
         !str_detect(word, pattern = "[[:digit:]]"),
         !str_detect(word, pattern = "[[:punct:]]"),
         !str_detect(word, pattern = "(.)\\1{2,}"),
         !str_detect(word, pattern = "\\b(.)\\b")) %>%
  count(house, word, sort = TRUE) %>% 
  bind_tf_idf(word, house, n) %>% 
  group_by(house) %>% 
  top_n(tf_idf, n = 100) %>% 
  pull(word) %>% 
  unique() %>% 
  sort()

#generates bag of word features
bowFeatures <- df %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  filter(word %in%  bow) %>% 
  count(character, word) %>% 
  spread(word, n) %>% 
  map_df(replace_na, 0)

#writes bigram list
bigrams <- df %>% 
  unnest_tokens(bigram, "text", token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% nameList$value,
         !word2 %in% nameList$value) %>% 
  unite("bigram", c(word1, word2), sep = " ") %>% 
  count(house, bigram, sort = TRUE) %>% 
  bind_tf_idf(bigram, house ,n) %>% 
  group_by(house) %>% 
  top_n(tf_idf, n = 100) %>% 
  pull(bigram) %>% 
  unique() %>% 
  sort()

#generates bigram features
bigramFeatures <- df %>% 
  unnest_tokens(bigram, "text", token = "ngrams", n = 2) %>% 
  filter(bigram %in% bigrams) %>% 
  count(character, bigram) %>% 
  spread(bigram, n) %>% 
  map_df(replace_na, 0)


#writes trigram list
trigrams <- df %>% 
  unnest_tokens(trigram, "text", token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1","word2","word3"), sep = " ") %>% 
  filter(!word1 %in% nameList$value) %>% 
  filter(!word2 %in% nameList$value) %>% 
  filter(!word3 %in% nameList$value) %>% 
  unite("trigram", c(word1, word2, word3), sep = " ") %>% 
  count(house, trigram) %>% 
  bind_tf_idf(house, trigram, n) %>% 
  group_by(house) %>% 
  top_n(n, n = 25) %>% 
  pull(trigram) %>% 
  unique() %>% 
  sort()

#generates trigram features
trigramFeatures <- df %>% 
  unnest_tokens(trigram, "text", token = "ngrams", n = 3) %>% 
  filter(trigram %in% trigrams) %>% 
  count(character, trigram) %>% 
  spread(trigram, n) %>% 
  map_df(replace_na, 0)


#sentiment lexicon engineering 
sentiments <- df %>% 
  get_sentences() %>% 
  select(character, text) %>% 
  cbind(huliu <- sentences %>% 
          sentiment(lexicon::hash_sentiment_huliu) %>% 
          select(sentiment) %>% 
          rename("huliu" = sentiment),jockers_rinker <- sentences %>% 
          sentiment(lexicon::hash_sentiment_jockers_rinker) %>% 
          select(sentiment) %>% 
          rename("jockers_rinker" = sentiment),
                    nrc <- sentences %>% 
          (lexicon::hash_sentiment_nrc) %>% 
          select(sentiment) %>% 
          rename("nrc" = sentiment),
                    senticnet <- sentences %>% 
          sentiment(lexicon::hash_sentiment_senticnet) %>% 
          select(sentiment) %>% 
          rename("senticnet" = sentiment),
                    sentiword <- sentences %>% 
          sentiment(lexicon::hash_sentiment_sentiword) %>% 
          select(sentiment) %>% 
          rename("sentiword" = sentiment),
                    slangsd <- sentences %>% 
          sentiment(lexicon::hash_sentiment_slangsd) %>% 
          select(sentiment) %>% 
          rename("slangsd" = sentiment),
                    socal_google <- sentences %>% 
          sentiment(lexicon::hash_sentiment_socal_google) %>% 
          select(sentiment) %>% 
          rename("socal_google" = sentiment))


#Finds average sentiment for each character
sentiments <-  sentiments %>%
  select(-text) %>% 
  gather(key = "sentiment", value = "score", -character) %>% 
  group_by(character, sentiment) %>% 
  summarise(score = mean(score)) %>% #Finds mean sentiment by sentences 
  spread(key = sentiment, value = score)



#Generates emotion features 
emotions <- lexicon::nrc_emotions
emotionFeatures <- df %>% 
  get_sentences() %>% 
  unnest_tokens(word, `text`) %>% 
  filter(word %in% emotions$term) %>% 
  left_join(emotions, by = c("word" = "term")) %>% 
  select(-word) %>% 
  select(character, anger, anticipation, disgust, fear, joy, sadness, surprise, trust) %>% 
  group_by(character) %>% 
  summarise_each(funs(sum)) %>% 
  left_join(df %>% 
              unnest_tokens(word, "text") %>% 
              group_by(character) %>% 
              count(character, character), 
            by = c("character" = "character")) %>% 
  gather(key = "sentiment", value = "score", -character, -n) %>% 
  mutate(score = score/n) %>% #Averages the sentiment by word count
  select(-n) %>% 
  spread(sentiment, score) %>% 
  rename("anger.emotion" = anger,
         "anticipation.emotion" = anticipation,
         "digust.emotion" = disgust,
         "fear.emotion" = fear,
         "joy.emotion" = joy,
         "sadness.emotion" = sadness,
         "surprise.emotion" = surprise,
         "trust.emotion" = trust)


#Creates final df for modeling 
harrypotter <- df %>% 
  select(house,character) %>% 
  rename("TargetHouse" = house) %>% 
  unique() %>% 
  arrange() %>% 
  left_join(bowFeatures) %>% 
  left_join(bigramFeatures) %>% 
  left_join(trigramFeatures) %>% 
  left_join(sentiments) %>% 
  left_join(emotionFeatures) %>% 
  map_df(replace_na, 0)

#saveRDS(harrypotter, "harrypotter.rds")