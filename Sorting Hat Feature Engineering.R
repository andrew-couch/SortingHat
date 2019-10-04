library(tidyverse)
library(tidytext)
library(sentimentr)

df <- read.csv("harrypotter.csv")
df$text <- as.character(df$text)

nameList <- read.csv("namelist.csv")

#This section creates ngram features 
#The features will be grouped by character 
#Characters are different for each medium there's a movie_harry and book_harry
#May group characters by medium and book which would be Chamber_Of_Secrets_Movie_Harry

#Creates word list for bag of words feature 
wordList <- df %>% 
  select(text) %>% 
  unnest_tokens(word, "text") %>% 
  filter(!word %in% nameList$value) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, pattern = "[[:digit:]]"), 
         !str_detect(word, pattern = "[[:punct:]]"), 
         !str_detect(word, pattern = "(.)\\1{2,}"),  
         !str_detect(word, pattern = "\\b(.)\\b")) %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 10) %>% 
  pull(word)

#generates bag of words features
bowFeatures <- df %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  filter(word %in%  wordList) %>% 
  count(character, word) %>% 
  spread(word, n) %>% 
  map_df(replace_na, 0)

#creates bigram list for bigram features
bigramList <- df %>% 
  select(text) %>% 
  unnest_tokens(bigram, "text", token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%               
  filter(!word1 %in% stop_words$word,                 
         !word2 %in% stop_words$word,
         !str_detect(word1, pattern = "[[:digit:]]"), 
         !str_detect(word2, pattern = "[[:digit:]]"),
         !str_detect(word1, pattern = "[[:punct:]]"), 
         !str_detect(word2, pattern = "[[:punct:]]"),
         !str_detect(word1, pattern = "(.)\\1{2,}"),  
         !str_detect(word2, pattern = "(.)\\1{2,}"),
         !str_detect(word1, pattern = "\\b(.)\\b"),  
         !str_detect(word1, pattern = "\\b(.)\\b")) %>%
  filter(!word1 %in% nameList$value,
         !word2 %in% nameList$value) %>% 
  unite("bigram", c(word1, word2), sep = " ") %>%
  count(bigram) %>%
  filter(n > 2) %>%
  pull(bigram)

#creates bigram features
bigramFeatures <- df %>% 
  unnest_tokens(bigram, "text", token = "ngrams", n = 2) %>%  
  filter(bigram %in% bigramList) %>% 
  count(character, bigram) %>% 
  spread(bigram, n) %>% 
  map_df(replace_na, 0)

#creates trigram list for trigram features
trigramList <- df %>% 
  unnest_tokens(trigram, "text", token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1","word2","word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !str_detect(word1, pattern = "[[:digit:]]"),
         !str_detect(word1, pattern = "[[:punct:]]"),
         !str_detect(word1, pattern = "(.)\\1{2,}"),
         !str_detect(word1, pattern = "\\b(.)\\b"),
         !word2 %in% stop_words$word,
         !str_detect(word2, pattern = "[[:digit:]]"),
         !str_detect(word2, pattern = "[[:punct:]]"),
         !str_detect(word2, pattern = "(.)\\1{2,}"),
         !str_detect(word2, pattern = "\\b(.)\\b"),
         !word3 %in% stop_words$word,
         !str_detect(word3, pattern = "[[:digit:]]"),
         !str_detect(word3, pattern = "[[:punct:]]"),
         !str_detect(word3, pattern = "(.)\\1{2,}"),
         !str_detect(word3, pattern = "\\b(.)\\b")) %>% 
  filter(!word1 %in% nameList$value,
         !word2 %in% nameList$value,
         !word3 %in% nameList$value) %>% 
  unite("trigram", c(word1, word2, word3), sep = " ") %>% 
  count(trigram) %>% 
  filter(n > 2) %>% 
  pull(trigram)

#creates trigram features 
trigramFeatures <- df %>% 
  unnest_tokens(trigram, "text", token = "ngrams", n = 3) %>% 
  filter(trigram %in% trigramList) %>% 
  count(character, trigram) %>% 
  spread(trigram, n) %>% 
  map_df(replace_na,0)


#This section creates lexicon sentiment analysis features 
#data will need to be processed on a sentence level using sentimentR 

df %>% 
  get_sentences() %>% 
  unnest_tokens(word, "text") %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments(lexicon = "bing")) %>% 
  group_by(book,character, element_id,sentence_id, sentiment) %>% 
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0)






#This section will add an tf-idf feature (term frequency and inverse document frequency)
#Where document will be each movie or book 
#Weights words and may help separating the houses
#filters out top 100 tf_idf words for each house, takes more than 100 because of ties 

tf_idfWOrdList <- df %>% 
  filter(house != "No Entry") %>% 
  get_sentences() %>% 
  unnest_tokens(word, "text") %>% 
  filter(!word %in% nameList$value,
         !str_detect(word, pattern = "[[:digit:]]"),
         !str_detect(word, pattern = "[[:punct:]]"),
         !str_detect(word, pattern = "(.)\\1{2,}"),
         !str_detect(word, pattern = "\\b(.)\\b"),) %>%
  anti_join(stop_words) %>% 
  count(house, word, sort = TRUE) %>% 
  bind_tf_idf(word, house, n) %>% 
  group_by(house) %>% 
  top_n(tf_idf, n = 100) %>% 
  pull(word) %>% 
  unique() 

