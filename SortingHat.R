library(tidyverse)
library(tidytext)
library(corpus)

df <- read.csv("HarryPotter.csv")
df$Dialogue <- as.character(df$Dialogue)
df$Review.Text <- df$Review.Text %>% as.character()

#Tokenizes and counts frequency words and plots 
df %>% 
  select(., 'Dialogue') %>% 
  unnest_tokens(word, 'Dialogue') %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  ggplot(., mapping = aes(n)) + geom_histogram() + scale_x_log10()

#Cleaning data 
df %>% 
  select(., 'Dialogue') %>% 
  unnest_tokens(word, 'Dialogue') %>% 
  anti_join(stop_words) %>% 
  filter(
    !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word, pattern = "\\b(.)\\b")    # removes any remaining single letter words
  ) %>%
  mutate(word = text_tokens(word, stemmer = "en") %>% unlist()) %>% # add stemming process
  count(word) %>% 
  filter(n >1)

#Create word list
word_list <- df %>% 
  select(., 'Dialogue') %>% 
  unnest_tokens(word, 'Dialogue') %>% 
  anti_join(stop_words) %>% 
  filter(
    !str_detect(word, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word, pattern = "\\b(.)\\b")    # removes any remaining single letter words
  ) %>%
  mutate(word = text_tokens(word, stemmer = "en") %>% unlist()) %>% # add stemming process
  count(word) %>% 
  filter(n >1) %>%
  pull(word)
  

# create new features
bowFeatures <- df %>%
  unnest_tokens(word, `Dialogue`) %>%
  anti_join(stop_words) %>%
  filter(word %in% word_list) %>%     # filter for only words in the wordlist
  count(Character, word) %>%                 # count word useage by customer ID
  spread(word, n) %>%                 # convert to wide format
  map_df(replace_na, 0)               # replace NAs with 0

df_bow <- df %>%
  inner_join(bowFeatures, by = "Character") %>%   # join data sets
  select(-`Dialogue`)                    # remove original review text

#N grams of 2
df %>%
  unnest_tokens(bigram, `Dialogue`, token = "ngrams", n = 2) %>%
  head()

# create a vector of all bi-grams to keep 
ngram_list <- df %>%
  unnest_tokens(bigram, `Dialogue`, token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>%               
  filter(
    !word1 %in% stop_words$word,                 # remove stopwords from both words in bi-gram
    !word2 %in% stop_words$word,
    !str_detect(word1, pattern = "[[:digit:]]"), # removes any words with numeric digits
    !str_detect(word2, pattern = "[[:digit:]]"),
    !str_detect(word1, pattern = "[[:punct:]]"), # removes any remaining punctuations
    !str_detect(word2, pattern = "[[:punct:]]"),
    !str_detect(word1, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
    !str_detect(word2, pattern = "(.)\\1{2,}"),
    !str_detect(word1, pattern = "\\b(.)\\b"),   # removes any remaining single letter words
    !str_detect(word1, pattern = "\\b(.)\\b")
  ) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  count(bigram) %>%
  filter(n >1) %>% # filter for bi-grams used 10 or more times
  pull(bigram)

# create new bi-gram features
ngram_features <- df %>%
  unnest_tokens(bigram, `Dialogue`, token = "ngrams", n = 2) %>%
  filter(bigram %in% ngram_list) %>%    # filter for only bi-grams in the ngram_list
  count(Character, bigram) %>%                 # count bi-gram useage by customer ID
  spread(bigram, n) %>%                 # convert to wide format
  map_df(replace_na, 0)                 # replace NAs with 0


cleanedDf <- inner_join(bowFeatures, ngram_features, by = "Character")
cleanedDf <- inner_join(df %>% select(House, Character) %>% unique(), cleanedDf, by = "Character")
