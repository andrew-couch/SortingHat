library(tidyverse)
library(tidytext)
library(sentimentr)

harrypotter <- read.csv("HarryPotter.csv")
harrypotter$Dialogue <- harrypotter$Dialogue %>% as.character()
harrypotter <- harrypotter %>% get_sentences()
harrypotter$Obs <- seq.int(nrow(harrypotter))


harrypotter %>% 
  select(Dialogue) %>% 
  unnest_tokens(word, "Dialogue") %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, pattern = "[[:digit:]]"), 
    !str_detect(word, pattern = "[[:punct:]]"), 
    !str_detect(word, pattern = "(.)\\1{2,}"),  
    !str_detect(word, pattern = "\\b(.)\\b")) %>%
  count(word, sort = TRUE) %>% 
  filter(n > 1) %>% 
  ggplot(mapping = aes(n)) + geom_histogram() + scale_x_log10()

wordList <- harrypotter %>% 
  select(Dialogue) %>% 
  unnest_tokens(word, "Dialogue") %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, pattern = "[[:digit:]]"), 
         !str_detect(word, pattern = "[[:punct:]]"), 
         !str_detect(word, pattern = "(.)\\1{2,}"),  
         !str_detect(word, pattern = "\\b(.)\\b")) %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 1) %>% 
  pull(word)

bagofwordsFeatures <- harrypotter %>%
  unnest_tokens(word, `Dialogue`) %>%
  anti_join(stop_words) %>%
  filter(word %in% wordList) %>%     
  count(Obs, word) %>%                
  spread(word, n) %>%          
  map_df(replace_na, 0)

bigramList <- harrypotter %>%
  unnest_tokens(bigram, `Dialogue`, token = "ngrams", n = 2) %>%  
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
  unite("bigram", c(word1, word2), sep = " ") %>%
  count(bigram) %>%
  filter(n >1) %>%
  pull(bigram)

bigramFeatures <- harrypotter %>%
  unnest_tokens(bigram, `Dialogue`, token = "ngrams", n = 2) %>%
  filter(bigram %in% bigramList) %>%   
  count(Obs, bigram) %>%              
  spread(bigram, n) %>%
  map_df(replace_na, 0)

trigramList <- harrypotter %>%
  unnest_tokens(trigram, `Dialogue`, token = "ngrams", n = 3) %>% 
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
  unite("trigram", c(word1, word2, word3), sep = " ") %>% 
  count(trigram) %>% 
  filter(n > 1) %>% 
  pull(trigram)

trigramFeatures <- harrypotter %>% 
  unnest_tokens(trigram, `Dialogue`, token = "ngrams", n = 3) %>% 
  filter(trigram %in% trigramList) %>% 
  count(Obs, trigram) %>% 
  spread(trigram, n) %>% 
  map_df(replace_na,0)

harrypotter %>% 
  head() %>% 
  get_sentences() %>%
  sentiment(polarity_dt = lexicon::hash_sentiment_jockers) %>% 
  view()




lexicon::hash_nrc_emotions
lexicon::hash_sentiment_huliu
lexicon::hash_sentiment_jockers_rinker
#Finanical Lexicon
lexicon::hash_sentiment_loughran_mcdonald
lexicon::hash_sentiment_nrc
#https://cran.r-project.org/web/packages/lexicon/lexicon.pdf