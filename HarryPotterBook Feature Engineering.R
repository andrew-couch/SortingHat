library(tidyverse)
library(tidytext)
library(harrypotter)

df <- philosophers_stone %>% as.data.frame()
colnames(df) <- "text"

df %>% 
  unnest_tokens(bigram,`text`, token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  select(word1, word2) %>% 
  filter(word1 == "said" | word2 == "said") %>% 
  gather() %>% 
  select(value) %>% 
  unique() %>% 
  arrange(value)