library(tidyverse)
library(tidytext)
library(harrypotter)

philosophers_stone <- philosophers_stone %>% as.data.frame()
chamber_of_secrets <- chamber_of_secrets %>% as.data.frame()
prisoner_of_azkaban <- prisoner_of_azkaban %>% as.data.frame()
goblet_of_fire <- goblet_of_fire %>% as.data.frame()
order_of_the_phoenix <- order_of_the_phoenix %>% as.data.frame()
half_blood_prince <- half_blood_prince %>% as.data.frame()
deathly_hallows <- deathly_hallows %>% as.data.frame()


philosophers_stone$book <- "philosophers_stone"
chamber_of_secrets$book <- "chamber_of_secrets"
prisoner_of_azkaban$book <- "prisoner_of_azkaban"
goblet_of_fire$book <- "goblet_of_fire"
order_of_the_phoenix$book <- "order_of_the_phoenix"
half_blood_prince$book <- "half_blood_prince"
deathly_hallows$book <- "deathly_hallows"

df <- rbind(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban, goblet_of_fire, order_of_the_phoenix, half_blood_prince, deathly_hallows)
colnames(df) <- c("text", "book")

saidWordList <- df %>% 
  unnest_tokens(bigram,`text`, token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  select(word1, word2) %>% 
  filter(word1 == "said" | word2 == "said") %>% 
  gather() %>% 
  select(value) %>% 
  unique() %>% 
  arrange(value) %>% 
  anti_join(stop_words, by = c("value" = "word"))