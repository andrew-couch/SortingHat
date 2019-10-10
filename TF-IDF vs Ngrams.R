library(tidyverse)
library(tidytext)
library(sentimentr)

df <- read.csv("harrypotter.csv", stringsAsFactors = FALSE)
df <- df %>% filter(house != "No Entry")
names <- read.csv("namelist.csv")

#BOW Tf-IDF
BOW_tfidf <-  df %>% 
  get_sentences() %>% 
  unnest_tokens(word, "text") %>% 
  filter(!word %in% names$value) %>% 
  filter(!word %in% stop_words$word) %>% 
  count(house, word, sort = TRUE) %>% 
  bind_tf_idf(word, house, n) %>% 
  group_by(house) %>% 
  top_n(tf_idf, n = 10)


BOW <- df %>% 
  get_sentences() %>% 
  unnest_tokens(word, "text") %>% 
  filter(!word %in% nameList$value) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, pattern = "[[:digit:]]"), 
         !str_detect(word, pattern = "[[:punct:]]"), 
         !str_detect(word, pattern = "(.)\\1{2,}"),  
         !str_detect(word, pattern = "\\b(.)\\b")) %>% 
  count(house, word, sort = TRUE) %>% 
  group_by(house) %>% 
  top_n(n, n = 10)



BOW_tfidf %>% 
  select(house, word, tf_idf) %>%
  rename("house" = house, "word" = word, "value" = tf_idf) %>% 
  mutate(type = "BOW tf-idf") %>% 
  rbind(BOW %>% 
          rename("house" = house, "word" = word, "value" = n) %>% 
          mutate(type = "BOW")) %>% 
  group_by(type, house) %>% 
  arrange(type, house) %>% 
  ggplot(mapping = aes(x = reorder_within(word, value, house), 
                       y = value, 
                       color = house, 
                       fill = house)) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~house + type, scales = "free") + 
  coord_flip()

df %>% 
  get_sentences() %>% 
  unnest_tokens(word, "text") %>% 
  filter(!word %in% names$value) %>% 
  filter(!word %in% stop_words$word) %>% 
  count(house, word, sort = TRUE) %>% 
  bind_tf_idf(word, house, n) %>% 
  group_by(house) %>% 
  top_n(tf_idf, n = 100) %>% 
  ggplot(mapping = aes(x = reorder_within(word, tf_idf, house), 
                       y = tf_idf, 
                       color = house, 
                       fill = house)) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~house, scales = "free", ncol = 1) + 
  coord_flip()


df %>% 
  get_sentences() %>% 
  unnest_tokens(word, "text") %>% 
  filter(!word %in% nameList$value) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, pattern = "[[:digit:]]"), 
         !str_detect(word, pattern = "[[:punct:]]"), 
         !str_detect(word, pattern = "(.)\\1{2,}"),  
         !str_detect(word, pattern = "\\b(.)\\b")) %>% 
  count(house, word, sort = TRUE) %>% 
  group_by(house) %>% 
  top_n(n, n = 100) %>% 
  ggplot(mapping = aes(x = reorder_within(word, n, house), y = n, color = house, fill = house)) + 
  geom_col() + 
  scale_x_reordered() +
  facet_wrap(~house, scales = "free", ncol = 1) + 
  coord_flip()



#Bigrams Tf-IDF
Bigram_tfidf <- df %>% 
  get_sentences() %>% 
  unnest_tokens(bigram, "text", token = "ngrams", n = 2) %>% 
  count(house, bigram) %>% 
  bind_tf_idf(bigram, house, n) %>% 
  group_by(house) %>% 
  top_n(tf_idf, n = 100)

Trigram_tfidf <- df %>% 
  get_sentences() %>% 
  unnest_tokens(trigram, "text", token = "ngrams", n = 3) %>% 
  count(house, trigram) %>% 
  bind_tf_idf(trigram, house, n) %>% 
  group_by(house) %>% 
  top_n(tf_idf, n = 100)




