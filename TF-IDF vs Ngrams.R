library(tidyverse)
library(tidytext)
library(sentimentr)

df <- read.csv("harrypotter.csv", stringsAsFactors = FALSE)
df <- df %>% filter(house != "No Entry")
names <- read.csv("namelist.csv")

#Top 100 words by tf-idf plot
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


#Top 100 words by BOW plot
df %>% 
  get_sentences() %>% 
  unnest_tokens(word, "text") %>% 
  filter(!word %in% names$value) %>% 
  filter(!word %in% stop_words$word) %>% 
  group_by(house) %>% 
  count(house, word) %>% 
  top_n(n, n = 100) %>% 
  ggplot(mapping = aes(x = reorder_within(word, n, house), 
                       y = n, 
                       color = house, 
                       fill = house)) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~house, scales = "free", ncol = 1) + 
  coord_flip()



#Tf-Idf top 100 words
bowtfidf <- df %>% 
  get_sentences() %>% 
  unnest_tokens(word, "text") %>% 
  filter(!word %in% names$value) %>% 
  filter(!word %in% stop_words$word) %>% 
  count(house, word, sort = TRUE) %>% 
  bind_tf_idf(word, house, n) %>% 
  group_by(house) %>% 
  top_n(tf_idf, n = 100) %>% 
  select(house, word, tf_idf)

#BOW top 100 words 
bow <- df %>% 
  get_sentences() %>% 
  unnest_tokens(word, "text") %>% 
  filter(!word %in% names$value) %>% 
  filter(!word %in% stop_words$word) %>% 
  group_by(house) %>% 
  count(house, word) %>% 
  top_n(n, n = 100) %>% 
  arrange(house, -n)

#Shared between tfidf and bow
tfidf %>% 
  filter(word %in% bow$word) %>% 
  group_by(house) %>% 
  count(house)


#Bigram feature
bigram <- df %>% 
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
  filter(!word1 %in% names$value,
         !word2 %in% names$value) %>% 
  unite("bigram", c(word1, word2), sep = " ") %>% 
  count(house, bigram) %>% 
  filter(n > 2)


#Tf-idf Bigram
bigramtfidf <- df %>% 
  unnest_tokens(bigram, "text", token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%   
  filter(!word1 %in% names$value,
         !word2 %in% names$value) %>% 
  unite("bigram", c(word1, word2), sep = " ") %>%
  count(house, bigram, sort = TRUE) %>% 
  bind_tf_idf(bigram, house, n) %>% 
  group_by(house) %>% 
  top_n(tf_idf, n = 100) %>% 
  select(house, bigram, tf_idf)


#Plot comparing top 10 bigrams from tf-idf and ngrams 
bigramtfidf %>% 
  group_by(house) %>% 
  top_n(tf_idf, n = 10) %>% 
  rename("house" = house, "bigram" = bigram, "value" = tf_idf) %>% 
  mutate(type = "tf-idf") %>% 
  rbind(bigram %>%
          group_by(house) %>% 
          top_n(n, n =10) %>% 
          rename("house" = house, "bigram" = bigram, "value" = n) %>% 
          mutate(type = "ngram")) %>% 
  ggplot(mapping = aes(x = reorder_within(bigram, value, house), 
                       y = value, 
                       color = house, fill = house)) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~type + house, scales = "free") + coord_flip()



#Plot comparing top 10 words from tf-idf and bow 
df %>% 
  get_sentences() %>% 
  unnest_tokens(word, "text") %>% 
  filter(!word %in% names$value) %>% 
  filter(!word %in% stop_words$word) %>% 
  count(house, word, sort = TRUE) %>% 
  bind_tf_idf(word, house, n) %>% 
  group_by(house) %>% 
  top_n(tf_idf, n = 10) %>% 
  select(house, word, tf_idf) %>% 
  mutate(type = "tf-idf") %>% 
  rename("value" = tf_idf)

df %>% 
  get_sentences() %>% 
  unnest_tokens(word, "text") %>% 
  filter(!word %in% names$value) %>% 
  filter(!word %in% stop_words$word) %>% 
  group_by(house) %>% 
  count(house, word) %>% 
  top_n(n, n = 10) %>% 
  mutate(type = "bow") %>% 
  rename("value" = n)

rbind(df %>% 
        get_sentences() %>% 
        unnest_tokens(word, "text") %>% 
        filter(!word %in% names$value) %>% 
        filter(!word %in% stop_words$word) %>% 
        count(house, word, sort = TRUE) %>% 
        bind_tf_idf(word, house, n) %>% 
        group_by(house) %>% 
        top_n(tf_idf, n = 10) %>% 
        select(house, word, tf_idf) %>% 
        mutate(type = "tf-idf") %>% 
        rename("value" = tf_idf), 
      df %>% 
        get_sentences() %>% 
        unnest_tokens(word, "text") %>% 
        filter(!word %in% names$value) %>% 
        filter(!word %in% stop_words$word) %>% 
        group_by(house) %>% 
        count(house, word) %>% 
        top_n(n, n = 10) %>% 
        mutate(type = "bow") %>% 
        rename("value" = n)) %>% 
  ggplot(mapping = aes(x = reorder_within(word, value, house), 
                       y = value, 
                       color = house, 
                       fill = house)) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~ type + house, 
             scales = "free", ncol = 2) +  
  coord_flip() 



#sentiment tf-idf
tfidf <-  df %>% 
  get_sentences() %>% 
  unnest_tokens(word, "text") %>% 
  filter(!word %in% names$value) %>% 
  filter(!word %in% stop_words$word) %>% 
  count(house, word, sort = TRUE) %>% 
  bind_tf_idf(word, house, n) %>% 
  group_by(house)

