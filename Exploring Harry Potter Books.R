library(tidyverse)
library(tidytext)
library(harrypotter)
library(sentimentr)

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

harryWords <- df %>% 
  unnest_tokens(bigram,`text`, token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  select(word1, word2) %>% 
  filter(word1 == "harry" | word2 == "harry") %>% 
  gather() %>% 
  select(value) %>% 
  arrange(value) %>% 
  anti_join(stop_words, by = c("value" = "word")) %>% 
  select(value) %>% 
  filter(!value %in% c("harry", "potter","hermione", "ron")) %>% 
  count(value) %>% 
  top_n(n, n = 10) %>% 
  arrange(-n)

ronWords <- df %>% 
  unnest_tokens(bigram,`text`, token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  select(word1, word2) %>% 
  filter(word1 == "ron" | word2 == "ron") %>% 
  gather() %>% 
  select(value) %>% 
  arrange(value) %>% 
  anti_join(stop_words, by = c("value" = "word")) %>% 
  select(value) %>% 
  filter(!value %in% c("harry", "potter","hermione", "ron", "weasley", "ginny")) %>% 
  count(value) %>% 
  top_n(n, n = 10) %>% 
  arrange(-n)


hermioneWords <- df %>% 
  unnest_tokens(bigram,`text`, token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  select(word1, word2) %>% 
  filter(word1 == "hermione" | word2 == "hermione") %>% 
  gather() %>% 
  select(value) %>% 
  arrange(value) %>% 
  anti_join(stop_words, by = c("value" = "word")) %>% 
  select(value) %>% 
  filter(!value %in% c("harry", "potter","hermione", "ron", "granger")) %>% 
  count(value) %>% 
  top_n(n, n = 10) %>% 
  arrange(-n)

ronWords
harryWords
hermioneWords

harry <- ggplot(data = harryWords, mapping = aes(x = reorder(value, n), y = n)) + geom_col(fill = "red") + coord_flip() + ggtitle("Harry")  
ron <- ggplot(data = ronWords, mapping = aes(x = reorder(value, n), y = n)) + geom_col(fill = "blue") + coord_flip() + ggtitle("Ron")
hermione <- ggplot(data = hermioneWords, mapping = aes(x = reorder(value, n), y = n)) + geom_col(fill = "green") + coord_flip() + ggtitle("Hermione")

gridExtra::grid.arrange(harry, ron, hermione)

topWords <- df %>% 
  get_sentences() %>% 
  unnest_tokens("word", `text`) %>% 
  filter(!word %in% stop_words$word) %>% 
  select(book, word) %>% 
  group_by(book) %>% 
  count(word) %>% 
  top_n(n, n =10)

topWords <- topWords %>% arrange(book, -n) %>% group_by(book) %>% mutate(order = row_number()) 
topWords$book <- factor(topWords$book, levels = c("philosophers_stone","chamber_of_secrets","prisoner_of_azkaban","goblet_of_fire","order_of_the_phoenix","half_blood_prince","deathly_hallows"))

ggplot(data = topWords, mapping = aes(reorder_within(word, n, book, sep = " "), n)) + geom_col(aes(color = book, fill = book)) + facet_wrap(~book, scales = "free") + coord_flip()

#use of harry, ron, and hermione in the books 
topWords %>% ggplot(mapping = aes(x = book, y = n, group = word, color = word)) + geom_line() + scale_y_log10() 