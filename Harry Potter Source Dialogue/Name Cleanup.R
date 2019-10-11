library(tidyverse)
library(tidytext)

book <- read.csv("booktext.csv")
screenplay <- read.csv("harrypotterclean.csv")


book <- book %>% select(-element_id, -sentence_id)

characterHouse <- screenplay %>% select(Character, House) %>% unique()


book$character <-  paste(substring(book$character, 1,1) %>% toupper(),
      substring(book$character, 2, book$character %>% as.character() %>% nchar()), sep = "")



book <- book %>% left_join(characterHouse, by = c("character" = "Character"))


missing <- book %>% filter(is.na(House))
missingNames <- missing %>% select(character) %>% unique() %>% arrange(character)
#write.csv(book, "bookmissingnames.csv",row.names = FALSE)

prof <- read.csv("bookmissingnames.csv")
prof$obs <- seq(1:nrow(prof))

prof %>% 
  filter(character == "Professor") %>% 
  unnest_tokens(bigram, `text`, "ngrams", n = 2) %>% 
  separate(bigram, c("word1","word2"), sep = " ") %>% 
  filter(word1 == "professor") %>% 
  select(word2) %>% 
  filter(word2 %in% c("mcgonagall","dumbledore","sprout","binns","lockhart","trelawney","moody","karakaroff","grubbly","snape","slughorn","flitwick","quirrell","lupin"))

labeledProf <- prof %>% 
  filter(character == "Professor") %>% 
  unnest_tokens(bigram, `text`, "ngrams", n = 2) %>% 
  separate(bigram, c("word1","word2"), sep = " ") %>% 
  filter(word1 == "professor"& word2 %in% c("mcgonagall","dumbledore","sprout","binns","lockhart","trelawney","moody","karakaroff","grubbly","snape","slughorn","flitwick","quirrell","lupin")) %>% 
  mutate(character = word2) %>% 
  select(-word1, -word2) %>% 
  inner_join(prof, by = ("obs" = "obs")) %>% 
  select(character.x, book.x, text, House.x, obs)

colnames(labeledProf) <- c("character","book","text", "house", "obs")
prof <- prof %>% filter(character != "Professor")
colnames(prof) <- c("character","book","text", "house", "obs")

booktext <- rbind(prof, labeledProf)
write.csv(booktext, "booktextwithprof.csv", row.names = FALSE)