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


#Finds said synonyms for Harry
df %>% 
  unnest_tokens(bigram,`text`, token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  select(word1, word2) %>% 
  filter(word1 == "harry" | word2 == "harry") %>% 
  gather() %>% 
  select(value) %>% 
  unique() %>% 
  arrange(value) %>% 
  anti_join(stop_words, by = c("value" = "word")) %>% 
  filter(substr(value, start = nchar(value), stop = nchar(value)) == "d")

#dialogueVerbs is the entire list of said synonyms used to find characters in the books 
dialogueVerbs <- c("accused", "agreed", "announced", "answered", "argued", "assured","begged","blurted","called","challenged","cheered","confessed","convinced","cried","decided","echoed","explained","fretted","gasped","guessed","hissed","imitated","informed","interjected","interrupted","japped","persuaded","promised","prompted","protested","read","replied","responded","retold","roared","scowled","screamed","screeched","shared","shouted","shurgged","snarled","sniggered","snorted","squeled","squeaked","stammered","said","told","voiced","wailed","whimpered","whispered","worried","yawned","yelled","yelped")
dialogueVerbs <- as.data.frame(dialogueVerbs)

#Finds said synonyms for Ron
df %>% 
  unnest_tokens(bigram,`text`, token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  select(word1, word2) %>% 
  filter(word1 == "ron" | word2 == "ron") %>% 
  gather() %>% 
  select(value) %>% 
  unique() %>% 
  arrange(value) %>% 
  anti_join(stop_words, by = c("value" = "word")) %>% 
  filter(substr(value, start = nchar(value), stop = nchar(value)) == "d") %>% 
  select(value) %>% 
  anti_join(dialogueVerbs, by = c("value" = "dialogueVerbs"))

#Uses holder to add synonyms to a dataframe 
addedWords <- c("added","breathed","barked","breathed","chortled","demanded","exchanged","exclaimed","intervened","moaned","mumbled","offered","retorted","shocked","sighed","sneered","spluttered","suggested") %>% as.data.frame()
colnames(addedWords) <- "dialogueVerbs"
dialogueVerbs <-  rbind(dialogueVerbs, addedWords)


#Finds said synonyms for Hermione
df %>% 
  unnest_tokens(bigram,`text`, token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  select(word1, word2) %>% 
  filter(word1 == "hermione" | word2 == "hermione") %>% 
  gather() %>% 
  select(value) %>% 
  unique() %>% 
  arrange(value) %>% 
  anti_join(stop_words, by = c("value" = "word")) %>% 
  filter(substr(value, start = nchar(value), stop = nchar(value)) == "d") %>% 
  select(value) %>% 
  anti_join(dialogueVerbs, by = c("value" = "dialogueVerbs"))
addedWords <- c("added","bemused","bickered","choked","cooed","elaborated","giggled","groaned","jeered","laughed","mentioned","murmured","muttered","reminded","scoffed","shrieked","squealed","talked","warned","winced") %>% as.data.frame()
colnames(addedWords) <- "dialogueVerbs"
dialogueVerbs <-  rbind(dialogueVerbs, addedWords)

#Finds said synonyms for Malfoy
df %>% 
  unnest_tokens(bigram,`text`, token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  select(word1, word2) %>% 
  filter(word1 == "malfoy" | word2 == "malfoy") %>% 
  gather() %>% 
  select(value) %>% 
  unique() %>% 
  arrange(value) %>% 
  anti_join(stop_words, by = c("value" = "word")) %>% 
  filter(substr(value, start = nchar(value), stop = nchar(value)) == "d") %>% 
  select(value) %>% 
  anti_join(dialogueVerbs, by = c("value" = "dialogueVerbs"))
addedWords <- c("bawled","bellowed","chuckled","howled","mouthed") %>% as.data.frame()
colnames(addedWords) <- "dialogueVerbs"
dialogueVerbs <- rbind(dialogueVerbs, addedWords)



#Finds characters by using bigrams, if word has said than it could be harry said or said harry
firstNameLexicon <- lexicon::freq_first_names
firstNameLexicon <- firstNameLexicon %>% select(Name)
firstNameLexicon <-  sapply(firstNameLexicon, tolower) %>% as.data.frame()
colnames(firstNameLexicon) <- "names"

#uses freq_first_names lexicon to find names in harry potter 
#many names missing but still is worth doing 
nameList <- df %>% 
  unnest_tokens(bigram,`text`, token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  select(word1, word2) %>% 
  filter(word1 %in% dialogueVerbs$dialogueVerbs | word2 %in% dialogueVerbs$dialogueVerbs) %>% 
  gather() %>% 
  select(value) %>% 
  unique() %>% 
  filter(!value %in% dialogueVerbs$dialogueVerbs) %>% 
  anti_join(stop_words, by = c("value" = "word")) %>% 
  arrange(value) %>% 
  filter(value %in% firstNameLexicon$names)


#filters out old names 
df %>% 
  unnest_tokens(bigram,`text`, token = "ngrams", n = 2) %>%  
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  select(word1, word2) %>% 
  filter(word1 %in% dialogueVerbs$dialogueVerbs | word2 %in% dialogueVerbs$dialogueVerbs) %>% 
  gather() %>% 
  select(value) %>% 
  unique() %>% 
  filter(!value %in% dialogueVerbs$dialogueVerbs) %>% 
  anti_join(stop_words, by = c("value" = "word")) %>% 
  arrange(value) %>% 
  anti_join(nameList, by = c("value" = "value")) %>% 
  view()

#Manually finding names (first and last that were missed)
#Didn't use freq_last name lexicon because potential overalp with regular words 

newNames <- c("cho","crabbe","dobby","dolhov","draco","dully","dumbledore","dursley","finnigan","firenze","fleur","flitwick","goyle","granger","gregorovitch","grindelvald","gridenlwald","hargrid","harry","harper","hermione","igro","jones","jorkins","karakoff","kingsley","kirke","kreacher","krum","lovegood","lucius","lupin","macmillan","macnair","malfoy","malkin","mcgonagall","mclaggen","mcmillan","nagini","ninny","nox","nymphadora","o'flaherty","olivander","padma","parkinson","parvati","phineas","pomona","potter","professor","riddle","roberts","romilda","romulus","ronan","rookwood","scropious","seamus","sirius","snape","snivellus","snivelus","tonks","tosh","trelawney","umbridge","uncle","voldemort","warrington","weasley","weaseleys","wormtail","") %>% as.data.frame()
colnames(newNames) <- "value"

nameList <- rbind(nameList,newNames) %>% arrange()

#uses nameList and dialogueVerbs to find sentences in books
#finds the index (book, chapter, and sentence) where dialogue verb and name are in bigram
sentences <- df %>% 
  get_sentences() %>% 
  unnest_tokens(bigram, `text`, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter((word1 %in% dialogueVerbs$dialogueVerbs & word2 %in% nameList$value) | (word1 %in% dialogueVerbs$dialogueVerbs & word2 %in%    nameList$value)) %>% 
  select(-word1, -word2) %>% 
  unique() %>% 
  arrange(book, element_id, sentence_id) %>% 
  inner_join(df %>% get_sentences(), 
             by = c("book" = "book", "element_id" = "element_id", "sentence_id" = "sentence_id"))

#finds sentences with bigram of character verb that also contains quotes 
sharedText <- inner_join(df %>% 
                           get_sentences() %>% 
                           filter(str_detect(text, '\\"') == TRUE) %>% 
                           select(book, element_id, sentence_id), 
                         sentences, by = c("book" = "book", "element_id" = "element_id","sentence_id" = "sentence_id")) %>% 
  arrange(book, element_id, sentence_id)

#adds bigram features
sharedText <- sharedText %>% 
  mutate(key = paste(book, element_id, sentence_id, sep = "")) %>% 
  unnest_tokens(bigram, `text`, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 %in% nameList$value & word2 %in% dialogueVerbs$dialogueVerbs |
           word2 %in% nameList$value & word1 %in% dialogueVerbs$dialogueVerbs) %>% 
  unite("bigram", c(word1, word2), sep = " ") %>%
  count(key,bigram) %>% 
  inner_join(sharedText %>% 
               mutate(key = paste(book, element_id, sentence_id, sep = "")) , 
             by = c("key" = "key")) %>% 
  select(-n) %>% 
  arrange(book, element_id, sentence_id)

#labels sentences that contain 2 names and dialogue verbs
#chooses verb dialogue over dialogue verb (said harry over harry said)
duplicateSharedText <- sharedText %>% 
  count(key) %>% 
  filter(n != 1) %>% 
  select(key) %>% 
  inner_join(sharedText, by = ("key" = "key")) %>% 
  select(-key, -bigram) %>% 
  mutate(key = paste(book,element_id, sentence_id,sep = "")) %>% 
  unnest_tokens(bigram, `text`, token = "ngrams", n =2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 %in% dialogueVerbs$dialogueVerbs & word2 %in% nameList$value) %>% 
  unique() %>% select(word2, key) %>% 
  inner_join(sharedText, by = ("key" = "key")) %>% 
  select(-key, -bigram)

#labels dialogue that does not have duplicate dialoge verbs or characters 
#finds name matching with any bigram 
uniqueSharedText <- sharedText %>% 
  count(key) %>% 
  filter(n == 1) %>% 
  select(key) %>% 
  inner_join(sharedText, by = ("key" = "key")) %>% 
  select(key, bigram) %>% 
  unnest_tokens(bigram, `bigram`, token = "ngrams", n =2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  gather(key = "bigram", value = "word", -key) %>% 
  select(-bigram) %>% 
  filter(word %in% nameList$value) %>% 
  inner_join(sharedText, by = ("key" = "key")) %>% 
  select(-bigram, -key)

#Creates final df with labeled dialogue
colnames(duplicateSharedText) <- c("character", "book","element_id","sentence_id","text")
colnames(uniqueSharedText) <- c("character", "book","element_id","sentence_id","text")
bookText <- rbind(duplicateSharedText, uniqueSharedText) %>% 
  unique() %>% 
  arrange(book, element_id, sentence_id)

#write.csv(bookText, "booktext.csv", row.names = FALSE)