library(tidyverse)
library(tidytext)
library(sentimentr)
library(ggthemes)
options(scipen = 999)

df <- read.csv("harrypotter.csv")
df$text <- as.character(df$text)
df <- df %>% mutate(medium = df$book %>% str_extract(pattern = paste(c("Book","Movie"), collapse = "|")))
names <- df %>% separate(character, c("medium","character"), sep = "_") %>% select(character) %>% unique()
names <- tolower(names$character)
df <- df %>% separate(book, c("medium","title"), sep = "_", extra = "merge")
df <- df %>% separate(character, c("medium","name"), remove = FALSE)

#Comapres spoken words by house
df %>% 
  filter(house != "No Entry") %>% 
  get_sentences() %>% 
  unnest_tokens(word, `text`) %>% 
  anti_join(stop_words, by = ("word" = "word"))%>% 
  filter(!word %in% names) %>% 
  group_by(house) %>% 
  count(word) %>% 
  top_n(n, n = 10) %>% 
  arrange(house, -n) %>%
  ungroup() %>% 
  mutate(word = reorder_within(word, n, house)) %>% 
  ggplot(mapping = aes(x = word, y = n, color = house, fill = house)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~house, scales = "free") +
  scale_x_reordered() +
  coord_flip() + 
  theme_economist() + ggtitle("Most popular words by house")


  #Comapres Pos/Neg words for each house 
  df %>% 
    filter(house != "No Entry") %>% 
    get_sentences() %>% 
    unnest_tokens(word, `text`) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing")) %>% 
    group_by(house, word, sentiment) %>% 
    count(sentiment) %>% 
    group_by(house, sentiment) %>% 
    mutate(rank = rank(-n, ties.method = "first")) %>% 
    filter(rank <= 5) %>% 
    ungroup() %>% 
    mutate(n = if_else(sentiment == "positive",n,-n)) %>%  
    mutate(word = reorder_within(word, n, house)) %>% 
    ggplot(aes(x = word, y = n, fill = sentiment)) +
    geom_col() + 
    scale_x_reordered() +
    facet_wrap(~house, scales = "free") + 
    theme_economist() +
    coord_flip() + 
    ggtitle("Top 5 positive and negative words by house")
  
  #compares sentiment from books/movies
  df %>% 
    filter(house != "No Entry") %>% 
    get_sentences() %>% 
    unnest_tokens(word, `text`) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing")) %>% 
    group_by(title, word, sentiment) %>% 
    count(sentiment) %>% 
    group_by(title, sentiment) %>% 
    mutate(rank = rank(-n, ties.method = "first")) %>% 
    filter(rank <= 5) %>% 
    ungroup() %>% 
    mutate(n = if_else(sentiment == "positive",n,-n)) %>%  
    mutate(word = reorder_within(word, n, title)) %>% 
    ggplot(aes(x = word, y = n, fill = sentiment)) +
    geom_col() + 
    scale_x_reordered() +
    facet_wrap(~title, scales = "free") + 
    coord_flip() +
    theme_economist() + 
    ggtitle("Top 5 positive/negative words by book")
  
  
  #Compares sentiment trends in the books by genre
  df %>% 
    filter(house != "No Entry") %>% 
    get_sentences() %>% 
    unnest_tokens(word, `text`) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing")) %>% 
    group_by(medium , title, sentiment) %>% 
    count(sentiment) %>% 
    filter(medium != "wiki") %>% 
    filter(title != "Wiki") %>% 
    filter(!title %in% c("Half_Blood_Prince","Order_Of_The_Phoenix","Wiki")) %>% 
    ungroup() %>% 
    mutate(title = factor(title, levels = c("Philosophers_Stone","Sorcerer_Stone","Chamber_Of_Secrets","Prisoner_Of_Azkaban","Prisonor_Of_Azk","Goblet_Of_Fire","Deathly_Hallows"))) %>% 
    ggplot(aes(x = title, y = n, color = sentiment, group = sentiment)) + 
    geom_line(size = 1) + 
    facet_wrap(~medium, scales = "free") +
    theme_economist() +
    ggtitle("Positive/Negative sentiment trends by medium")
  

  #Compares house sentiment trends throughout the series by medium 
  df %>% 
    filter(medium != "wiki" | title != "Wiki") %>% 
    filter(house != "No Entry") %>% 
    filter(!title %in% c("Half_Blood_Prince","Order_Of_The_Phoenix","Wiki")) %>% 
    get_sentences() %>% 
    unnest_tokens(word, `text`) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing")) %>% 
    group_by(medium, title, house, sentiment) %>% 
    count(sentiment) %>% 
    inner_join(df %>% 
                 filter(medium != "wiki" | title != "Wiki") %>% 
                 filter(house != "No Entry") %>% 
                 filter(!title %in% c("Half_Blood_Prince","Order_Of_The_Phoenix","Wiki")) %>% 
                 get_sentences() %>% 
                 unnest_tokens(word, `text`) %>% 
                 anti_join(stop_words) %>% 
                 group_by(medium,title, house) %>% 
                 count(word) %>% 
                 group_by(medium,title, house) %>% 
                 summarise(totalword = sum(n)), 
               by = c("medium" = "medium", "title" = "title", "house" = "house")) %>% 
    summarise(score = n/totalword) %>% 
    ungroup() %>% 
    mutate(title = factor(title, levels = c("Philosophers_Stone","Sorcerer_Stone","Chamber_Of_Secrets","Prisoner_Of_Azkaban","Prisonor_Of_Azk","Goblet_Of_Fire","Deathly_Hallows"))) %>% 
    ggplot(aes(x = title, y = score, color = house, group = house)) + geom_line(size = 1) + facet_wrap(~medium + sentiment, scales = "free") + theme_economist() +
    ggtitle("Positive Negative Trends by House and Medium")


 #top 5 Pos neg words by house and title
  df %>% 
    filter(house != "No Entry") %>% 
    filter(!title %in% c("Half_Blood_Prince","Order_Of_The_Phoenix","Wiki")) %>% 
    filter(medium == "Movie") %>% 
    get_sentences() %>% 
    unnest_tokens(word, `text`) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing")) %>% 
    group_by(title, house, word, sentiment) %>% 
    count(sentiment) %>% 
    group_by(title,house, sentiment) %>% 
    mutate(rank = rank(-n, ties.method = "first")) %>% 
    filter(rank <= 5) %>% 
    ungroup() %>% 
    mutate(n = if_else(sentiment == "positive",n,-n)) %>%  
    mutate(word = reorder_within(word, n, title) )%>% 
    mutate(title = factor(title, levels = c("Philosophers_Stone","Sorcerer_Stone","Chamber_Of_Secrets","Prisoner_Of_Azkaban","Prisonor_Of_Azk","Goblet_Of_Fire","Deathly_Hallows"))) %>% 
    ggplot(aes(x = reorder_within(word, n, house), y = n, fill = sentiment)) +
    geom_col() + 
    scale_x_reordered() +
    facet_wrap(house~title, scales = "free") +
    coord_flip() +
    ggtitle("Top 5 Positive/Negative words by house and book") + 
    theme_economist() + 
    theme(legend.position = "top",
          legend.justification = "center",
          plot.title = element_text(hjust = .5))
  
 
#using tf-idf plotting top 5 words  
  df %>% 
    filter(house != "No Entry") %>% 
    get_sentences() %>% 
    unnest_tokens(word, `text`) %>% 
    filter(!word %in% names) %>% 
    count(house,word, sort = TRUE) %>% 
    bind_tf_idf(word, house, n) %>% 
    group_by(house) %>% 
    top_n(tf_idf, n = 5) %>% 
    ungroup() %>% 
    mutate(word = reorder(word, tf_idf)) %>% 
    ggplot(aes(x = word, y = tf_idf, color = house, fill = house)) + 
    geom_col() + 
    facet_wrap(~house, scales = "free") + 
    coord_flip() + 
    theme_economist() + 
    ggtitle("Top 5 tf-idf words by House")
  

#Top 10 characters and their top 5 positive negative words   
df %>% 
  filter(house != "No Entry") %>% 
  inner_join( df %>% 
                select(name) %>% 
                count(name) %>% 
                top_n(n, n = 10) %>% 
                select(name), by = ("name" = "name")) %>% 
  get_sentences() %>% 
  unnest_tokens(word, `text`) %>%
  anti_join(stop_words) %>% 
  inner_join(get_sentiments(lexicon = "bing")) %>%
  select(name, word, sentiment) %>% 
  group_by(name, word, sentiment) %>% 
  count(sentiment) %>% 
  group_by(name, sentiment) %>% 
  top_n(n, n = 5) %>% 
  arrange(name, sentiment, -n) %>% 
  ungroup() %>% 
  mutate(n = if_else(sentiment == "positive", n,-n)) %>% 
  ggplot(mapping = aes(x = reorder_within(word, n, name), 
                       y = n, 
                       color = sentiment, 
                       fill = sentiment)) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~name, scales = "free") + 
  coord_flip() + 
  theme_economist() + 
  ggtitle("Top 5 positive/negative words by each character")


#Comparing BOW and Tf-idf

rbind(df %>% get_sentences() %>% 
          unnest_tokens(word,"text") %>% 
          select(house, word) %>% 
          filter(!word %in% names) %>% 
          filter(!word %in% stop_words$word) %>% 
          count(house,word) %>% 
          group_by(house) %>% 
          top_n(n, n = 5) %>% 
          rename("value" = n) %>% 
          mutate(type = "bow"), 
        df %>% get_sentences() %>% unnest_tokens(word, "text") %>% select(house,word) %>% 
          filter(!word %in% names) %>% 
          count(house,word) %>% 
          bind_tf_idf(word, house,n) %>% 
          select(house,word, tf_idf) %>% 
          group_by(house) %>% 
          top_n(tf_idf, n =5) %>% 
          mutate(type = "tf-df") %>% 
          rename("value" = tf_idf)) %>% 
  ggplot(mapping = aes(x = reorder_within(word, value, house), 
                       y = value, 
                       color = house, 
                       fill = house)) + 
  geom_col() + 
  scale_x_reordered() + 
  facet_wrap(~house + type, scales = "free", ncol = 2) + 
  coord_flip() + 
  theme_economist() +
  ggtitle("Bag of words vs Tf-idf")