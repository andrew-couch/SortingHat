library(shiny)
library(rtweet)
library(tidyverse)
library(rtweet)
library(tidytext)
library(sentimentr)
library(doParallel)
library(ggrepel)
library(ggthemes)

get_token()

bow <- read.csv("bowlist.csv", header = TRUE,stringsAsFactors = FALSE)
bigram <- read.csv("bigramlist.csv", header = TRUE,stringsAsFactors = FALSE)
trigram <- read.csv("trigramlist.csv", header = TRUE,stringsAsFactors = FALSE)
emotions <- lexicon::nrc_emotions
LogisticRegressionModel <- readRDS("LogisticRegressionModel.rds")
NaiveBayesModel <- readRDS("NaiveBayesModel.rds")
L1Model <- readRDS("L1Model.rds")
L2Model <- readRDS("L2Model.rds")
ElasticNetModel <- readRDS("ElasticNetModel.rds")
MARSModel <- readRDS("MARSModel.rds")
KnnModel <- readRDS("KnnModel.rds")
RandomForestModel <- readRDS("RandomForestModel.rds")
SVMModel <- readRDS("SupportVectorMachineModel.rds")
EnsembleModel <- readRDS("EnsembleModel.rds")


ui <- fluidPage(
  h1("The Sorting Hat"),
  actionButton("go", "Scrape Tweets"),
  textInput("username", h3("Twitter Username"), NULL),
  plotOutput("houseplot")
  
)

server <- function(input, output) {
  tweetData <- reactiveValues(data = NULL)
  
  observeEvent(input$go, {
    tweetData$data <- get_timeline(input$username, n = 3200)
    tweetData$data <- tweetData$data %>% 
      filter(is_retweet == "FALSE") %>% 
      select(text)
    
    bowFeatures <- tweetData$data %>% 
      unnest_tokens(word, "text") %>% 
      filter(word != "tweet") %>% 
      right_join(bow, by = c("word" = "bow")) %>% 
      count(word, word) %>%
      mutate(n = n-1) %>% 
      spread(word, n)
    
    bigramFeatures <- tweetData$data %>% 
      unnest_tokens(bigram, "text", token = "ngrams", n = 2) %>% 
      right_join(bigram, by = c("bigram" = "bigram")) %>% 
      count(bigram, bigram) %>%
      mutate(n = n-1) %>% 
      spread(bigram, n)
    
    trigramFeatures <- tweetData$data %>% 
      unnest_tokens(trigram, "text", token = "ngrams", n = 3) %>% 
      right_join(trigram, by = c("trigram" = "trigram")) %>% 
      count(trigram, trigram) %>%
      mutate(n = n-1) %>% 
      spread(trigram, n)
    
    sentences <- tweetData$data %>% select(text) %>% get_sentences()
    
    sentiments <- cbind(
      sentences %>% 
        sentiment((lexicon::hash_sentiment_huliu)) %>% 
        select(sentiment) %>% 
        rename("huliu" = sentiment),
      sentences %>% 
        sentiment(lexicon::hash_sentiment_jockers_rinker) %>% 
        select(sentiment) %>% 
        rename("jockers_rinker" = sentiment),
      sentences %>% 
        sentiment(lexicon::hash_sentiment_nrc) %>% 
        select(sentiment) %>% 
        rename("nrc" = sentiment),
      sentences %>% 
        sentiment(lexicon::hash_sentiment_senticnet) %>% 
        select(sentiment) %>% 
        rename("senticnet" = sentiment),
      sentences %>% 
        sentiment(lexicon::hash_sentiment_sentiword) %>% 
        select(sentiment) %>% 
        rename("sentiword" = sentiment),
      sentences %>% 
        sentiment(lexicon::hash_sentiment_slangsd) %>% 
        select(sentiment) %>% 
        rename("slangsd" = sentiment),
      sentences %>% 
        sentiment(lexicon::hash_sentiment_socal_google) %>% 
        select(sentiment) %>% 
        rename("socal_google" = sentiment)) 
    
    sentiments <-  sentiments %>% 
      gather(key = "sentiment", value = "score") %>% 
      group_by(sentiment) %>% 
      summarise(score = mean(score)) %>% 
      spread(key = sentiment, value = score)
    
    emotionFeatures <- sentences %>% 
      unnest_tokens(word, "text") %>% 
      filter(word %in% emotions$term) %>% 
      left_join(emotions, by = c("word" = "term")) %>% 
      select(-word,-element_id, -sentence_id) %>% 
      summarise_each(funs(sum)) %>% 
      gather(key = "sentiment", value = "score") %>% 
      mutate(score = score / sentences %>% unnest_tokens(word, "text") %>% nrow()) %>% 
      spread(sentiment, score) %>% 
      rename("anger.emotion" = anger,
             "anticipation.emotion" = anticipation,
             "digust.emotion" = disgust,
             "fear.emotion" = fear,
             "joy.emotion" = joy,
             "sadness.emotion" = sadness,
             "surprise.emotion" = surprise,
             "trust.emotion" = trust)
    
    df <- cbind(bowFeatures, bigramFeatures, trigramFeatures, sentiments, emotionFeatures)
    
    ensembleData <- cbind(predict(LogisticRegressionModel, df, type = "prob"),
                          predict(NaiveBayesModel, df, type = "prob"),
                          predict(L1Model, df, type = "prob"),
                          predict(L2Model,df, type = "prob"),
                          predict(ElasticNetModel, df, type = "prob"),
                          predict(MARSModel, df, type = "prob"),
                          predict(KnnModel, df, type = "prob"),
                          predict(RandomForestModel, df, type = "prob"),
                          predict(SVMModel, df, type = "prob"))
    colnames(ensembleData) <- c("LogisticGryffindor","LogisticHufflepuff","LogisticRavenclaw","LogisticSlytherin",
                                "NaiveBayesGryffindor","NaiveBayesHufflepuff","NaiveBayesRavenclaw","NaiveBayesSlytherin",
                                "L1Gryffindor","L1Hufflepuff","L1Ravenclaw","L1Slytherin",
                                "L2Gryffindor","L2Hufflepuff","L2Ravenclaw","L2Slytherin",
                                "ElasticNetGryffindor","ElasticNetHufflepuff","ElasticNetRavenclaw","ElasticNetSlytherin",
                                "MARSGryffindor","MARSHufflepuff","MARSRavenclaw","MARSSlytherin",
                                "KNNGryffindor","KNNHufflepuff","KNNRavenclaw","KNNSlytherin",
                                "RandomForestGryffindor","RandomForestHufflepuff","RandomForestRavenclaw","RandomForestSlytherin",
                                "SVMGryffindor","SVMHufflepuff","SVMRavenclaw","SVMSlytherin")
    
    HousePrediction <- predict(EnsembleModel, ensembleData, type = "prob")
    
    colnames(HousePrediction) <- c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")
    
    })
  
  output$houseplot <- renderPlot({
    req(is.null(tweetData$data) == FALSE)
    HousePrediction %>% 
      gather(key = "House", value = "Percentage") %>% 
      ggplot(aes(x = House, 
                 y = Percentage, 
                 color = House, 
                 fill = House)) + 
      geom_col() + 
      scale_y_continuous(labels = scales::percent) + 
      ggtitle(paste(userName, "'s House Assignment", sep = "")) + 
      theme(plot.title = element_text(hjust = .5)) + 
      theme_economist()
  })
  
}

shinyApp(ui = ui, server = server)