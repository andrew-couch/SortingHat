#install.packages("text2vec")
library(text2vec)

test <- c("this is andrew", "this is a test", "this is a bigram")
test2 <- c("this is a test for a model")

#Creates word vecotr
it_train <- itoken(test,
       preprocessor = tolower,
       tokenizer = word_tokenizer)
#Created word list for vector 
vocab <- create_vocabulary(it_train)
#Creates matrix using word list
testMatrix <-  create_dtm(it_train, vocab_vectorizer(vocab))

testMatrix
#Creates word vector 
it_test <- itoken(test2,
                  preprocessor = tolower,
                  tokenizer = word_tokenizer)

#Uses oringial word vector (note that for and model are not counted)
test2matrix <- create_dtm(it_test, vocab_vectorizer(vocab))

bigram <- create_vocabulary(it_train, ngram = c(1L,2L))
bigramTestMatrix <- create_dtm(it_train, vocab_vectorizer(bigram))


bigramTest2Matrix <- create_dtm(it_test, vocab_vectorizer(bigram))


as.matrix(bigramTest2Matrix)