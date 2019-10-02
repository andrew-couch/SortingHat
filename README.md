# SortingHat 
Harry Potter Classification Model using Screenplay and Book data

## Data
The features consists of Book, House, Character, and Text
* Book - title of the harry potter book with the medium (book/movie)
*	House - five levels, 4 houses and non entry which are characters who are not in the houses
*	Character - characters that are also separated by medium
* Text - unprocessed sentences

## Pre-Processing for Feature Engineering
* The data will be separated to one sentence per observation 
*	This will be done with get_sentences from the sentimentR package

## Feature Engineering 

#### Bag of Words 
*	A name list of all unique words from the dataset will be created as a set of features for bag of words
*	Bag of words will be counted and joined to each  sentence_id that corresponds with the character and its sentence

#### Bigrams
*	A bigram list of bigrams that have been observed more than 1 will be created as a set of features for bigrams
*	Bigrams will be counted and joined to each sentence_id that corresponds with the character and its sentence 

#### Trigrams
*	A trigram list of trigrams that have been observed more than 1 will be created as a set of features for trigrams
*	Trigrams will be counted and joined to each sentence_id that corresponds with the character and its sentence 

#### TF-IDF 
*	TF-IDF will be used in conjunction with bag of words to find unique words for each house 
*	This will used bag of words and use houses as the documents 
*	This essentially finds unique and common words for each house 
*	This may be substituted as the word list for the bag of words features for modeling

#### Sentiment Lexicon
* Since the data is pre-processed for each observation as a sentence, lexicons will be used to evaluate each sentence's sentiment
* After sentiment has been measured, the sentiment values will be averaged down by word and may have an additional feature of average by sentence

#### Pre-Processing for Model Training 
*	Once features are generated, the feature lists will need to be saved to create feature constraints for future data
*	Ex: if "dog" was never said and the new data contains the word "dog", "dog" will be ignored and only words in the list will be used 
*	The data will then be condensed/grouped by each character,
* Some characters will say more words than others so ngram features will need to be centered and scaled, either by words/ngrams or with general center scale transformations 

## Modeling
*	The model will be a multiclass classification problem 
*	Multiple models will be created and once tuned, will be ensembled 
*	Models will range from simple logistic regression models to Recurrent Neural Networks
*	Models will be created using caret and keras
* Model ensembling will be done using majority vote 

## Model List
#### Multinomial Logistic Regression
* method = logreg

#### Naive Bayes Classifier
* method = naive_bayes

#### Regularized Logistic Regression (L1)
* method = reglogistic

#### Penalized Logistic Regression (L2)
* method = plr

#### Elastic Net
* method = glmnet

#### Multivariate Adaptive Regression Spline
* method = earth

#### Random Forest
* method = rf

#### Support Vector Machine
* method = svmLinear

#### Extreme Gradient Boosting
* method = XGBoostLinear

#### Convolutional Neural Network
* method = Keras pacakge 

#### Long Short Term Memory Neural Network
* method = Keras package
