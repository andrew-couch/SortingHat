# SortingHat 
Harry Potter Classification Model using Screenplay and Book data

## Data
The features consists of Book, House, Character, and Text
* Book - title of the harry potter book with the medium (book/movie)
*	House - five levels, 4 houses and non entry which are characters who are not in the houses
*	Character - characters that are also separated by medium
* Text - unprocessed sentences

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
*	TF-IDF will be used in conjunction with bag of words and ngrams to find unique words/ngrams for each house 
*	This will act as a feature reduction
* Top_N will be used to find the top n bag of words/ngrams for each house 
* N will be set arbitrarily however will try to balance word diversity and how well it generalizes the data 

#### Sentiment Lexicon
* Sentiment will be measured for each character 
* Multiple lexicons will be used (Huliu, Jockers Rinker, NRC, Senticnet, Sentiword, SlangSD, Socal Google, and NRC emotions)
* The sentiment will be measured by setence level however, will be averaged for each character
* The averaging is done through simple averaging instead of weighted average

#### Pre-Processing for Model Training 
*	Once features are generated, the feature lists will need to be saved to create feature constraints for future data
*	Ex: if "dog" was never said and the new data contains the word "dog", "dog" will be ignored and only words in the list will be used 
*	The data will then be condensed/grouped by each character,
* Bow/Ngrams may need to be centered due to character dialogue variance

## Modeling
*	The model will be a multiclass classification problem 
*	Multiple models will be created and once tuned, will be ensembled 
*	Models will range from simple logistic regression model to a Recurrent Neural Network
*	Models will be created using caret and keras
* Model ensembling will be done using majority vote 

## Model List
#### Multinomial Logistic Regression (Completed)
* method = LogitBoost

#### Naive Bayes Classifier (Completed)
* method = naive_bayes

#### Regularized Logistic Regression (L1) (Completed)
* method = glmnet (Alpha = 1)

#### Penalized Logistic Regression (L2) (Completed)
* method = glmnet (Alpha = 0)

#### Elastic Net (Completed)
* method = glmnet

#### Multivariate Adaptive Regression Spline (Completed)
* method = earth

#### K-Nearest Neighbor (Completed)
* method = knn

#### Random Forest (Completed)
* method = rf

#### Support Vector Machine (Completed)
* method = svmLinear

#### Extreme Gradient Boosting
* method = XGBoostLinear

#### Convolutional Neural Network
* method = Keras pacakge 

#### Long Short Term Memory Neural Network
* method = Keras package
