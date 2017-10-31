# NLP: Ngram Prediction Model Application Documentation

## Application Overview
This application uses the Katz Back-off (KBO) Model with discounting for quadgrams 
to provide the top three words predicted to complete the user input phrase (ngram).

## Discount values
For the purposes of this simplified application the discount was assumed to be
a value of 0.5 for bigrams, trigrams, and quadgrams.

## User Inputs
The user inputs a phrase for which they would like the last word to be predicted
or the beginning of a phrase for which they would like the next word predicted.

## Application Outputs
The application outputs the three words with the highest probability of being the
next word in the phrase based on the KBO Model.  The first word has the highest
probability, the second word the second highest, and the third word the third
highest.

## Word Filtering
The predicition algorithm cleans the input phrase before applying the model, 
removing profanity and english stopwords. If a "filter error" is received then
all words in the input phrase have been filtered out.  If you wish to see the
list of words used for these filters check: profanity or stopwords.

## Application Files Github Location
https://github.com/pulsar135/ddpweek4project/tree/master/week4project