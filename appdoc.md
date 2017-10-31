# NLP: Ngram Prediction Model Application Documentation

## Application Overview
This application uses the Katz Back-off (KBO) Model with discounting for quadgrams 
to provide the top three words predicted to complete the user input phrase (ngram).
A list of resources on KBO Model, Natural Language Processing, and Text Mining
in R can be found on github in the [resources list][1]

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
list of words used for these filters check: [profanity] [2] or [stopwords] [3].

## Application Files Github Location
The files for this application can be found on github [here] [4]

[1]: https://github.com/pulsar135/projrepo/blob/master/resources.txt "resources list"
[2]: https://github.com/pulsar135/projrepo/blob/master/profanity.txt "profanity"
[3]: https://github.com/pulsar135/projrepo/blob/master/stopwords.txt "stopwords"
[4]: https://github.com/pulsar135/projrepo "here"
