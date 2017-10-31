create_ngrams<-function(){

      #Load Libraries
      library('tm')
      library('SnowballC')
      library('RWeka')
      library('purrr')
      library('data.table')
      library('dplyr')
      source("ngramdf.R")
      
      #Create directory/filename variables
      homedir <- getwd()
      corpdir <- paste(homedir, "/corpora", sep = "")
      trainfile <- paste(corpdir, "/train.txt", sep = "")
      #trainfile <- paste(corpdir, "/little_test_corpus.txt", sep = "")
      
      
      #Create training corpus
      usource <- URISource(trainfile, encoding = "UTF-8", "text")
      (traincorp <- VCorpus(usource, readerControl = list(language = "en")))
      
      #Create dataframe of terms and term frequencies from TDM variables
      #For 2,3,4grams the starting terms will be in one column and the prediction 
      #term in a separate column
      #Save ngram data.table to an .RData file for later use
      
      
      #Unigrams
      stdm1_df <- getgram1dt(traincorp)
      
      saveRDS(stdm1_df, "1grams.RData")
      rm(list = c("stdm1_df"))
      
      
      #Bigrams
      stdm2_df <- getgram2dt(traincorp)
      
      saveRDS(stdm2_df, "2grams.RData")
      rm(list = c("stdm2_df"))
      
      #Trigrams
      stdm3_df <- getgram3dt(traincorp)
      
      saveRDS(stdm3_df, "3grams.RData")
      rm(list = c("stdm3_df"))
      
      #Quadgrams
      stdm4_df <- getgram4dt(traincorp)
      
      
      saveRDS(stdm4_df, "4grams.RData")
      rm(list = c("stdm4_df"))
}