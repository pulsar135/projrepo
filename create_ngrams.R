create_ngrams<-function(){

      #Load Libraries
      library('tm')
      library('SnowballC')
      library('RWeka')
      library('purrr')
      library('data.table')
      library('dplyr')
      
      
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
      
      #Unigram Tokenizer
      Tgram1 <- function(n) NGramTokenizer(n, Weka_control(min = 1, max = 1))
      #Unigram TermDocumentMatrix
      samtdm1 <- TermDocumentMatrix(traincorp, control = list(wordLengths = c(1,Inf), tokenize = Tgram1))
      samtdm1 <- removeSparseTerms(samtdm1, 0.97)
      #Unigram data.frame
      stdm1_df <- data.table(term=rownames(as.matrix(samtdm1)), 
                             freq=rowSums(as.matrix(samtdm1)))
      stdm1_df <- as.data.table(stdm1_df %>% arrange(term, desc(freq)))
      stdm1_df <- stdm1_df[stdm1_df$freq > 1,]
      
      saveRDS(stdm1_df, "1grams.RData")
      rm(list = c("samtdm1", "stdm1_df"))
      
      
      #Bigrams
      
      #Bigram Tokenizer
      Tgram2 <- function(n) NGramTokenizer(n, Weka_control(min = 2, max = 2))
      #Bigram TermDocumentMatrix
      samtdm2 <- TermDocumentMatrix(traincorp, control = list(wordLengths = c(1,Inf), tokenize = Tgram2))
      samtdm2 <- removeSparseTerms(samtdm2, 0.97)
      #Bigram data.frame
      stdm2_df <- data.table(term=rownames(as.matrix(samtdm2)), 
                             freq=rowSums(as.matrix(samtdm2)))
      stdm2_df <- stdm2_df[stdm2_df$freq > 1,]
      tsplit <- strsplit(stdm2_df$term, " ")
      termsft <- map_chr(tsplit, 1)
      pterm <- map_chr(tsplit, 2)
      stdm2_df <- cbind(termsft, pterm, stdm2_df)
      stdm2_df <- as.data.table(stdm2_df %>% 
            arrange(termsft, pterm, desc(freq)) %>% select(-3))
      
      saveRDS(stdm2_df, "2grams.RData")
      rm(list = c("samtdm2", "stdm2_df"))

      
      #Trigrams
      
      #Trigram Tokenizer
      Tgram3 <- function(n) NGramTokenizer(n, Weka_control(min = 3, max = 3))
      #Trigram TermDocumentMatrix
      samtdm3 <- TermDocumentMatrix(traincorp, control = list(wordLengths = c(1,Inf), tokenize = Tgram3))
      samtdm3 <- removeSparseTerms(samtdm3, 0.97)
      #Trigram data.frame
      stdm3_df <- data.table(term=rownames(as.matrix(samtdm3)), 
                             freq=rowSums(as.matrix(samtdm3)))
      stdm3_df <- stdm3_df[stdm3_df$freq > 1,]
      tsplit <- strsplit(stdm3_df$term, " ")
      term1 <- map_chr(tsplit, 1)
      term2 <- map_chr(tsplit, 2)
      pterm <- map_chr(tsplit, 3)
      termsft <- paste(term1, term2, sep = "_")
      stdm3_df <- cbind(termsft, pterm, stdm3_df)
      stdm3_df <- as.data.table(stdm3_df %>% 
            arrange(termsft, pterm, desc(freq)) %>% select(-3))
      
      saveRDS(stdm3_df, "3grams.RData")
      rm(list = c("samtdm3", "stdm3_df"))
      
      
      #Quadgrams
      
      #Quadgram Tokenizer
      Tgram4 <- function(n) NGramTokenizer(n, Weka_control(min = 4, max = 4))
      #Quadgram TermDocumentMatrix
      samtdm4 <- TermDocumentMatrix(traincorp, control = list(wordLengths = c(1,Inf), tokenize = Tgram4))
      samtdm4 <- removeSparseTerms(samtdm4, 0.97)
      #Quadgram data.frame
      stdm4_df <- data.table(term=rownames(as.matrix(samtdm4)), 
                             freq=rowSums(as.matrix(samtdm4)))
      stdm4_df <- stdm4_df[stdm4_df$freq > 1,]
      tsplit <- strsplit(stdm4_df$term, " ")
      term1 <- map_chr(tsplit, 1)
      term2 <- map_chr(tsplit, 2)
      term3 <- map_chr(tsplit, 3)
      pterm <- map_chr(tsplit, 4)
      termsft <- paste(term1, term2, term3, sep = "_")
      stdm4_df <- cbind(termsft, pterm, stdm4_df)
      stdm4_df <- as.data.table(stdm4_df %>% 
                                      arrange(termsft, pterm, desc(freq)) %>% select(-3))
      
      saveRDS(stdm4_df, "4grams.RData")
      rm(list = c("samtdm4", "stdm4_df"))
}