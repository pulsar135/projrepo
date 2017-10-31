predictword <- function(phrase, r) {
      
      #Load Libraries
      library('tm')
      library('data.table')
      library('dplyr')
      library('stringr')
      source("kbo_model.R")
      
      #Load 1,2,3grams from saved files
      ng1 <- data.table(readRDS("1grams.RData"))
      ng2 <- data.table(readRDS("2grams.RData"))
      ng3 <- data.table(readRDS("3grams.RData"))
      
      #Set the discount values for bigrams and trigrams
      #Assumed as 0.5 for both for this project
      g2 <- 0.25
      g3 <- 0.5
      
      #Create profanity filter
      homedir <- getwd()
      profanityfile <- paste(homedir, "/profanity.txt", sep = "")
      con <- file(profanityfile, "r")
      profanity <- readLines(con, skipNul = TRUE)
      close(con)
      
      #Clean the input phrase
      phrase <- tolower(phrase)
      phrase <- removeNumbers(phrase)
      phrase <- gsub("won't", "will not", phrase)
      phrase <- gsub("\\S+n't", " not", phrase)
      phrase <- gsub("\\S+'ll", " will", phrase)
      phrase <- gsub("\\S+'re", " are", phrase)
      phrase <- gsub("\\S+'ve", " have", phrase)
      phrase <- gsub("\\S+'m", " am", phrase)
      phrase <- removeWords(phrase, stopwords("english"))
      phrase <- removeWords(phrase, profanity)
      phrase <- gsub("\\S+shit\\S+", "", phrase)
      phrase <- gsub("\\S+shit", "", phrase)
      phrase <- gsub("shit\\S+", "", phrase)
      phrase <- gsub("\\S+fuck\\S+", "", phrase)
      phrase <- gsub("\\S+fuck", "", phrase)
      phrase <- gsub("fuck\\S+", "", phrase)
      phrase <- removePunctuation(phrase)
      #phrase <- stemDocument(phrase)
      phrase <- stripWhitespace(phrase)
      phrase <- str_trim(phrase)
      
      
      #If phrase cleaning removes all words throw error, ask for new input
      if (identical(phrase, "")) {
            results <- "Clean Error"
      }
      
      else {
      
            #Split input phrase and reassemble with "_" between words and get the 
            #number (n) of words in the input phrase
            psplit <- unlist(strsplit(phrase, " "))
            n <- length(psplit)
            sterms <- paste(psplit[n-1], psplit[n], sep = "_")
        
            #Perform analysis using Katz Back-off Model (contained in kbo_model.R) 
            #for trigrams to get prediction of the three highest probability words to
            #complete the input phrase
            obgram3 <- getobgram3(sterms, ng3)
            probobgram3 <- getobgram3prob(obgram3, ng2, sterms, g3)
            uogram3tails <- getuogram3tails(obgram3, ng1)
            gram1 <- str_split(sterms, "_")[[1]][2]
            gram1 <- ng1[ng1$term == gram1,]
            alphagram2 <- getalphagram2(gram1, ng2, g2)
            bogram2 <- getbogram2(sterms, uogram3tails)
            obogram2 <- getobogram2(sterms, uogram3tails, ng2)
            uobogram2 <- getuobogram2(sterms, uogram3tails, obogram2)
            obogram2p <-getobogram2prob(obogram2, ng1, g2)
            uobogram2p <- getuobogram2prob(uobogram2, ng1, alphagram2)
            gram2prob <- rbind(obogram2p, uobogram2p)
            gram2prob
            gram2 <- str_split(sterms, "_")[[1]][1:2]
            gram2 <- ng2[(ng2$termsft %in% gram2[1] & ng2$pterm %in% gram2[2]), ]
            alphagram3 <- getalphagram3(obgram3, gram2, g3)
            uogram3p <- getuogram3prob(sterms, gram2prob, alphagram3)
            
            #Output from KBO summarized into data.table of results with 3 highest 
            #probability words
            probgram3 <- rbind(probobgram3, uogram3p)
            probgram3 <- as.data.table(probgram3 %>% arrange(desc(probgram3$prob)))
            results <- data.table(pred = probgram3$pterm[1:3], 
                                  prob = probgram3$prob[1:3])
            return(results)
      }
}
    
    