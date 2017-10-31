predictword <- function(phrase) {
      
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
      ng4 <- data.table(readRDS("4grams.RData"))
      
      #Set the discount values for bigrams and trigrams
      #Assumed as 0.5 for both for this project
      g2 <- 0.5
      g3 <- 0.5
      g4 <- 0.5
      
      ##Create profanity filter
      #homedir <- getwd()
      #profanityfile <- paste(homedir, "/profanity.txt", sep = "")
      #con <- file(profanityfile, "r")
      #profanity <- readLines(con, skipNul = TRUE)
      #close(con)
      # 
      ##Clean the input phrase
      #phrase <- tolower(phrase)
      #phrase <- removeNumbers(phrase)
      #phrase <- gsub("won't", "will not", phrase)
      #phrase <- gsub("\\S+n't", " not", phrase)
      #phrase <- gsub("\\S+'ll", " will", phrase)
      #phrase <- gsub("\\S+'re", " are", phrase)
      #phrase <- gsub("\\S+'ve", " have", phrase)
      #phrase <- gsub("\\S+'m", " am", phrase)
      #phrase <- removeWords(phrase, stopwords("english"))
      #phrase <- removeWords(phrase, profanity)
      #phrase <- gsub("\\S+shit\\S+", "", phrase)
      #phrase <- gsub("\\S+shit", "", phrase)
      #phrase <- gsub("shit\\S+", "", phrase)
      #phrase <- gsub("\\S+fuck\\S+", "", phrase)
      #phrase <- gsub("\\S+fuck", "", phrase)
      #phrase <- gsub("fuck\\S+", "", phrase)
      #phrase <- removePunctuation(phrase)
      ##phrase <- stemDocument(phrase)
      #phrase <- stripWhitespace(phrase)
      #phrase <- str_trim(phrase)
      
      #If phrase cleaning removes all words throw error, ask for new input
      if (identical(phrase, "")) {
            results <- data.table(pred = rep("Filter Error", 3), 
                                  prob = rep(0, 3))
      }
      
      else {
            
            #Split input phrase and reassemble with "_" between words and get the 
            #number (n) of words in the input phrase
            psplit <- unlist(strsplit(phrase, " "))
            n <- length(psplit)
            sterms <- paste(psplit[n-2], psplit[n-1], psplit[n], sep = "_")
            
            #Perform analysis using Katz Back-off Model (contained in kbo_model.R) 
            #for quadgrams to get prediction of the three highest probability 
            #words to complete the input phrase
            obgram4 <- getobgram4(sterms, ng4)
            probobgram4 <- getobgram4prob(obgram4, ng3, sterms, g4)
            uogram4tails <- getuogram4tails(obgram4, ng1)
            gram2 <- str_split(sterms, "_")[[1]][1:3]
            gram2 <- ng2[(ng2$termsft %in% gram2[2] & ng2$pterm %in% gram2[3]), ]
            alphagram3 <- getalphagram3(gram2, ng3, g3)
            bogram3 <- getbogram3(sterms, uogram4tails)
            obogram3 <- getobogram3(sterms, uogram4tails, ng3)
            uobogram3 <- getuobogram3(sterms, uogram4tails, obogram3)
            obogram3p <-getobogram3prob(obogram3, ng2, g3)
            uobogram3p <- getuobogram3prob(uobogram3, ng1, alphagram3)
            #gram3prob <- rbind(obogram3p, uobogram3p)
            gram3prob <- obogram3p
            gram3prob
            
            gram1 <- str_split(sterms, "_")[[1]][3]
            gram1 <- ng1[ng1$term == gram1,]
            alphagram2 <- getalphagram2(gram1, ng2, g2)
            uogram4ubo3tails <- getuogram4ubo3tails(uogram4tails, obogram3)
            bogram2 <- getbogram2(sterms, uogram4ubo3tails)
            obogram2 <- getobogram2(sterms, uogram4ubo3tails, ng2)
            uobogram2 <- getuobogram2(sterms, uogram4ubo3tails, obogram2)
            obogram2p <-getobogram2prob(obogram2, ng1, g2)
            uobogram2p <- getuobogram2prob(uobogram2, ng1, alphagram2)
            gram2prob <- rbind(obogram2p, uobogram2p)
            gram2prob
            
            gram3 <- str_split(sterms, "_")[[1]][1:3]
            gram3 <- c(paste(gram3[1], gram3[2], sep = "_"), gram3[3])
            gram3 <- ng3[(ng3$termsft %in% gram3[1] & ng3$pterm %in% gram3[2]), ]
            alphagram4 <- getalphagram4(obgram4, gram3, g4)
            uogram4p <- getuogram4prob(sterms, gram2prob, gram3prob, alphagram4)
            
            
            #Output from KBO summarized into data.table of results with 3 highest 
            #probability words
            probgram4 <- rbind(probobgram4, uogram4p)
            probgram4 <- as.data.table(probgram4 %>% arrange(desc(probgram4$prob)))
            results <- data.table(pred = probgram4$pterm[1:3], 
                                  prob = probgram4$prob[1:3])
            return(results)
      }
}