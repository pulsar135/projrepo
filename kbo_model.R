#Get Observed Trigrams
#Returns a data.table of trigrams beginning with the last two words of the
#input phrase and their frequencies. Columns(termsft, pterm, freq)
getobgram3 <- function(sterms, ng3) {
      ng3ftr <- as.data.table(filter(ng3, termsft == sterms))
      
      return(ng3ftr)
}

##########################################################################

#Get Observed Trigram Probabilities
#Returns a data.table of trigrams beginning with the last two words of the
#input phrase and their probabilities. Columns(termsft, pterm, prob)
getobgram3prob <- function(obgram3, ng2, sterms, g3) {
      if(nrow(obgram3) < 1) return(NULL)
      term <- str_split(sterms, "_")[[1]][1:2]
      tocnt <- filter(ng2, termsft == term[1], pterm == term[2])$freq[1]
      obgram3 <- as.data.table(obgram3 %>% 
            mutate(prob = ((freq - g3) / tocnt)) %>% select(-3))
      
      return(obgram3)
}

##########################################################################

#Get Unobserved Trigram Tail Words
#Returns a character vector of unigrams that do not appear as tail words in the
#trigram table.
getuogram3tails <- function(obgram3, ng1) {
      obtriwrd <- obgram3$pterm
      uotriwrd <- ng1[!(ng1$term %in% obtriwrd),]$term
      
      return(uotriwrd)
}

##########################################################################

#Get Bigram Alpha (back-off weight)
#Returns a number variable of the back-off weight calculated from the left-over 
#probability of the discounted observed bigrams
getalphagram2 <- function(gram1, ng2, g2) {
      bistartuni <- ng2[grep(gram1$term, ng2$termsft),]
      
      if (nrow(bistartuni) < 1) { return(0) }
      alphabi <- 1 - (sum(bistartuni$freq - g2) / gram1$freq)

      return(alphabi)
}      

##########################################################################

#Get Backed-Off Bigrams
#Returns a data.table of Backed-Off Bigrams where the first term is the tail
#word of the input phrase and the prediction term is the unobserved trigram
#tail words. Columns(termsft, pterm)
getbogram2 <- function(sterms, uogram3tails) {    
      tailwrd <- str_split(sterms, "_")[[1]][2]
      bogram2 <- data.table(termsft = tailwrd, pterm = uogram3tails)
      
      return(bogram2)
}

##########################################################################

#Get Observed Backed-Off Bigrams
#Returns a data.table of the backed-off bigrams which appear in the bigrams
#source table and their frequencies. Columns(termsft, pterm, freq)
getobogram2 <- function(sterms, uogram3tails, ng2) {
      bogram2 <- getbogram2(sterms, uogram3tails)
      obogram2 <- as.data.table(ng2 %>% 
            filter(termsft %in% bogram2$termsft & pterm %in% bogram2$pterm))
      
      return(obogram2)
}

##########################################################################

#Get Unobserved Back-Off Bigrams
#Returns a data.table of unobserved back-off bigrams, which did not appear in
#the bigrams source table. Columns(termsft, pterm)
getuobogram2 <- function(sterms, uogram3tails, obogram2) {
      bogram2 <- getbogram2(sterms, uogram3tails)
      uobogram2 <- bogram2[!(bogram2$termsft %in% obogram2$termsft & bogram2$pterm %in% obogram2$pterm),]
      
      return(uobogram2)
}

##########################################################################

#Get Observed Back-Off Bigram Probabilities
#Returns a data.table of observed back-off bigrams with their probabilities
#included. Columns(termsft, pterm, prob)
getobogram2prob<- function(obogram2, ng1, g2) {
      ftwrds <- obogram2$termsft
      ftwrdsfreq <- ng1[ng1$term %in% ftwrds, ]
      obogram2p <- (obogram2$freq - g2) / ftwrdsfreq$freq
      obogram2p <- data.table(termsft = obogram2$termsft, 
                              pterm = obogram2$pterm, prob = obogram2p)
      
      return(obogram2p)
}

##########################################################################

#Get Unobserved Back-off Bigram Probabilities
#Returns a data.table of unobserved back-off bigrams with their probabilities
#included. Columns(termsft, pterm, prob)
getuobogram2prob <- function(uobogram2, ng1, alphagram2) {
      uobogram2p <- uobogram2$pterm
      uobogram2p <- ng1[ng1$term %in% uobogram2p, ]
      uobogram2p <- data.table(termsft = uobogram2$termsft, pterm = uobogram2$pterm, 
                                prob = (alphagram2 * uobogram2p$freq / sum(uobogram2p$freq)))
      
      return(uobogram2p)
}

##########################################################################

#Get Trigram Alpha (back-off weight)
#Returns a number variable of the back-off weight calculated from the left-over 
#probability of the discounted observed trigrams
getalphagram3 <- function(obgram3, gram2, g3) {
      if (nrow(obgram3) < 1) return(1)
      alphatri <- 1 - sum((obgram3$freq - g3) / gram2$freq[1])
      
      return(alphatri)
}

##########################################################################

#Get Unobserved Trigram Probabilities
#Return a data.table of unobserved trigrams that start with the input phrase,
#but were not present in the trigrams source table, and their probabilities.
#Columns(termsft, pterm, prob)
getuogram3prob <- function(sterms, gram2prob, alphagram3) {
      gram2prob <- as.data.table(gram2prob %>% arrange(desc(prob)))
      sumgram2p <- sum(gram2prob$prob)
      sterms1 <- str_split(sterms, "_")[[1]][1]
      uogram3 <- paste(sterms1, gram2prob$termsft, sep = "_")
      uogram3prob <- alphagram3 * gram2prob$prob / sumgram2p
      uogram3_dt <- data.table(termsft = uogram3, pterm = gram2prob$pterm, 
                               prob = uogram3prob)
      
      return(uogram3_dt)
}