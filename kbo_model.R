#Get Observed Quadgrams
#Returns a data.table of quadgrams beginning with the last three words of the
#input phrase and their frequencies. Columns(termsft, pterm, freq)
getobgram4 <- function(sterms, ng4) {
      ng4ftr <- as.data.table(filter(ng4, termsft == sterms))
      
      return(ng4ftr)
}

##########################################################################

#Get Observed Quadgram Probabilities
#Returns a data.table of quadgrams beginning with the last three words of the
#input phrase and their probabilities. Columns(termsft, pterm, prob)
getobgram4prob <- function(obgram4, ng3, sterms, g4) {
      if(nrow(obgram4) < 1) return(NULL)
      term <- str_split(sterms, "_")[[1]][1:3]
      fterms <- paste(term[1], term[2], sep = "_")
      tocnt <- filter(ng3, termsft == fterms, pterm == term[3])$freq[1]
      obgram4 <- as.data.table(obgram4 %>% 
                                     mutate(prob = ((freq - g4) / tocnt)) %>% select(-3))
      
      return(obgram4)
}

##########################################################################

#Get Unobserved Quadgram Tail Words
#Returns a character vector of unigrams that do not appear as tail words in the
#quadgram table.
getuogram4tails <- function(obgram4, ng1) {
      obquadwrd <- obgram4$pterm
      uoquadwrd <- ng1[!(ng1$term %in% obquadwrd),]$term
      
      return(uoquadwrd)
}

##########################################################################

#Get Trigram Alpha (back-off weight)
#Returns a number variable of the back-off weight calculated from the left-over 
#probability of the discounted observed trigrams
getalphagram3 <- function(gram2, ng3, g3) {
      if (nrow(gram2) < 1) return(0)
      gram2terms <- paste(gram2$termsft, gram2$pterm, sep = "_")
      tristartbi <- ng3[grep(gram2terms, ng3$termsft),]
      
      #if (nrow(tristartbi) < 1) return(0)
      alphatri <- 1 - (sum(tristartbi$freq - g3) / gram2$freq)
      
      return(alphatri)
}      

##########################################################################

#Get Backed-Off Trigrams
#Returns a data.table of Backed-Off Trigrams where the first terms is the last 
#two tail words of the input phrase and the prediction term is the unobserved 
#quadgram tail words. Columns(termsft, pterm)
getbogram3 <- function(sterms, uogram4tails) {    
      tailwrd <- str_split(sterms, "_")[[1]][1:3]
      tailwrd <- paste(tailwrd[2], tailwrd[3], sep = "_")
      bogram3 <- data.table(termsft = tailwrd, pterm = uogram4tails)
      
      return(bogram3)
}

##########################################################################

#Get Observed Backed-Off Trigrams
#Returns a data.table of the backed-off trigrams which appear in the trigrams
#source table and their frequencies. Columns(termsft, pterm, freq)
getobogram3 <- function(sterms, uogram4tails, ng3) {
      bogram3 <- getbogram3(sterms, uogram4tails)
      obogram3 <- as.data.table(ng3 %>% 
                                      filter(termsft %in% bogram3$termsft & pterm %in% bogram3$pterm))
      
      return(obogram3)
}

##########################################################################

#Get Unobserved Back-Off Trigrams
#Returns a data.table of unobserved back-off trigrams, which did not appear in
#the trigrams source table. Columns(termsft, pterm)
getuobogram3 <- function(sterms, uogram4tails, obogram3) {
      bogram3 <- getbogram3(sterms, uogram4tails)
      uobogram3 <- bogram3[!(bogram3$termsft %in% obogram3$termsft & bogram3$pterm %in% obogram3$pterm),]
      
      return(uobogram3)
}

##########################################################################

#Get Observed Back-Off Trigram Probabilities
#Returns a data.table of observed back-off trigrams with their probabilities
#included. Columns(termsft, pterm, prob)
getobogram3prob<- function(obogram3, ng2, g3) {
      ftwrds <- obogram3$termsft
      ftwrds <- str_split_fixed(ftwrds, "_", 2)[,1:2]
      ftwrdsfreq <- ng2[(ng2$termsft %in% ftwrds[,1] & ng2$pterm %in% ftwrds[,2]), ]
      obogram3p <- (obogram3$freq - g3) / ftwrdsfreq$freq
      obogram3p <- data.table(termsft = obogram3$termsft, 
                              pterm = obogram3$pterm, prob = obogram3p)
      
      return(obogram3p)
}

##########################################################################

#Get Unobserved Back-off Trigram Probabilities
#Returns a data.table of unobserved back-off trigrams with their probabilities
#included. Columns(termsft, pterm, prob)
getuobogram3prob <- function(uobogram3, ng1, alphagram3) {
      uobogram3p <- uobogram3$pterm
      uobogram3p <- ng1[ng1$term %in% uobogram3p, ]
      uobogram3p <- data.table(termsft = uobogram3$termsft, pterm = uobogram3$pterm, 
                               prob = (alphagram3 * uobogram3p$freq / sum(uobogram3p$freq)))
      
      return(uobogram3p)
}

##########################################################################

#Get Bigram Alpha (back-off weight)
#Returns a number variable of the back-off weight calculated from the left-over 
#probability of the discounted observed bigrams
getalphagram2 <- function(gram1, ng2, g2) {
      bistartuni <- ng2[grep(gram1$term, ng2$termsft),]
      
      if (nrow(bistartuni) < 1) return(0)
      alphabi <- 1 - (sum(bistartuni$freq - g2) / gram1$freq)
      
      return(alphabi)
}

##########################################################################

#Get Unobserved Quadgram of Unobserved Back-Off Trigram Tails
#Returns a character vector of unigrams that do not appear as tail words in the
#trigram source table for the back-off trigrams.
getuogram4ubo3tails <- function(uogram4tails, obogram3) {
      obotriwrd <- obogram3$pterm
      ubotriwrd <- uogram4tails[!(uogram4tails %in% obotriwrd)]
      
      return(ubotriwrd)
}

##########################################################################

#Get Backed-Off Bigrams
#Returns a data.table of Backed-Off Bigrams where the first term is the tail
#word of the input phrase and the prediction term is the unobserved quadgram
#tail words. Columns(termsft, pterm)
getbogram2 <- function(sterms, uogram4ubo3tails) {    
      tailwrd <- str_split(sterms, "_")[[1]][3]
      bogram2 <- data.table(termsft = tailwrd, pterm = uogram4ubo3tails)
      
      return(bogram2)
}

##########################################################################

#Get Observed Backed-Off Bigrams
#Returns a data.table of the backed-off bigrams which appear in the bigrams
#source table and their frequencies. Columns(termsft, pterm, freq)
getobogram2 <- function(sterms, uogram4ubo3tails, ng2) {
      bogram2 <- getbogram2(sterms, uogram4ubo3tails)
      obogram2 <- as.data.table(ng2 %>% 
                                      filter(termsft %in% bogram2$termsft & pterm %in% bogram2$pterm))
      
      return(obogram2)
}

##########################################################################

#Get Unobserved Back-Off Bigrams
#Returns a data.table of unobserved back-off bigrams, which did not appear in
#the bigrams source table. Columns(termsft, pterm)
getuobogram2 <- function(sterms, uogram4ubo3tails, obogram2) {
      bogram2 <- getbogram2(sterms, uogram4ubo3tails)
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

#Get Quadgram Alpha (back-off weight)
#Returns a number variable of the back-off weight calculated from the left-over 
#probability of the discounted observed quadgrams
getalphagram4 <- function(obgram4, gram3, g4) {
      if (nrow(obgram4) < 1) return(1)
      alphaquad <- 1 - sum((obgram4$freq - g4) / gram3$freq[1])
      
      return(alphaquad)
}

##########################################################################

#Get Unobserved Quadgram Probabilities
#Return a data.table of unobserved quadgrams that start with the input phrase,
#but were not present in the quadgrams source table, and their probabilities.
#Columns(termsft, pterm, prob)
getuogram4prob <- function(sterms, gram2prob, gram3prob, alphagram4) {
      if (nrow(gram3prob) < 1) {
            uogram43_dt <- data.table(termsft = character(), 
                                      pterm = character(), prob = numeric())
      }
      else {
            gram3prob <- as.data.table(gram3prob %>% arrange(desc(prob)))
            sumgram3p <- sum(gram3prob$prob)
            sterms1 <- str_split(sterms, "_")[[1]][1]
            #uogram43 <- paste(sterms1, gram3prob$termsft, sep = "_")
            uogram43 <- paste(sterms1, gram3prob$termsft, sep = "_")
            uogram43prob <- alphagram4 * gram3prob$prob / sumgram3p
            uogram43_dt <- data.table(termsft = uogram43, 
                                      pterm = gram3prob$pterm, 
                                      prob = uogram43prob)
      }
      
      gram2prob <- as.data.table(gram2prob %>% arrange(desc(prob)))
      sumgram2p <- sum(gram2prob$prob)
      sterms12 <- str_split(sterms, "_")[[1]][1:3]
      sterms12 <- paste(sterms12[1], sterms12[2], sep = "_")
      uogram42 <- paste(sterms12, gram2prob$termsft, sep = "_")
      uogram42prob <- alphagram4 * gram2prob$prob / sumgram2p
      uogram42_dt <- data.table(termsft = uogram42, pterm = gram2prob$pterm, 
                                prob = uogram42prob)
      
      uogram4_dt <- rbind(uogram43_dt, uogram42_dt)
      uogram4_dt <- as.data.table(uogram4_dt %>% arrange(desc(prob)))
      
      
      return(uogram4_dt)
}