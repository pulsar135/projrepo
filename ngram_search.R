bigram_search <- function(ng2, psplit) {
      ng2ftr <- filter(ng2, term1 == psplit[1])
      
      if (nrow(ng2ftr) == 0) {nextwrd <- "No Prediction Available"}
      
      else if (nrow(ng2ftr) > 0 & nrow(ng2ftr) < 3) {
            nextwrd <- vector(l = nrow(ng2ftr))
      } 
      
      else {nextwrd <- vector(l = 3)}
      
      if (nrow(ng2ftr) > 0) {
            for (j in 1:length(nextwrd)) {nextwrd[j] <- ng2ftr[j,2]}
      }
      
      nextwrd
}


trigram_search <- function(ng3, psplit) {
      psterms <- paste(psplit[1], "_", psplit[2], sep = "")
      
      ng3ftr <- as.data.table(filter(ng3, terms12 == psterms))
      
      if (nrow(ng3ftr) == 0) {nextwrd <- "No Prediction Available"}
      
      else if (nrow(ng3ftr) > 0 & nrow(ng3ftr) < 3) {
            nextwrd <- vector(l = nrow(ng3ftr))
      } 
      
      else {nextwrd <- vector(l = 3)}
      
      if (nrow(ng3ftr) > 0) {
            for (j in 1:length(nextwrd)) {nextwrd[j] <- ng3ftr[j,3]}
      }
      
      nextwrd
}


quadgram_search <- function(ng4, ng4_lob, psplit) {
      psterms3 <- paste(psplit[1], "_", psplit[2], "_", psplit[3], sep = "")
      
      ng4ftr <- as.data.table(filter(ng4, terms13 == psterms3))
      
      #if (nrow(ng4ftr) == 0) {nextwrd <- "No Prediction Available"}
      
      if (nrow(ng4ftr) > 0) { 
            if (nrow(ng4ftr) > 0 & nrow(ng4ftr) < 3) {
                  nextwrd <- vector(l = nrow(ng4ftr))
            } 
            else {
                  nextwrd <- vector(l = 3)
            }
      
            allfreq <- sum(ng4ftr$term_freq)
            
            ng4ftr <- ng4ftr[, finalprob := (discount * term_freq)/allfreq]
            
            ng4ftr <- as.data.table(arrange(ng4ftr, desc(finalprob)))
            
            for (j in 1:length(nextwrd)) {nextwrd[j] <- ng4ftr[j,2]}
      }
      else {
            psterms2 <- paste(psplit[2], "_", psplit[3], sep = "")
            
            ng3ftr <- as.data.table(filter(ng3, terms12 == psterms2))
            
            betalob <- ng4_lob[terms13 == psterms3]$probremain
            
            if (nrow(ng3ftr) > 0) {
                  
                  if (nrow(ng3ftr) > 0 & nrow(ng3ftr) < 3) {
                        nextwrd <- vector(l = nrow(ng3ftr))
                  } 
                  else {
                        nextwrd <- vector(l = 3)
                  }
                  
                  allfreq <- sum(ng3ftr$term_freq)
                  
                  alpha <- betalob/sum((ng3ftr$term_freq * ng3ftr$discount) / allfreq)
                  
                  ng3ftr <- ng3ftr[, finalprob := alpha * ((discount * term_freq)/allfreq)]
                  
                  ng3ftr <- as.data.table(arrange(ng3ftr, desc(finalprob)))
                  
                  for (j in 1:length(nextwrd)) {nextwrd[j] <- ng3ftr[j,2]}  
            }
      }
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      return(nextwrd)
}