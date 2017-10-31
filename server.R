# Server logic for the shiny web application described in the accompanying ui.R 
# file

library(shiny)
library('tm')
library('dplyr')
library('stringr')
source("predictword.R")

# Define server logic
shinyServer(function(input, output) {
      # Predict possible words to complete the ngram
      wordpred <- reactive({
            validate(need(input$ingram != "", 
                          "Please input a phrase to get predicitions."))
            inphrase <- input$ingram
            
            #Create profanity filter
            homedir <- getwd()
            profanityfile <- paste(homedir, "/profanity.txt", sep = "")
            con <- file(profanityfile, "r")
            profanity <- readLines(con, skipNul = TRUE)
            close(con)
            
            #Clean the input phrase
            inphrase <- tolower(inphrase)
            inphrase <- removeNumbers(inphrase)
            inphrase <- gsub("won't", "will not", inphrase)
            inphrase <- gsub("\\S+n't", " not", inphrase)
            inphrase <- gsub("\\S+'ll", " will", inphrase)
            inphrase <- gsub("\\S+'re", " are", inphrase)
            inphrase <- gsub("\\S+'ve", " have", inphrase)
            inphrase <- gsub("\\S+'m", " am", inphrase)
            inphrase <- removeWords(inphrase, stopwords("english"))
            inphrase <- removeWords(inphrase, profanity)
            inphrase <- gsub("\\S+shit\\S+", "", inphrase)
            inphrase <- gsub("\\S+shit", "", inphrase)
            inphrase <- gsub("shit\\S+", "", inphrase)
            inphrase <- gsub("\\S+fuck\\S+", "", inphrase)
            inphrase <- gsub("\\S+fuck", "", inphrase)
            inphrase <- gsub("fuck\\S+", "", inphrase)
            inphrase <- removePunctuation(inphrase)
            inphrase <- stripWhitespace(inphrase)
            inphrase <- str_trim(inphrase)
            
            #If all words filtered out set inphrase = filter error message
            if (identical(inphrase, "")) {
                  inphrase <- "Filter Error"
            }
            
            #validate that all words have not been filtered out
            validate(need(inphrase != "Filter Error", "Filter Error: See documentation tab for more info on filtered words."))
            
            #predict next word algorithm
            predictword(inphrase)
            
      })
      
      # Create output text of the first predicted word
      output$predwrd1 <- renderText({ wordpred()[[1]][1] })
      
      # Create output text of the second predicted word
      output$predwrd2 <- renderText({ wordpred()[[1]][2] })
      
      # Create output text of the first predicted word
      output$predwrd3 <- renderText({ wordpred()[[1]][3] })
})