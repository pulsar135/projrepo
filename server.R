# Server logic for the shiny web application described in the accompanying ui.R 
# file

library(shiny)
source("predictword.R")

# Define server logic
shinyServer(function(input, output) {
      # Predict possible words to complete the ngram
      wordpred <- reactive({
            validate(need(input$ingram != "", 
                          "Please input a phrase to get predicitions."))
            inphrase <- input$ingram
            predictword(inphrase)
            validate(need(predictword(inphrase) != "Clean Error", 
            "Filter Error: See documentation tab for more info on filtered words."))
      })
      
      # Create output text of the first predicted word
      output$predwrd1 <- renderText({ wordpred()[[1]][1] })
      
      # Create output text of the second predicted word
      output$predwrd2 <- renderText({ wordpred()[[1]][2] })
      
      # Create output text of the first predicted word
      output$predwrd3 <- renderText({ wordpred()[[1]][3] })
})