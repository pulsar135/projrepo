# Capstone Project: Natural Language Processing Ngram Prediction
# This application uses the Rscripts created for the capstone to take an ngram
# as an input and provide a prediction of the top three choices of the next word
# in the ngram

# Load Libraries
library(shiny)
library(markdown)

# Define UI for application that takes an ngram as input and predicts the top
# three words to complete the ngrame
shinyUI(fluidPage(
      
      # Application title
      titlePanel("NLP: Ngram Prediction Model"),
      
      # Sidebar with a text input for the ngram to be completed and a sumbit 
      # button to submit the ngram for prediction when ready
      sidebarLayout(
            sidebarPanel(
                  h3("Ngram Prediction Using Quadgram KBO Model"),
                  textInput("ingram", "Enter a phrase to be completed:", value = "Have a nice"),
                  submitButton("Submit")
            ),
      
            # Main output panel
            mainPanel(
                  tabsetPanel(type = "tabs",
                        
                              # Show output of the top three word predictions to
                              # complete the input ngram
                              tabPanel(
                                    "Application",
                              
                                    h3("1st Prediction Word:"),
                                    textOutput("predwrd1"),
                              
                                    h3("2nd Prediction Word:"),
                                    textOutput("predwrd2"),
                              
                                    h3("3rd Prediction Word:"),
                                    textOutput("predwrd3")
                              ),
                        
                              # Documentation for how the app works
                              tabPanel(
                                    "Documentation",
                                    mainPanel( includeMarkdown("appdoc.md")
                                    )
                              )
                  )
            )
      )
))