# Author: Brian L. Fuller
# Date: 6 April 2016

library(shiny)

my_prediction <- function(inputphrase) c(inputphrase, inputphrase, inputphrase)

shinyServer(
     function(input, output) {
          
          MyPredictedWords <- reactive({my_prediction(input$textin)})
          
#           p1 <- PredictedWords()[1]
#           p2 <- PredictedWords()[2]
#           p3 <- PredictedWords()[3]
          
          
          output$oPred1 <- renderText({MyPredictedWords()[1]})
          output$oPred2 <- renderText({MyPredictedWords()[2]})
          output$oPred3 <- renderText({MyPredictedWords()[3]})
     }
)