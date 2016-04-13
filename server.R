# Author: Brian L. Fuller
# Date: 6 April 2016

library(shiny)

bpredict <- function(str_in)
{
     # TODO: need to fix null case
     btoks <- btokens(str_in)
     ngs <- ngp(btoks)
     
     idx <- ngs[ngs>0][length(ngs[ngs>0])]
     
     jdx <- which(ngs == idx)
     
     if(length(jdx) == 0)
     {
          pred <- sources[[1]]$phrase[1]
     }
     else
     {
          pred <- sources[[jdx+1]]$righty[idx]
     }
     
     return(paste(str_in, "...", pred))
}


#my_prediction <- function(inputphrase) c(inputphrase, inputphrase, inputphrase)

shinyServer(
     function(input, output) {
          
          MyPredictedWords <- reactive({bpredict(input$textin)})
          
#           p1 <- PredictedWords()[1]
#           p2 <- PredictedWords()[2]
#           p3 <- PredictedWords()[3]
          
          
          output$oPred1 <- renderText({MyPredictedWords()})
          #output$oPred2 <- renderText({MyPredictedWords()[2]})
          #output$oPred3 <- renderText({MyPredictedWords()[3]})
     }
)