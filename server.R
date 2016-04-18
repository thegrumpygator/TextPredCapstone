# Author: Brian L. Fuller
# Date: 6 April 2016

library(shiny)


shinyServer(function(input, output) {
     output$oTable <- renderTable({
          reactive(bmerge(input$textin))() 
     })
     observe({
          # run whenever reset button is pressed
          input$reset
     })
})