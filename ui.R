# user interface
# Author: Brian L. Fuller
# Date: 6 April 2016

library(shiny)
shinyUI(pageWithSidebar( 
     headerPanel("Text Prediction Test App"), 
     sidebarPanel( 
          h4('Left bar label'),
          submitButton('Submit')
     ), 
     mainPanel(
          h3('Text Input Area'),
          textInput('textin', 'entered text', value = "", width = NULL, placeholder = "---"),
          h4('Prediction 1'),
          verbatimTextOutput("oPred1"),
          h4('Prediction 2'),
          verbatimTextOutput('oPred2'),
          h4('Prediction 3'),
          verbatimTextOutput('oPred3')
     ) 
))