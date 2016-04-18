# user interface
# Author: Brian L. Fuller
# Date: 6 April 2016

library(shiny)


shinyUI(fluidPage(
     titlePanel("Text Prediction App"),
     
     fluidRow(
          column(4, wellPanel(
               textInput('textin', "Your Text: ")#,
               #actionButton("reset", "Reset Text")
          )),
          column(8, wellPanel(
               tableOutput("oTable")
          ))
     )
))