library(shiny)
shinyUI(fluidPage(
  textInput("text", label = h2("Next Word Predictor"), value = "", width ="100%", placeholder = "Enter your text"),
  #actionButton(inputId="findNextButton", label = "Find next.."),
  fluidRow(
   
    verbatimTextOutput("dataset"),
    
    radioButtons("options", "Your choice",
                 c("Word 1" = "value1",
                   "Word 2" = "value2",
                   "Word 3" = "value3",
                   "Word 4" = "value4",
                   "Word 5" = "value5"), inline = TRUE),
  actionButton(inputId="findNextButton", label = "Find next.."),
  actionButton(inputId="addButton", label = "Add the word to your input"),
  actionButton(inputId="clearButton", label = "Clear")
  
 
  
)))
