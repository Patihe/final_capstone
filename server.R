# This is the minimum viable product for the app.
# Add error handling code, and your own enhancements

library(shiny)
source('finalprojectmodel.R')
profanity <- read.csv("profanity.csv", header=FALSE, stringsAsFactors=FALSE)
profanity <- profanity$V1#get column identifier
load('ngram.RData')

shinyServer(function(input, output) {
  dataInput <- reactive({
    if(input$radio == 1){
      predict0(input$entry,profanity,unigramDF,bigramDF,trigramDF,maxResults = input$n)
    }else{
      predict0(input$entry,profanity,unigramDF,bigramDF,trigramDF,maxResults = input$n)#substitute your own input here
    }
  })
  
  output$text <- renderText({
    dataInput()
  })
  
  output$sent <- renderText({
    input$entry
  })
})