# This is the minimum viable product for the app.
# Add error handling code, and your own enhancements

library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",#get more bootstrap themes from http://bootswatch.com/
                  # Set the page title
                  titlePanel("Data Science Capstone - SwiftKey Predictor Using Back-Off Good-Turing"),
                  
                  sidebarPanel(
                    textInput("entry",
                              h5("Input the sentence"),
                              "You"),
                    numericInput("n",
                                 h5("Numbers of predicted words"), 
                                 value = 2),
                    radioButtons("radio", 
                                 h5("Smoothing selection"),
                                 choices = list("Simple Back-off Model" = 1),
                                 selected = 1),
                    submitButton("SUBMIT"),
                    br()
                    
                  ),
                  
                  mainPanel(
                    tabsetPanel(type = "tabs", 
                                tabPanel("Instruction", 
                                         h4("Usage"),
                                         p("You can input a sentence in the topleft panel, and try the method for the n-gram model. Then press SUBMIT. You will see:"),
                                         span(h4(textOutput('sent')),style = "color:blue"),
                                         p('as the input and'),
                                         span(h4(textOutput('text')),style = "color:blue"),
                                         p('as predicted words OR Please Enter Text if the sentence field is missing') 
                                                   )
                                
                    )
                  ))
)