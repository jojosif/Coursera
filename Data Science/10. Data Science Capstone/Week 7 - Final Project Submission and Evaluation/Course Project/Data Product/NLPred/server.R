library(shiny)
source('prediction.R')
load('cleanCorpus.RData')

shinyServer(function(input, output) {
    final.prediction <- reactive({
        pr <- prediction.sb(w1, w2, w3, input$words, 3)
        paste("[1]", pr[1], "; [2]", pr[2], "; [3]", pr[3])
    })
    
    output$text <- renderText({
        final.prediction()
    })
})