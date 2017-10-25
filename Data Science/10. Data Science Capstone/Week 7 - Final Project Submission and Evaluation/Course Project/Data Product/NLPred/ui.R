library(shiny)
library(shinythemes)
library(markdown)

shinyUI(
    fluidPage(
        theme = shinytheme("flatly"),
        
        pageWithSidebar(
            # Application title
            titlePanel("Natural Language Predictor"),
            
            # Sidebar with a input text field
            sidebarPanel(
                textInput("words",
                          h4("Input something:"),
                          "you"),
                helpText("Only English words are supported."),
                submitButton("Predict Next")
            ),
            
            mainPanel(
                tabsetPanel(
                    tabPanel("Next Word Prediction",
                             p(''),
                             p('Type in a sentence in the left text field, hit 
                               enter (or press the "Predict Next" button), and the most 
                               possible 3 words predicted will display as 
                               follows.'),
                             p('The suggested input "you" shows this clearly.'),
                             p('Top 3 Possibilities:'),
                             h3(textOutput('text'))
                    ),
                    tabPanel("About This Application",
                             fluidRow(
                                 column(2, p('')),
                                 column(8,
                                        includeMarkdown("./about/about_app.md")),
                                 column(2, p('')))
                    )
                )  # tabsetPanel
            )  # mainPanel
        )  #pageWithSidebar
    )  #fluidPage
)  #shinyUI
