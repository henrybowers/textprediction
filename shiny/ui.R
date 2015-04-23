library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("united"),
                fluidRow(
                        titlePanel("Fast next-word predictor")
                ),
                hr('Type words here:'),
                textInput('inputText', '', value = ""),
                hr('Predicted next word is:'),
                fluidRow(
                        column(width=3, verbatimTextOutput('outputText'))
                ),
                fluidRow( verbatimTextOutput('parsed'))
        )
)
