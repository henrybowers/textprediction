library(shinythemes)

shinyUI(fluidPage(theme = shinytheme('united'),
                  
                fluidRow(
                    titlePanel('Fast and furious next-word predictor')
                    ),
                hr(),
                fluidRow(
                    column(width=4,
                           #h4('Type words here:'),
                           textInput('inputText', 'Type words here:', value = "")
                           ),
                    column(width=4,
                           h5(strong('Predicted next word:')),
                           h5(textOutput('outputText'))
                           )
                    ),
                hr(),
                fluidRow(
                    column(width=12,
                           h5(strong(('Other likely next words:'))),
                           p(),
                           tableOutput('goodMatches')
                           )
                    )
                )
    )
 