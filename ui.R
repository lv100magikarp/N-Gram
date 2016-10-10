library(shiny)
shinyUI(pageWithSidebar(
  headerPanel('N-Gram Prediction'),
  sidebarPanel(
    width = 6,
    textInput(inputId = 'inp',label = 'Input your text here (at least 2 words):'),
    actionButton(inputId = 'broad','Broad'),
    actionButton(inputId = 'local','Local'),
    h4('Note 1: Local works with the last 2 words. Broad works with a larger part of the 
       input text, but without stopwords (words without concrete meanings). Broad will 
       default to more lenient search if the restricted set of data is too small. It 
       usually produces predictions relevant to the input, but not necessarily what 
       grammatically follows. Each type of prediction offers at most the top 3 predictions
       in descending order. Local is recommended.'),
    h4('Note 2: This app can be slow in certain circumstances.')
  ),
  mainPanel(
    width = 4,
    h3('The predictions for the next word:'),
    h4('Broad:'),
    textOutput('broadpred'),
    h4('Local'),
    textOutput('localpred')
  )
))