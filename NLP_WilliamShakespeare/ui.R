# to install rCharts, use the following
#   require(devtools)
#   install_github('ramnathv/rCharts')

library(shiny);
#require(markdown)

shinyUI(fluidPage(theme = "bootstrap.css",pageWithSidebar( 
  titlePanel(h1("Natural Language Processing",align="center",style="padding: 25px 25px")),
  sidebarPanel(h3("Enter Your Text"), textInput("ControlsText", label = NULL, value = "")),
  mainPanel(
    tabsetPanel(
      tabPanel("William Shakespeare Word Predictions ",
               h3("Word Predictions using the works of William Shakespeare"),
               h5(tags$a(href="http://www.gutenberg.org/ebooks/100", 
                         "Complete Works Of William Shakespeare")),
               h4(textOutput("addDataSizeWS")),
               h4(textOutput("addTextInWS")),
               br(),
               h4("This predicts not only the future words, but the word that the user has started typing"),
               hr(),
               h3("Predictions"),
               h4(textOutput("addPredictionsWS1")),
               h4(textOutput("addPredictionsWS2")),
               h4(textOutput("addPredictionsWS3")),
               h4(textOutput("addPredictionsWS4")),
               h4(textOutput("addPredictionsWS5"))
              )
      #tabPanel("About", includeMarkdown("README.md"))
      )
    )
)))