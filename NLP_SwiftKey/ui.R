# to install rCharts, use the following
#   require(devtools)
#   install_github('ramnathv/rCharts')

library(shiny);
require(markdown)

shinyUI(fluidPage(theme = "bootstrap.css",pageWithSidebar( 
  titlePanel(h1("Natural Language Processing",align="center",style="padding: 25px 25px")),
  sidebarPanel(h3("Enter Your Text"), textInput("ControlsText", label = NULL, value = "")),
  mainPanel(
    tabsetPanel(
      tabPanel("Word Predictions ",
               h3("Word Predictions using SwiftKey Data"),
               h5("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"),
               h4(textOutput("addDataSize")),
               h4(textOutput("addTextIn")),
               br(),
               h4("This predicts not only the future words, but the word that the user has started typing"),
               hr(),
               h3("Predictions"),
               h4(textOutput("addPredictions1")),
               h4(textOutput("addPredictions2")),
               h4(textOutput("addPredictions3")),
               h4(textOutput("addPredictions4")),
               h4(textOutput("addPredictions5"))
              ),
      tabPanel("Edgar Allan Poe Word Predictions ",
               h3("Word Predictions using the works of Edgar Allan Poe"),
               h5(tags$a(href="http://www.openculture.com/2014/10/download-the-complete-works-of-edgar-allan-poe-macabre-stories-as-free-ebooks-audio-books.html", 
                         "Complete Works Of Edgar Allan Poe")),
               h4(textOutput("addDataSizePoe")),
               h4(textOutput("addTextInPoe")),
               br(),
               h4("This predicts not only the future words, but the word that the user has started typing"),
               hr(),
               h3("Predictions"),
               h4(textOutput("addPredictionsPoe1")),
               h4(textOutput("addPredictionsPoe2")),
               h4(textOutput("addPredictionsPoe3")),
               h4(textOutput("addPredictionsPoe4")),
               h4(textOutput("addPredictionsPoe5"))
              ),
      tabPanel("About", includeMarkdown("README.md"))
      )
    )
)))