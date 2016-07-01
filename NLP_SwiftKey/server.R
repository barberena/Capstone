# to install rCharts, use the following
#   require(devtools)
#   install_github('ramnathv/rCharts')

library(shiny);
source('SearchLogic.R')
source('dataPoe/PoeSearchLogic.R')

# Shiny Happy Server Code
shinyServer(
  function(input, output){
    output$addDataSize <- renderText({paste("Size of n-gram data files: ", dataSize)})
    output$addTextIn <- renderText({paste("Text Entered: ",input$ControlsText)})

    getWords <- reactive({GetPredictionWords(input$ControlsText)})
    output$addPredictions1 <- renderText({getWords()[1]})
    output$addPredictions2 <- renderText({getWords()[2]})
    output$addPredictions3 <- renderText({getWords()[3]})
    output$addPredictions4 <- renderText({getWords()[4]})
    output$addPredictions5 <- renderText({getWords()[5]})
    
    output$addDataSizePoe <- renderText({paste("Size of n-gram data files: ", dataSizePoe)})
    output$addTextInPoe <- renderText({paste("Text Entered: ",input$ControlsText)})
    
    getPoeWords <- reactive({GetPredictionPoeWords(input$ControlsText)})
    output$addPredictionsPoe1 <- renderText({getPoeWords()[1]})
    output$addPredictionsPoe2 <- renderText({getPoeWords()[2]})
    output$addPredictionsPoe3 <- renderText({getPoeWords()[3]})
    output$addPredictionsPoe4 <- renderText({getPoeWords()[4]})
    output$addPredictionsPoe5 <- renderText({getPoeWords()[5]})
    
})