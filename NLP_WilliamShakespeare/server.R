# to install rCharts, use the following
#   require(devtools)
#   install_github('ramnathv/rCharts')

library(shiny);
source('dataWS/WSSearchLogic.R')

# Shiny Happy Server Code
shinyServer(
  function(input, output){
    output$addDataSizeWS <- renderText({paste("Size of n-gram data files: ", dataSizeWS)})
    output$addTextInWS <- renderText({paste("Text Entered: ",input$ControlsText)})
    
    getWSWords <- reactive({GetPredictionWSWords(input$ControlsText)})
    output$addPredictionsWS1 <- renderText({getWSWords()[1]})
    output$addPredictionsWS2 <- renderText({getWSWords()[2]})
    output$addPredictionsWS3 <- renderText({getWSWords()[3]})
    output$addPredictionsWS4 <- renderText({getWSWords()[4]})
    output$addPredictionsWS5 <- renderText({getWSWords()[5]})
    
})