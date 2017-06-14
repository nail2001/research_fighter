library(shiny)
library(scholar)

source("render.R")

shinyServer(function(input, output) {
  observeEvent(input$fight, {
    P1 <- try(get_profile(input$GSID1))
    P2 <- try(get_profile(input$GSID2))
    
    if (class(P1) == "try-error") {
      output$id1 <- renderUI({ HTML("<font color='red'>Incorrect ID!!!</font>") })
    } else {
      output$id1 <- renderUI({ HTML("") })
    }
    
    if (class(P2) == "try-error") {
      output$id2 <- renderUI({ HTML("<font color='red'>Incorrect ID!!!</font>") })
    } else {
      output$id2 <- renderUI({ HTML("") })
    }
    
    if (class(P1) != "try-error" && class(P2) != "try-error") {
      render(output, P1, P2)
    }
  })
})
