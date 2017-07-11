library(shiny)

library(R.cache)
library(dplyr)
library(httr)
library(rvest)
library(stringr)
library(xml2)

source("scholar.R")
source("render.R")
source("data.R")

shinyServer(function(input, output, session) {
  
  observeEvent(input$fighter1, {
    if(input$fighter1 != "unknown")
      updateTextInput(session, "GSID1", value = fighters[fighters$name==input$fighter1,]$id)
  })

  observeEvent(input$fighter2, {
    if(input$fighter2 != "unknown")
      updateTextInput(session, "GSID2", value = fighters[fighters$name==input$fighter2,]$id)
  })

  observeEvent(input$GSID1, {
    if(!input$GSID1 %in% fighters$id)
      updateSelectInput(session, "fighter1", selected = "unknown")
  })

    observeEvent(input$GSID2, {
    if(!input$GSID2 %in% fighters$id)
      updateSelectInput(session, "fighter2", selected = "unknown")
  })
  
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
