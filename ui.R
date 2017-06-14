library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Research Fighter"),
    
    sidebarLayout(
      sidebarPanel(
        textInput("GSID1", "Google Scholar ID:", "wEU99lsAAAAJ"),
        htmlOutput("id1"),
        textInput("GSID2", "Google Scholar ID:", "R2ZrHtsAAAAJ"),
        htmlOutput("id2"),
        actionButton("fight", "Fight!")
      ),
      
      mainPanel(
        tableOutput("mainVsTable"),
        tableOutput("yearOfCiteVsTable"),
        tableOutput("yearOfPubVsTable"),
        plotOutput("yearOfCite"),
        plotOutput("yearOfPub")
      )
    )
  )
)
