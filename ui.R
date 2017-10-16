library(shiny)

source("data.R")

shinyUI(
  fluidPage(
    br(),
    HTML('<center><img src="rf2.png"></center>'),
    br(),
    br(),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        HTML("<font color='#ff8c00'><h2>FIGHTER 1:</h2></font>"),
        selectInput("fighter1", "Name:", c("unknown", fighters$name)),
        textInput("GSID1", "Google Scholar ID:", ""),
        htmlOutput("id1"),

        br(),
        br(),
        br(),
        
        HTML("<font color='#0073ff'><h2>FIGHTER 2:</h2></font>"),
        selectInput("fighter2", "Name:", c("unknown", fighters$name)),
        textInput("GSID2", "Google Scholar ID:", ""),
        htmlOutput("id2"),
        
        br(),
        br(),
        
        tags$head(
          tags$style(HTML('#fight{background-color:LightCoral; float:right}'))
        ),
        actionButton("fight", "Fight!"),
        
        br(),
        br()
        
      ),
      
      mainPanel(
        htmlOutput("result"),
        tableOutput("totalCitesAndIndexVsTable"),
        tableOutput("yearOfCiteVsTable"),
        tableOutput("yearOfPubVsTable"),
        plotOutput("yearOfCite"),
        plotOutput("yearOfPub")
      )
    )
  )
)
