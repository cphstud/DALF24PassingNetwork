library(shiny)
library(dplyr)
library(ggplot2)
library(ggsoccer)




ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("team",
                  label = "Vælg hold",
                  choices = dteams
                  ),
      selectInput("match",
                  label = "Vælg kamp",
                  choices = NULL
                  ),
      numericInput("minutes",
                  label = "Vælg kampperiode",
                  value = 0, step = 10
                  )
    ),
      mainPanel(
        plotOutput("passplot")
      )
    )
  )

server <- function(input, output, session) {
  # hold i dropdown
  observe({
    tval <- input$team
    tval=ifelse(nchar(tval)<2,"Ajax",input$team)
    choices <- dutchmatches %>% filter(home==tval) %>% select(longinfo)
    updateSelectInput(session, "match", choices = choices)
  })
  
  output$passplot <- renderPlot({
    mval <- input$match
    match <- str_split(mval,"_")
    mid <- match[[1]][2]
    teams <- match[[1]][1]
    hamatch <- str_split(teams," - ")
    
    #home and away teams
    ht <- hamatch[[1]][1]
    at <- hamatch[[1]][2]
    mid <- gsub(" ","",mid)
    print(mid)
    allPM <- allPasses %>% filter(matchId==mid)
    
    # prep the dataframe
    # split the teams
    allPM1 <- allPM %>% filter(team.name==ht)
    allPM2 <- allPM %>% filter(opponentTeam.name==at)
    
    
    
    
    
    
  })
}

shinyApp(ui, server)
