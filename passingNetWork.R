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
      )
    )
  )

server <- function(input, output, session) {
  # hold i dropdown
  observe({
    tval <- input$team
    choices <- dutchmatches %>% filter(home==tval) %>% select(label)
    updateSelectInput(session, "match", choices = choices)
  })
  
  
}

shinyApp(ui, server)