library(shiny)
library(dplyr)
library(ggplot2)
library(ggsoccer)




ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("team",
                  label = "Vælg hold",
                  choices = NULL
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
  
  
  
}

shinyApp(ui, server)