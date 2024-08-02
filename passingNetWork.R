library(rsconnect)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(stringr)
library(jsonlite)
library(grid)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("matches",
                  label = "vælg kamp",
                  choices <- c("dkSlov", "dkUK", "dkSerb"),
                  selected = "dkUK"
                  ),
      selectInput("team",
                  label = "vælg hold",
                  choices = c("Denmark","Slovenia","UK"),
                  selected = "Denmark"
                  ),
      numericInput("minutes",
                  label = "Vælg kampperiode",
                  value = 1, step = 1
                  ),
      numericInput("mincount",
                  label = "Vælg minimum antal afleveringer",
                  value = 1, step = 1
                  )
    ),
      mainPanel(
        plotOutput("passplot")
      )
    )
  )

server <- function(input, output, session) {
  dkSlov = fromJSON("data/3930162.json", flatten = T)
  dkUK = fromJSON("data/dkgb.json", flatten = T)
  dkSerb = fromJSON("data/dkserb.json", flatten = T)
  #choices <- c("dkSlov", "dkUk", "dkSerb")
  #choices <- c("dkSlov"="Slovenia", "dkUK"="England", "dkSerb"="Serbia")
  #choices <- c("Option 1" = "value1", "Option 2" = "value2", "Option 3" = "value3")
  
  
  # manuel prep
    #choices <- c("dkSlov"="Slovenia", "dkUK"="England", "dkSerb"="Serbia")
    #choices <- c("dkSlov"="Slovenia", "dkUK"="England", "dkSerb"="Serbia")
    # Here, we simulate fetching data with a static list of choices
    # Update the selectInput choices dynamically
    #updateSelectInput(session, "matches", choices = choices)
  
  #dkMatch = fromJSON("data/3930162.json", flatten = T)
  # mean location for den som afleverer
  
  output$passplot <- renderPlot({
    tval <- as.integer(input$minutes)
    team <- input$team
    minpass <- input$mincount
    test=as.character(input$matches)
    print(test)
    dkMatch=get(test)
    dkMPassesBU=dkMatch %>% filter(type.name=="Pass")
    dkMPasses=dkMPassesBU
    dkMPasses$location.x=unlist(lapply(dkMPasses$location, function(x) x[1]))
    dkMPasses$location.y=unlist(lapply(dkMPasses$location, function(x) x[2]))
    dkMPasses$pass.endLocation.x=unlist(lapply(dkMPasses$pass.end_location, function(x) x[1]))
    dkMPasses$pass.endLocation.y=unlist(lapply(dkMPasses$pass.end_location, function(x) x[2]))
    dkMPasses$pass.endLocation.y=unlist(lapply(dkMPasses$pass.end_location, function(x) x[2]))
    dkMPasses <- dkMPasses %>% mutate(period=(minute %/% 10)+1)
    #ht <- "Denmark"
    #tval <- 1
    #team <- "Denmark"
    #minpass <- 2
    
    dkMPassesG <- dkMPasses %>% filter(period==tval)
    allPM <- dkMPassesG
    allPM1 <- allPM %>% filter(team.name==team)
    allPM1gr <- allPM1 %>% group_by(player.name) %>% 
      mutate(mx=mean(location.x),
             my=mean(location.y),
      )  %>% select(player.name,
                    period,
                    pass.recipient.name,
                    mx,
                    my,
                    pass.endLocation.x,
                    pass.endLocation.y,
                    team.name
      ) %>% unique() 
    allPM1gr <- allPM1gr %>% group_by(pass.recipient.name) %>% mutate(
      mrx=mean(pass.endLocation.x),
      mry=mean(pass.endLocation.y),
    ) %>% ungroup()
    # now count the number of passes
    df2=allPM1gr %>% group_by(player.name,pass.recipient.name) %>% 
      mutate(cnt=n()) %>% arrange(desc(cnt)) %>% filter(cnt>minpass) %>% unique() %>% 
      select(player.name,cnt,pass.recipient.name,mx,my,mrx,mry,team.name) %>%  unique()
    
    custom_colors <- c("A" = "blue", "B" = "red")
    custom_labels <- c("A" = "Group A", "B" = "Group B")
    
    ggplot(df2) +
      annotate_pitch() +
      geom_segment(aes(x = mx, y = my, xend = mrx, yend = mry),size=df2$cnt/8,
                   arrow = arrow(length = unit(0.3, "cm"), type = "closed"))+
      geom_point(aes(x=mx,y=my,color=df2$team.name), size=2)+
      geom_point(aes(x=mrx,y=mry,color=df2$team.name), size=2)+
      geom_text(aes(x=mx,y=my,label=player.name),color="blue",size=2,vjust=-2)+
      geom_text(aes(x=mrx,y=mry,label=pass.recipient.name),color="red",size=2,vjust=2)+
      theme_pitch() +
      theme(legend.position = "none")+
      direction_label() +
      ggtitle("passmap", df2$team.name )
      
    
  })
}

shinyApp(ui, server)
