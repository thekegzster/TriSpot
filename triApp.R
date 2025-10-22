library(shiny)
library(tidyverse)
library(rlang)

#Source Files
source("athleteInformation.R")
source("personalRecords.R")


#Load Data
prData <- read_csv("personalRecords.csv")
athleteInformation <- read_csv("athleteInformation.csv")

#Data Processing
allAthletes <- prData$Name

ui <- fluidPage(
  div(
    style = "min-height: 100vh; position: relative;",
    
    # Fixed athlete selector
    absolutePanel(
      top   = "5%", left = "2%", width = "250px",
      selectInput(
        inputId  = "selected_athlete",
        label    = "Search...",
        choices  = NULL,
        selected = NULL
      )
    ),
    
    uiOutput("mainContent")
  )
)

server <- function(input, output, session) {
  
  #Response for when new player is selected
  observeEvent(prData, {
    updateSelectInput(
      session,
      "selected_athlete",
      choices  = allAthletes,
      selected = allAthletes[1]
    )
  })
  
  #Get the athlete
  selected_athlete <- reactive({
    req(input$selected_athlete)
    input$selected_athlete
  })
  
  #Get athlete details
  athleteInformation <- reactive({
    prData |> 
      filter(Name == selected_athlete()) |> 
      select(
        name       = Name,
        age        = Age,
        gender     = Gender,
        college    = College,
        city       = City,
        state      = State,
        country    = Country
      ) |> 
      slice(1)
  })
  
  output$mainContent <- renderUI({
    req(input$selected_athlete)
    player <- input$selected_athlete
    
    tabs <- list()
    
    tabs <- append(tabs, list(
      tabPanel("Athlete Home Page",
               # Athlete Information Panel
               absolutePanel(
                 top    = "13%", 
                 left   = "2%",
                 width  = "27%",
                 athleteInformationUI("athleteInformation")
               ),
               # Personal Records Panel
               absolutePanel(
                 top    = "5%", 
                 bottom = "2%",
                 left   = "30%",   
                 width  = "35%", 
                 personalRecordsUI("personalRecords")
               )
      )
    ))
    
    # If no tabs, show a placeholder
    if (length(tabs) == 0) {
      div("No data available for this athlete")
    } else {
      do.call(tabsetPanel, c(
        list(id = "main_tabs", type = "tabs"),
        tabs
      ))
    }
    
  })
  
  # Basic athlete information server
  athleteInformationServer(
    id           = "athleteInformation",
    details_df   = athleteInformation,
    action_img   = action_photo,
    headshot_img = headshot_photo
  )
  
  # Personal records server
  personalRecordsServer(
    id = "personalRecords",
    stats_df = reactive({
      prData |> 
        filter(Name == selected_athlete())
    })
  )
  
}



# Launch the app
shinyApp(ui = ui, server = server)

