library(shiny)
library(tidyverse)
library(rlang)

#Source Files
source("athleteInformation.R")
source("personalRecords.R")
source("athleteRaceResults.R")

source("raceResults.R")


#Load Data
prData <- read_csv("personalRecords.csv")
athleteInformation <- read_csv("athleteInformation.csv")
allResults <- read_csv("allResults.csv")


#Data Processing
allAthletes <- prData$Name |> 
  sort()

#Default photo paths
default_action <- "www/defaults/default_action.jpg"
default_headshot <- "www/defaults/default_headshot.jpg"


ui <- fluidPage(
  div(
    style = "min-height: 100vh; position: relative;",

    # Fixed athlete selector
    conditionalPanel(
      condition = "input.main_tabs === 'Athlete Home Page'",
      absolutePanel(
        top   = "5%", left = "2%", width = "250px",
        selectInput(
          inputId  = "selected_athlete",
          label    = "Search...",
          choices  = NULL,
          selected = NULL
        )
      )
    ),
    
    uiOutput("mainContent")
  )
)

server <- function(input, output, session) {
  
  #Response for when new player is selected
  observe({
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
  athleteInformationData <- reactive({
    athleteInformation |> 
      filter(Name == selected_athlete()) |> 
      slice(1)
  })
  
  #Get athlete photos
  action_photo <- reactive({
    path <- normalizePath(
      file.path("www", "actions", paste0(selected_athlete(), "_action.jpg")),
      mustWork = FALSE
    )
    if (!file.exists(path)) {
      normalizePath(default_action, mustWork = FALSE)
    } else {
      path
    }
  })
  
  headshot_photo <- reactive({
    path <- normalizePath(
      file.path("www", "headshots", paste0(selected_athlete(), "_headshot.png")),
      mustWork = FALSE
    )
    if (!file.exists(path)) {
      normalizePath(default_headshot, mustWork = FALSE)
    } else {
      path
    }
  })
  
  college_logo <- reactive({
    college <- athleteInformationData()$College[1]

    rel <- file.path("logos", paste0(college, ".png"))
    abs <- file.path("www", rel)
    
    if (file.exists(abs)) rel else NULL
  })
  
  output$mainContent <- renderUI({
    req(input$selected_athlete)
    player <- input$selected_athlete
    
    tabs <- list()
    
    #Athlete Home Page Panel
    tabs <- append(tabs, list(
      tabPanel("Athlete Home Page",
               # Athlete Information Panel
               absolutePanel(
                 top    = "13%",
                 bottom = "2%",
                 left   = "2%",
                 width  = "27%",
                 athleteInformationUI("athleteInformation")
               ),
               # Personal Records Panel
               absolutePanel(
                 top    = "6%", 
                 bottom = "2%",
                 left   = "30%",   
                 width  = "20%", 
                 personalRecordsUI("personalRecords")
               ),
               # Athlete Race Results Panel
               absolutePanel(
                 top    = "6%", 
                 bottom = "2%",
                 right  = "2%",   
                 width  = "47%", 
                 athleteRaceResultsUI("athleteRaceResults")
               )
      )
    ))
    
    #Race Results Panel
    tabs <- append(tabs, list(
      tabPanel("Race Results",
               # Race Results Panel
               absolutePanel(
                 top    = "5%",
                 bottom = "2%",
                 left   = "2%",
                 width  = "70%",
                 athleteInformationUI("raceResults")
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
    details_df   = athleteInformationData,
    action_img   = action_photo,
    headshot_img = headshot_photo,
    logo_img     = college_logo
  )
  
  # Personal records server
  personalRecordsServer(
    id = "personalRecords",
    data = reactive({
      prData |> 
        filter(Name == selected_athlete())
    })
  )
  
  # Athlete Race Results Server
  athleteRaceResultsServer(
    id = "athleteRaceResults",
    data = reactive({
      allResults |> 
        filter(Name == selected_athlete())
    })
  )
  
  #Race Results Server
  #raceResultsServer(
  #  id = "raceResults",
  #  race_df = reactive({
  #    allResults
  #  })
  #)
  
}



# Launch the app
shinyApp(ui = ui, server = server)

