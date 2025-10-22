library(shiny)
library(tidyverse)

# --------------------------------------------------
# UI: Basic Athlete Info Card
# --------------------------------------------------

athleteInformationUI <- function(id) {
  ns <- NS(id)
  div(
    class = "basic-info-card",
    style = "
      display: flex;
      flex-direction: column;
      background: #fff;
      border: 1px solid #ccc;
      border-radius: 8px;
      overflow: hidden;
      box-shadow: 0 2px 6px rgba(0,0,0,0.1);
      width: 100%;
    ",
    
    # Action photo container
    div(
      style = "
        position: relative;
        width: 100%;
        height: 200px;
        overflow: hidden;
        display: flex;
        justify-content: center;
        align-items: center;
        background: #fff;
      ",
      # Placeholder for action shot
      imageOutput(ns("actionPhoto"), width = "auto", height = "200px"),
      
      # Headshot overlay centered bottom
      tags$div(
        style = "
          position: absolute;
          bottom: 5px;
          left: 50%;
          transform: translateX(-50%);
          width: 100px;
          height: 100px;
          border-radius: 50%;
          overflow: hidden;
          border: 2px solid #fff;
          background: #fff;
        ",
        # Placeholder for headshot
        imageOutput(ns("headshot"), width = "60px", height = "60px")
      )
    ),
    
    # Information below images
    div(
      style = "padding: 12px;",
      uiOutput(ns("athleteInformation"))
    )
  )
}

# --------------------------------------------------
# SERVER: Basic Athlete Info Card Logic
# --------------------------------------------------
athleteInformationServer <- function(id,
                                     details_df,    # reactive 1-row df: name, college, state, country, etc.
                                     action_img,    # reactive string path/URL for action photo
                                     headshot_img   # reactive string path/URL for headshot
) {
  moduleServer(id, function(input, output, session) {
    output$actionPhoto <- renderImage({
      req(action_img())
      list(
        src         = action_img(),
        contentType = "image/png",
        alt         = "Action shot",
        width       = NULL,
        height      = 200
      )
    }, deleteFile = FALSE)
    
    output$headshot <- renderImage({
      req(headshot_img())
      list(
        src         = headshot_img(),
        contentType = "image/png",
        alt         = "Headshot",
        width       = 100,
        height      = 100
      )
    }, deleteFile = FALSE)
    
    # Athlete Information list
    output$athleteInformation <- renderUI({
      df <- details_df()
      tags$ul(
        style = "list-style: none; padding-left: 0; margin: 0 0 12px 0;",
        tags$li(tags$strong("Name: "), df$Name),
        tags$li(tags$strong("College: "), df$College),
        tags$li(tags$strong("City: "), df$City),
        tags$li(tags$strong("State: "), df$State),
        tags$li(tags$strong("Country: "), df$Country),
        tags$li(tags$strong("Gender: "), df$Gender),
        tags$li(tags$strong("Age: "), df$Age)
      )
    })
    
  })
}





