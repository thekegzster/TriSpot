library(shiny)
library(tidyverse)

# --------------------------------------------------
# UI: Athlete PR Info Card
# --------------------------------------------------
personalRecordsUI <- function(id) {
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
    
    # PR Table
    div(
      style = "padding: 12px;",
      tableOutput(ns("basicStats"))
    )
  )
}

# --------------------------------------------------
# SERVER: Athlete PR Info Card Logic
# --------------------------------------------------
personalRecordsServer <- function(id, stats_df) {
  moduleServer(id, function(input, output, session) {
    
    # Basic personal records table
    output$basicStats <- renderTable({
      stats_df() |> 
        select(PR_Time_Sprint, PR_Time_Olympic)
    }, rownames = FALSE, spacing = "l", align = "c")
  })
}

