library(shiny)
library(tidyverse)
library(hms)

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
      border-radius: 12px;
      overflow: hidden;
      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      width: 100%;
    ",
    
    div(
      style = "
        display:flex; align-items:center; justify-content:center;
        background: linear-gradient(90deg, #1e3a8a, #0f766e);
        color: white;
        padding: 14px 16px;
      ",
      tags$h3(
        "Personal Records",
        style = "margin:0; font-weight:700; letter-spacing:.3px; text-align:center;"
      )
    ),
    
    #Border between header and table
    div(style="height:3px; background:linear-gradient(90deg,#0ea5e950,#22c55e50);"),
    
    #Tabs
    uiOutput(ns("tabs")),
    
    div(style = "padding: 14px 16px;", uiOutput(ns("tabContent")))
    
  )
}


# --------------------------------------------------
# SERVER: Athlete PR Info Card Logic
# --------------------------------------------------
personalRecordsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    time_cols <- c("PR_Time_Sprint", "PR_Swim_Time_Sprint", "PR_Swim_Pace_Sprint", "PR_Bike_Time_Sprint", "PR_Run_Time_Sprint",
                   "PR_Run_Pace_Sprint", "PR_Time_Olympic", "PR_Swim_Time_Olympic", "PR_Swim_Pace_Olympic", "PR_Bike_Time_Olympic", 
                   "PR_Run_Time_Olympic", "PR_Run_Pace_Olympic", "PR_T1", "PR_T2")
    
    bike_pace_cols <- c("PR_Bike_Pace_Sprint", "PR_Bike_Pace_Olympic")
    
    sprint_cols <- c(
      "PR_Swim_Time_Sprint",
      "PR_Swim_Pace_Sprint",
      "PR_Bike_Time_Sprint",
      "PR_Bike_Pace_Sprint",
      "PR_Run_Time_Sprint",
      "PR_Run_Pace_Sprint",
      "PR_T1",
      "PR_T2",
      "PR_Time_Sprint"
    )
    
    olympic_cols <- c(
      "PR_Swim_Time_Olympic",
      "PR_Swim_Pace_Olympic",
      "PR_Bike_Time_Olympic",
      "PR_Bike_Pace_Olympic",
      "PR_Run_Time_Olympic",
      "PR_Run_Pace_Olympic",
      "PR_T1",
      "PR_T2",
      "PR_Time_Olympic"
    )
    
    # Labels for display
    label_map <- c(
      PR_Swim_Time_Sprint    = "Swim Time",
      PR_Swim_Pace_Sprint    = "Swim Pace",
      PR_Bike_Time_Sprint    = "Bike Time",
      PR_Bike_Pace_Sprint    = "Bike Pace",
      PR_Run_Time_Sprint     = "Run Time",
      PR_Run_Pace_Sprint     = "Run Pace",
      PR_Time_Sprint         = "Total Time",
      PR_Swim_Time_Olympic   = "Swim Time",
      PR_Swim_Pace_Olympic   = "Swim Pace",
      PR_Bike_Time_Olympic   = "Bike Time",
      PR_Bike_Pace_Olympic   = "Bike Pace",
      PR_Run_Time_Olympic    = "Run Time",
      PR_Run_Pace_Olympic    = "Run Pace",
      PR_Time_Olympic        = "Total Time",
      PR_T1                  = "T1",
      PR_T2                  = "T2"
    )
    
    # Helper: has valid total time
    has_sprint  <- reactive({
      df <- data()
      if (nrow(df) == 0) return(FALSE)
      v <- suppressWarnings(as.numeric(df$PR_Time_Sprint[1]))
      !is.na(v) && is.finite(v) && v > 0
    })
    
    has_olympic <- reactive({
      df <- data()
      if (nrow(df) == 0) return(FALSE)
      v <- suppressWarnings(as.numeric(df$PR_Time_Olympic[1]))
      !is.na(v) && is.finite(v) && v > 0
    })
    
    # Dynamic tabs: only include tabs that have valid totals
    output$tabs <- renderUI({
      tabs <- list()
      if (has_sprint())  tabs <- c(tabs, list(tabPanel("Sprint",  value = "sprint")))
      if (has_olympic()) tabs <- c(tabs, list(tabPanel("Olympic", value = "olympic")))
      
      if (length(tabs) == 0) {
        # No tabs to show
        return(div(style="padding:12px; color:#666;", "No personal records to display yet."))
      }
      
      # Choose a sensible selected tab
      current <- input$distance_tab
      valid_vals <- c(if (has_sprint()) "sprint", if (has_olympic()) "olympic")
      selected <- if (is.null(current) || !(current %in% valid_vals)) valid_vals[1] else current
      
      tabsetPanel(id = session$ns("distance_tab"), type = "tabs", selected = selected, !!!tabs)
    })
    
    format_hms <- function(sec) {
      ifelse(
        is.na(sec),
        NA_character_,
        sprintf("%02d:%02d:%02d", sec %/% 3600, (sec %% 3600) %/% 60, sec %% 60)
      )
    }
    
    # Helper to build a tidy display table for a given distance
    build_table <- function(df, cols) {
      out <- df() |>
        slice(1) |>
        select(any_of(cols)) |>
        mutate(across(everything(), as.character)) |>
        pivot_longer(
          cols = everything(),
          names_to = "MetricRaw",
          values_to = "ValueRaw"
        ) |>
        mutate(
          Metric = recode(MetricRaw, !!!label_map),
          Time = case_when(
            # Change format of Swim and Run paces and transitions
            str_detect(MetricRaw, "Swim_Pace") ~ {
              formatted <- format_hms(as.numeric(ValueRaw))
              formatted <- sub("^00:", "", formatted)
              paste0(formatted, " /100m")
            },
            str_detect(MetricRaw, "Run_Pace") ~ {
              formatted <- format_hms(as.numeric(ValueRaw))
              formatted <- sub("^00:", "", formatted)
              paste0(formatted, " /mile")
            },
            str_detect(MetricRaw, "T1") ~ {
              formatted <- format_hms(as.numeric(ValueRaw))
              formatted <- sub("^00:", "", formatted)
            },
            str_detect(MetricRaw, "T2") ~ {
              formatted <- format_hms(as.numeric(ValueRaw))
              formatted <- sub("^00:", "", formatted)
            },

            MetricRaw %in% bike_pace_cols ~ as.character(ValueRaw),
            
            TRUE ~ format_hms(as.numeric(ValueRaw))
          )
        ) |>
        select(Metric, Time) |>
        filter(!is.na(Time) & Time != "")            # drop empty rows
      out
    }

    # Content under the tabs
    output$tabContent <- renderUI({
      if (!has_sprint() && !has_olympic()) {
        return(div(style="padding:12px; color:#666;", "No results available for this athlete."))
      }
      current <- input$distance_tab
      if (is.null(current)) current <- if (has_sprint()) "sprint" else "olympic"
      
      tbl <- if (identical(current, "olympic")) build_table(data, olympic_cols)
      else                              build_table(data, sprint_cols)
      
      tagList(
        tags$table(
          style = "width:100%; border-collapse:separate; border-spacing:0 6px;",
          tags$thead(
            tags$tr(
              tags$th("Metric", style="text-align:left; padding:6px 8px; font-weight:600; color:#555;"),
              tags$th("Time",   style="text-align:right; padding:6px 8px; font-weight:600; color:#555;")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(tbl)), function(i) {
              is_total <- tbl$Metric[i] == "Total Time"
              style_row <- if (is_total) "background:#e8f0ff; font-weight:600;" else "background:#f9fafb;"
              
              tags$tr(
                style = style_row,
                tags$td(
                  tbl$Metric[i],
                  style = paste0(
                    "padding:10px 8px; border-radius:10px 0 0 10px;",
                    if (is_total) " font-weight:700;" else ""
                  )
                ),
                tags$td(
                  tbl$Time[i],
                  style = paste0(
                    "padding:10px 8px; text-align:right; border-radius:0 10px 10px 0;",
                    if (is_total) " font-weight:700;" else ""
                  )
                )
              )
            })
          )
        )
      )
    })
    
  })
}

