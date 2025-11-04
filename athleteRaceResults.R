library(reactable)
library(htmltools)

# ---------- helpers ----------
format_seconds <- function(sec) {
  sprintf("%02d:%02d:%02d", sec %/% 3600, (sec %% 3600) %/% 60, sec %% 60)
}

getRaceResults <- function(df){
  df |>
    mutate(
      ChipTime = format_seconds(ChipTime)
    ) |>
    select(
      ChipTime, raceName, year, Place, AgeCategory, PlaceAgeCategory, raceDistance
    ) |>
    rename(
      Time = ChipTime,
      Race = raceName,
      Year = year,
      `Age Cat` = AgeCategory,
      `AC Place` = PlaceAgeCategory,
      Distance = raceDistance
    ) |>
    mutate(
      Year = as.integer(Year),
      Place = as.integer(Place),
      `AC Place` = as.integer(`AC Place`)
    )
}

# ---------- UI ----------
athleteRaceResultsUI <- function(id) {
  ns <- NS(id)
  div(
    class = "basic-info-card",
    style = "
      display:flex; flex-direction:column; background:#fff; border:1px solid #e5e7eb;
      border-radius:14px; overflow:hidden; box-shadow:0 6px 18px rgba(0,0,0,0.06); width:100%;
    ",
    
    # header
    div(
      style = "
        display:flex; align-items:center; justify-content:center;
        background: linear-gradient(90deg, #1e3a8a, #0f766e);
        color:white; padding:14px 16px;
      ",
      tags$div(
        style = "display:flex; align-items:center; gap:10px;",
        tags$h3("Race Results", style="margin:0; font-weight:700; letter-spacing:.2px; text-align:center;")
      )
    ),
    
    # border between header and table
    div(style="height:3px; background:linear-gradient(90deg,#0ea5e950,#22c55e50);"),
    
    # table
    div(
      style = "padding:14px;",
      reactableOutput(ns("athlete_race_results_table"))
    ),
    
    # component-scoped CSS
    tags$style(HTML(sprintf("
      /* pills / chips */
      .%1$s-pill { display:inline-block; padding:2px 8px; border-radius:9999px;
                   font-size:12px; font-weight:600; line-height:1; }
      .%1$s-pill-gray  { background:#f3f4f6; color:#111827; }
      .%1$s-pill-blue  { background:#e0f2fe; color:#075985; }
      .%1$s-pill-emerald{ background:#d1fae5; color:#065f46; }
      .%1$s-pill-amber { background:#fef3c7; color:#92400e; }
      .%1$s-pill-rose  { background:#ffe4e6; color:#9f1239; }
      .%1$s-mono { font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace; }
      .%1$s-medal { display:inline-flex; align-items:center; gap:6px; }
      .%1$s-medal i { font-style:normal; font-weight:700; padding:1px 6px; border-radius:9999px; font-size:12px; }
      .%1$s-gold   { background:#fef3c7; color:#92400e; }
      .%1$s-silver { background:#e5e7eb; color:#374151; }
      .%1$s-bronze { background:#fce7f3; color:#9d174d; }
      .%1$s-rank   { background:#eef2ff; color:#3730a3; }
      .%1$s-year   { background:#f1f5f9; color:#0f172a; padding:2px 8px; border-radius:8px; font-weight:600; }
      .%1$s-cell   { white-space: nowrap; }
    ", ns("")))
    )
  )
}

# ---------- SERVER ----------
athleteRaceResultsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # tiny render helpers (return HTML tags)
    medal_cell <- function(place) {
      if (is.na(place)) return(span(place))
      if (place == 1) div(class="medal-cell",
                          tags$span(class=sprintf("%s-medal", session$ns("")),
                                    tags$i("1st", class=sprintf("%s-gold", session$ns("")))))
      else if (place == 2) div(class="medal-cell",
                               tags$span(class=sprintf("%s-medal", session$ns("")),
                                         tags$i("2nd", class=sprintf("%s-silver", session$ns("")))))
      else if (place == 3) div(class="medal-cell",
                               tags$span(class=sprintf("%s-medal", session$ns("")),
                                         tags$i("3rd", class=sprintf("%s-bronze", session$ns("")))))
      else div(class="medal-cell",
               tags$span(class=sprintf("%s-medal", session$ns("")),
                         tags$i(paste0("#", place), class=sprintf("%s-rank", session$ns("")))))
    }
    
    distance_chip <- function(d) {
      pal <- list(
        "Sprint"   = "blue",
        "Olympic"  = "emerald"
      )
      cls <- pal[[as.character(d)]]
      if (is.null(cls)) cls <- "gray"
      tags$span(class = sprintf("%s-pill %s-pill-%s", session$ns(""), session$ns(""), cls),
                as.character(d))
    }
    
    age_place_pill <- function(x) {
      tags$span(class = sprintf("%s-pill %s-pill-gray", session$ns(""), session$ns("")),
                ifelse(is.na(x), "", paste0("#", x)))
    }
    
    output$athlete_race_results_table <- reactable::renderReactable({
      df <- getRaceResults(data())
      
      reactable::reactable(
        df,
        theme = reactable::reactableTheme(
          borderColor = "#e5e7eb",
          stripedColor = "#fafafa",
          highlightColor = "#eef2ff"
        ),
        searchable = FALSE,
        resizable = FALSE,
        bordered = FALSE,
        compact = TRUE,
        striped = TRUE,
        highlight = TRUE,
        defaultPageSize = 8,
        paginationType = "simple",
        defaultSorted = list(Year = "desc"),
        defaultColDef = colDef(
          headerClass = "rt-header",
          class = sprintf("%s-cell", session$ns("")),
          minWidth = 120
        ),
        columns = list(
          Time = colDef(
            name = "Time",
            minWidth = 80,
            cell = function(val) tags$span(class=sprintf("%s-mono", session$ns("")), val)
          ),
          Race = colDef(name = "Race", minWidth = 155),
          Year = colDef(
            name = "Year",
            minWidth = 80,
            cell = function(val) tags$span(class = sprintf("%s-year", session$ns("")), val)
          ),
          Place = colDef(
            name = "Place",
            minWidth = 65,
            align = "left",
            cell = function(val) medal_cell(val)
          ),
          `Age Cat` = colDef(name = "Age Cat", minWidth = 80),
          `AC Place` = colDef(
            name = "AC Place",
            minWidth = 85,
            cell = function(val) age_place_pill(val)
          ),
          Distance = colDef(
            name = "Distance",
            minWidth = 85,
            cell = function(val) distance_chip(val)
          )
        )
      )
    })
  })
}
