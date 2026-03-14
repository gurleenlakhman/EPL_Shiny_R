library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)

# Data
epl <- read.csv("data/epl_final.csv", stringsAsFactors = FALSE)
epl$MatchDate <- as.Date(epl$MatchDate)

# Dropdown choices
all_seasons <- sort(unique(epl$Season))
all_teams   <- sort(unique(c(epl$HomeTeam, epl$AwayTeam)))

ui <- dashboardPage(
  dashboardHeader(title = "EPL Match Tracker"),
  
  dashboardSidebar(
    selectInput(
      inputId  = "team",
      label    = "Select Team",
      choices  = all_teams,
      selected = "Arsenal"
    ),
    selectInput(
      inputId  = "season",
      label    = "Select Season",
      choices  = all_seasons,
      selected = "2023/24"
    )
  ),
  
  dashboardBody(
    fluidRow(
      valueBoxOutput("total_matches", width = 4),
      valueBoxOutput("home_win_rate", width = 4),
      valueBoxOutput("away_win_rate", width = 4)
    ),
    fluidRow(
      box(title = "Home vs Away Performance", width = 6,
          solidHeader = TRUE, status = "primary",
          plotOutput("home_away_plot")),
      box(title = "Win Rate: Home vs Away", width = 6,
          solidHeader = TRUE, status = "primary",
          plotOutput("win_rate_plot"))
    ),
    fluidRow(
      box(title = "Season Progression", width = 12,
          solidHeader = TRUE, status = "info",
          plotOutput("progression_plot"))
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive calc — filters data by team and season
  filtered_data <- reactive({
    team   <- input$team
    season <- input$season
    
    # Home matches from team's perspective
    home_rows <- epl |>
      filter(HomeTeam == team, Season == season) |>
      transmute(
        Date   = MatchDate,
        Venue  = "Home",
        GF     = FullTimeHomeGoals,
        GA     = FullTimeAwayGoals,
        Result = case_when(
          FullTimeResult == "H" ~ "Win",
          FullTimeResult == "A" ~ "Loss",
          TRUE                  ~ "Draw"
        )
      )
    
    # Away matches from team's perspective
    away_rows <- epl |>
      filter(AwayTeam == team, Season == season) |>
      transmute(
        Date   = MatchDate,
        Venue  = "Away",
        GF     = FullTimeAwayGoals,
        GA     = FullTimeHomeGoals,
        Result = case_when(
          FullTimeResult == "A" ~ "Win",
          FullTimeResult == "H" ~ "Loss",
          TRUE                  ~ "Draw"
        )
      )
    
    bind_rows(home_rows, away_rows) |>
      arrange(Date) |>
      mutate(
        SeasonThird = case_when(
          row_number() <= n() / 3       ~ "Early",
          row_number() <= 2 * n() / 3   ~ "Mid",
          TRUE                          ~ "Late"
        )
      )
  })
  
}


