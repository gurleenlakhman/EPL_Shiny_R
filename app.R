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
    # Value boxes
    output$total_matches <- renderValueBox({
      valueBox(nrow(filtered_data()), "Total Matches", icon = icon("futbol"), color = "blue")
    })
    
    output$home_win_rate <- renderValueBox({
      df   <- filtered_data() |> filter(Venue == "Home")
      rate <- paste0(round(mean(df$Result == "Win") * 100, 1), "%")
      valueBox(rate, "Home Win Rate", icon = icon("house"), color = "green")
    })
    
    output$away_win_rate <- renderValueBox({
      df   <- filtered_data() |> filter(Venue == "Away")
      rate <- paste0(round(mean(df$Result == "Win") * 100, 1), "%")
      valueBox(rate, "Away Win Rate", icon = icon("plane"), color = "orange")
    }
    shinyApp(ui = ui, server = server))
    
    # Plot 1: Home vs Away goals
    output$home_away_plot <- renderPlot({
      df <- filtered_data() |>
        group_by(Venue) |>
        summarise(Scored = mean(GF), Conceded = mean(GA), .groups = "drop") |>
        pivot_longer(c(Scored, Conceded), names_to = "Metric", values_to = "Avg")
      
      ggplot(df, aes(x = Venue, y = Avg, fill = Metric)) +
        geom_col(position = "dodge", width = 0.5) +
        labs(x = NULL, y = "Average Goals", fill = NULL) +
        theme_minimal()
    })
    
    # Plot 2: Win rate
    output$win_rate_plot <- renderPlot({
      df <- filtered_data() |>
        group_by(Venue) |>
        summarise(WinRate = mean(Result == "Win") * 100, .groups = "drop")
      
      ggplot(df, aes(x = Venue, y = WinRate, fill = Venue)) +
        geom_col(width = 0.4, show.legend = FALSE) +
        scale_y_continuous(limits = c(0, 100)) +
        labs(x = NULL, y = "Win Rate (%)") +
        theme_minimal()
    })
    
    # Plot 3: Season progression
    output$progression_plot <- renderPlot({
      df <- filtered_data() |>
        group_by(SeasonThird) |>
        summarise(AvgGoals = mean(GF), .groups = "drop") |>
        mutate(SeasonThird = factor(SeasonThird, levels = c("Early", "Mid", "Late")))
      
      ggplot(df, aes(x = SeasonThird, y = AvgGoals, group = 1)) +
        geom_line(colour = "#3c8dbc", linewidth = 1.2) +
        geom_point(size = 4, colour = "#3c8dbc") +
        labs(x = "Season Period", y = "Avg Goals Scored") +
        theme_minimal()
    })
    
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


