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

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  dashboardHeader(title = "EPL Match Tracker"),
  
  dashboardSidebar(
    selectInput("team", "Select Team", choices = all_teams, selected = "Arsenal"),
    selectInput("season", "Select Season", choices = all_seasons, selected = "2023/24")
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

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Reactive calc
  filtered_data <- reactive({
    home_rows <- epl |>
      filter(HomeTeam == input$team, Season == input$season) |>
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
    
    away_rows <- epl |>
      filter(AwayTeam == input$team, Season == input$season) |>
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
          row_number() <= n() / 3     ~ "Early",
          row_number() <= 2 * n() / 3 ~ "Mid",
          TRUE                        ~ "Late"
        )
      )
  })
  
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
  })
  
  # Plot 1: Home vs Away goals
  output$home_away_plot <- renderPlot({
    df <- filtered_data() |>
      group_by(Venue) |>
      summarise(Scored = mean(GF), Conceded = mean(GA), .groups = "drop") |>
      pivot_longer(c(Scored, Conceded), names_to = "Metric", values_to = "Avg")
    
    ggplot(df, aes(x = Venue, y = Avg, fill = Metric)) +
      geom_col(position = "dodge", width = 0.5) +
      geom_text(aes(label = round(Avg, 2)),
                position = position_dodge(width = 0.5),
                vjust = -0.4, size = 3.5, fontface = "bold") +
      scale_fill_manual(values = c("Scored" = "#3c8dbc", "Conceded" = "#e74c3c")) +
      labs(x = NULL, y = "Average Goals", fill = NULL,
           subtitle = "Average goals scored and conceded per match") +
      theme_minimal()
  })
  
  # Plot 2: Win rate
  output$win_rate_plot <- renderPlot({
    df <- filtered_data() |>
      group_by(Venue) |>
      summarise(WinRate = mean(Result == "Win") * 100, .groups = "drop")
    
    ggplot(df, aes(x = Venue, y = WinRate, fill = Venue)) +
      geom_col(width = 0.4, show.legend = FALSE) +
      geom_text(aes(label = paste0(round(WinRate, 1), "%")),
                vjust = -0.4, size = 4, fontface = "bold") +
      scale_fill_manual(values = c("Home" = "#27ae60", "Away" = "#e67e22")) +
      scale_y_continuous(limits = c(0, 100),
                         labels = function(x) paste0(x, "%")) +
      labs(x = NULL, y = "Win Rate (%)",
           subtitle = "Percentage of matches won at home vs away") +
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
      geom_text(aes(label = round(AvgGoals, 2)),
                vjust = -1.2, size = 3.5, fontface = "bold") +
      labs(x = "Season Period", y = "Avg Goals Scored",
           subtitle = "Average goals scored in early, mid, and late thirds of the season") +
      theme_minimal()
  })
  
}

shinyApp(ui = ui, server = server)
