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