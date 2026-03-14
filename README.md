# EPL Match Tracker — R Shiny

An interactive R Shiny dashboard for exploring English Premier League (EPL) match performance by team and season. This is an individual assignment re-implementation of the [Group 6 EPL Match Tracker](https://github.com/UBC-MDS/DSCI-532_2026_6_EPL_match_tracker), originally built in Python Shiny.

## Live App

🔗 [Click here to open the dashboard](#) ← will be updated after deployment

------------------------------------------------------------------------

## What the App Does

Select any **team** and **season** from the sidebar to explore:

-   **Home vs Away Performance** — average goals scored and conceded by venue
-   **Win Rate: Home vs Away** — win percentage at home vs on the road
-   **Season Progression** — how average goals trend across Early, Mid, and Late thirds of the season
-   **Summary value boxes** — total matches, home win rate, and away win rate

------------------------------------------------------------------------

## Installation

Make sure you have R (≥ 4.1) installed. Then install the required packages in R:

``` r
install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "lubridate", "tidyr"))
```

------------------------------------------------------------------------

## Running Locally

1.  Clone the repository:

``` bash
git clone https://github.com/gurleenlakhman/EPL_Shiny_R.git
cd EPL_Shiny_R
```

2.  Open `app.R` in RStudio and click **Run App**, or run in the R console:

``` r
shiny::runApp()
```

The app will open at `http://127.0.0.1:<port>` in your browser.

------------------------------------------------------------------------

## Data

`data/epl_final.csv` contains EPL match records from the 2000/01 through 2024/25 seasons, covering 46 clubs across 25 seasons (\~9,380 matches).

------------------------------------------------------------------------

## About

Created by **Gurleen Kaur** as part of DSCI 532 (Data Visualization II) at UBC's Master of Data Science programme, March 2026.
