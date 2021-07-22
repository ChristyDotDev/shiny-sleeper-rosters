#install.packages("ffsimulator", repos = "https://dynastyprocess.r-universe.dev")
library(ffsimulator)
library(ggplot2)
library(ffscrapr)
library(tidyverse)
library(shinythemes)
library(Rglpk)
library(ggridges)
library(shiny)
library(shinythemes)

league_id <- '649923060580864000'
conn <- sleeper_connect(season = 2021, league_id = league_id)

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("League Simulator"),
  conditionalPanel(
    condition = "output.loaded == 'LOADED'",
    uiOutput('output'),
    titlePanel("Positional MVPs"),
    uiOutput('mvp')
  ),
  conditionalPanel(
    condition = "output.loaded != 'LOADED'",
    titlePanel("Loading..."),
    textOutput('loaded')
  )
)

server <- function(input, output) {
  print("Running single season sim")
  weeks_num <- 15
  sims <- ff_simulate(conn = conn, n_seasons = 1, n_weeks = weeks_num, best_ball = FALSE, injury_model='simple')
  #print(sims)
  season_summary <- sims$summary_simulation %>%
    select(franchise_name, h2h_wins, points_for, potential_points) %>%
    mutate(missed_potential = 100 - ((points_for/potential_points)*100)) %>%
    arrange(-h2h_wins, -points_for)
  names(season_summary) <- c('Franchise Name', 'Wins', 'Points For', 'Poential Points For', 'Missed Potential pct')

  output$output <- renderTable({
    return(tibble(season_summary))
  })
  
  print(sims$roster_scores)
  mvps <- sims$roster_scores %>%
    group_by(player_id) %>%
    summarise(player=first(player_name), points=sum(projected_score), pos=first(pos), team=first(franchise_name)) %>%
    arrange(-points) %>%
    group_by(pos) %>%
    arrange(-points) %>%
    summarise(player=first(player), points=first(points), pos=first(pos), team=first(team))
  output$mvp <- renderTable({
    return(tibble(mvps))
  })
  
  output$loaded <- renderText('LOADED')
}

shinyApp(ui = ui, server = server)