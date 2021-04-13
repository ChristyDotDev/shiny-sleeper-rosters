library(shiny)
library(ffscrapr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
#remotes::install_github("dynastyprocess/fpscrapr")
library(fpscrapr)

#TODO - select league type brings up login form based on league type
#TODO - login submit pulls some team info

fpRanks <- fp_rankings(page = "consensus-cheatsheets", sport = "nfl") %>%
    mutate(pos_ecr = as.integer(gsub(".*?([0-9]+).*", "\\1", pos_rank))  ) %>%
    select(player_id, pos_ecr)
dynEcr <- fp_rankings(page = "dynasty-overall", sport = "nfl") %>%
    mutate(dyn_pos_ecr = as.integer(gsub(".*?([0-9]+).*", "\\1", pos_rank))  ) %>%
    select(player_id, dyn_pos_ecr)

player_values <- dp_values("values-players.csv") %>%
    left_join(dp_playerids(), by = c("fp_id" = "fantasypros_id"))

load_rosters <- function(sleeper_league_id) {
    league <- sleeper_connect(season = 2021, league_id = sleeper_league_id)
    print(league)
    rosters <- ff_rosters(league) %>%
        left_join(player_values, by = c("player_id"="sleeper_id")) %>%
        mutate(fp_id = as.integer(fp_id)) %>%
        left_join(fpRanks, by = c("fp_id"="player_id")) %>%
        left_join(dynEcr, by = c("fp_id"="player_id")) %>%
        select(player_id, franchise_name, player_name, team, age, pos.x, value_2qb, pos_ecr, dyn_pos_ecr)
    print("league loaded")
    return (rosters)
}

ui <- fluidPage(
    titlePanel("Some Dynasty Thing"),
    
    verticalLayout(
        HTML("<h4>FF Login</h4>"),
        textInput("league_id", "Sleeper League ID", "649923060580864000"),
        tableOutput("tradeTargets"),
        actionButton("league_submit", "Load League")
    )
)

server <- function(input, output) {
    league_info_loaded <- FALSE
    # reactive expression
    league_teams <- eventReactive( input$league_submit, {
        league_info_loaded <- TRUE
        return(load_rosters(input$league_id))
    })
    
    output$tradeTargets = renderTable({
        rosters <- league_teams() 
        teams <- rosters %>%
            distinct(franchise_name)
        print(teams)
        return(teams)
    })
}

shinyApp(ui = ui, server = server)


