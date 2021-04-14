library(shiny)
library(ffscrapr)
library(tidyverse)
#remotes::install_github("dynastyprocess/fpscrapr")
library(fpscrapr)

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
    rosters <- ff_rosters(league) %>%
        left_join(player_values, by = c("player_id"="sleeper_id")) %>%
        mutate(fp_id = as.integer(fp_id)) %>%
        left_join(fpRanks, by = c("fp_id"="player_id")) %>%
        left_join(dynEcr, by = c("fp_id"="player_id")) %>%
        select(player_id, franchise_name, player_name, team, age, pos.x, value_2qb, pos_ecr, dyn_pos_ecr)
    return (rosters)
}

ui <- fluidPage(
    titlePanel("Some Dynasty Thing"),
    
    verticalLayout(
        # Only show this panel if the plot type is a histogram
        conditionalPanel(
            condition = ("input.league_submit == 0"), # league_submit button has not been clicked
            HTML("<h4>FF Login</h4>"),
            textInput("league_id", "Sleeper League ID", "649923060580864000"),
            actionButton("league_submit", "Load League")
        ),
        uiOutput('team_select'),
        textOutput('teamage')
    )
)

server <- function(input, output) {
    teamstate <- reactiveValues(data = NULL)
    
    # reactive expression
    league_teams <- eventReactive( input$league_submit, {
        return(load_rosters(input$league_id))
    })
    
    output$team_select = renderUI({
        teams <- league_teams() %>%
            distinct(franchise_name)
        selectInput('selected_team', 'Team', teams)
    })
    
    mean_age <- eventReactive(input$selected_team, {
        print(input$selected_team)
        print("###################")
        print(league_teams() %>% filter(franchise_name == input$selected_team))
        mean_age <- league_teams() %>%
            filter(franchise_name == input$selected_team) %>%
            summarise(mean_age = mean(age, na.rm = TRUE)) %>%
            select(mean_age)
        return(mean_age)
    })
    
    mean_age <- observeEvent(input$selected_team, {
        roster_age <- league_teams() %>%
            filter(franchise_name == input$selected_team) %>%
            summarise(mean_age = mean(age, na.rm = TRUE))
        teamstate$mean_age <-roster_age$mean_age
    })
    
    output$teamage <- renderText({
        if (is.null(teamstate$mean_age)) return()
        return(paste("Roster Average Age", teamstate$mean_age))
    })
}

shinyApp(ui = ui, server = server)


