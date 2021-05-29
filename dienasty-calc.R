library(shiny)
library(ffscrapr)
library(tidyverse)
library(shinythemes)
#remotes::install_github("dynastyprocess/fpscrapr")
library(fpscrapr)
library(DT)

player_counts <- tibble(pos = c("QB","RB","WR","TE"), count = c(2,3,3,2))

fpRanks <- fp_rankings(page = "consensus-cheatsheets", sport = "nfl") %>%
    mutate(pos_ecr = as.integer(gsub(".*?([0-9]+).*", "\\1", pos_rank))  ) %>%
    select(player_id, pos_ecr)
dynEcr <- fp_rankings(page = "dynasty-overall", sport = "nfl") %>%
    mutate(dyn_pos_ecr = as.integer(gsub(".*?([0-9]+).*", "\\1", pos_rank))  ) %>%
    select(player_id, dyn_pos_ecr)

player_values <- dp_values("values-players.csv") %>%
    left_join(dp_playerids(), by = c("fp_id" = "fantasypros_id"))

load_rosters <- function(league, teams) {
    ffrosters <- ff_rosters(league)
    teams <-ffrosters %>%
        distinct(franchise_name)
    rosters <- ffrosters %>%
        left_join(player_values, by = c("player_id"="sleeper_id")) %>%
        mutate(fp_id = as.integer(fp_id)) %>%
        left_join(fpRanks, by = c("fp_id"="player_id")) %>%
        left_join(dynEcr, by = c("fp_id"="player_id")) %>%
        select(player_id, franchise_name, player_name, team, age, pos.x, value_2qb, pos_ecr, dyn_pos_ecr) %>%
        left_join(player_counts, by = c("pos.x" = "pos")) %>%
        group_by(pos.x) %>%
        mutate(league_pos_rank = rank(desc(value_2qb), na.last = TRUE, ties.method= "min")) %>%
        ungroup() %>%
        mutate(pos_starters_league = count* nrow(teams) ) %>%
        mutate(pos_starters_team = count) %>%
        group_by(pos.x,franchise_name) %>%
        mutate(roster_pos_rank = rank(desc(value_2qb), na.last = TRUE, ties.method= "min")) %>%
        ungroup()
    
    return (rosters)
}

ui <- fluidPage(
    #theme = shinytheme("superhero"),
    titlePanel("Dynasty Trade Analyser"),
    
    column(6,
        uiOutput('team_1_selected'),
        uiOutput('team_select_1'),
        DT::dataTableOutput("team_assets_1")
    ),
    column(6, 
        uiOutput('team_2_selected'),
        uiOutput('team_select_2'),
        DT::dataTableOutput("team_assets_2")
    )
)

server <- function(input, output) {
    teamstate <- reactiveValues(data = NULL)
    league_id <- '649923060580864000'
    league <- sleeper_connect(season = 2021, league_id = league_id)
    league_teams <- load_rosters(league)
    league_picks <- ff_draftpicks(league)
    
    output$team_select_1 = renderUI({
        teams <- league_teams %>%
            distinct(franchise_name)
        selectInput('selected_team_1', 'Team', teams)
    })
    output$team_select_2 = renderUI({
        teams <- league_teams %>%
            distinct(franchise_name)
        selectInput('selected_team_2', 'Team', teams)
    })
    
    observeEvent(input$selected_team_1, {
        team_assets <- league_teams %>%
            filter(franchise_name == input$selected_team_1) %>%
            select(player_id,player_name, team,age, pos.x, value_2qb) %>%
            arrange(-value_2qb)
        print(team_assets)
        teamstate$team_assets_1 <- team_assets
    })
    
    observeEvent(input$selected_team_2, {
        team_assets <- league_teams %>%
            filter(franchise_name == input$selected_team_2) %>%
            select(player_id,player_name, team,age, pos.x, value_2qb) %>%
            arrange(-value_2qb)
        print(team_assets)
        teamstate$team_assets_2 <- team_assets
    })
    
    
    output$team_assets_1 = DT::renderDataTable({
        if (is.null(teamstate$team_assets_1)) return()
        dt <- DT::datatable(teamstate$team_assets_1, options = list(paging=FALSE,autofill=TRUE))
        return(dt)
    })
    
    output$team_assets_2 = DT::renderDataTable({
        if (is.null(teamstate$team_assets_2)) return()
        dt <- DT::datatable(teamstate$team_assets_2, options = list(paging=FALSE,autofill=TRUE))
        return(dt)
    })
    
    observeEvent(input$team_assets_1_rows_selected, {
        teamstate$team_1_selected = teamstate$team_assets_1[input$team_assets_1_rows_selected, ]
    })
    
    observeEvent(input$team_assets_2_rows_selected, {
        teamstate$team_2_selected = teamstate$team_assets_2[input$team_assets_2_rows_selected, ]
    })
    
    output$team_1_selected = renderText({
        return(sum(teamstate$team_1_selected$value_2qb))
    })
    
    output$team_2_selected = renderText({
        return(sum(teamstate$team_2_selected$value_2qb))
    })
}

shinyApp(ui = ui, server = server)
