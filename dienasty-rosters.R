library(shiny)
library(ffscrapr)
library(tidyverse)
library(shinythemes)
#remotes::install_github("dynastyprocess/fpscrapr")
library(fpscrapr)

# TODO - picks in roster value
# TODO - move the heavy lifting to functions

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
    theme = shinytheme("superhero"),
    titlePanel("Dynasty Roster Analyser"),
    
    verticalLayout(
        uiOutput('team_select'),
            HTML("<h4>Roster Value</h4>"),
            checkboxInput("startersOnly", "Starters Only", value = FALSE),
            plotOutput("graph"),
            HTML("<h4>Bench players in the league who could start for you this year</h4>"),
            tableOutput("tradeTargets"),
            HTML("<h4>Top valued bench players</h4>"),
            tableOutput("tradeAway"),
            HTML("<h4>Top valued Free agents</h4>"),
            tableOutput("topFreeAgents")
    )
)

server <- function(input, output) {
    teamstate <- reactiveValues(data = NULL)
    league_id <- '649923060580864000'
    league <- sleeper_connect(season = 2021, league_id = league_id)
    league_teams <- load_rosters(league)
    league_picks <- ff_draftpicks(league)

    output$team_select = renderUI({
        teams <- league_teams %>%
            distinct(franchise_name)
        selectInput('selected_team', 'Team', teams)
    })
    
    output$graph <- renderPlot({
        if (is.null(input$selected_team)) return()
        players_included <- league_teams
        if(input$startersOnly){
            players_included <- players_included %>%
                filter(roster_pos_rank <= count)
        }
        roster_pos_values <- players_included %>%
            group_by(franchise_name, pos.x) %>%
            summarise(pos_value=sum(value_2qb, na.rm=TRUE))
        selected_roster <- roster_pos_values %>%
            filter(franchise_name == input$selected_team)
        worst <- roster_pos_values %>%
            group_by(pos.x) %>%
            summarise(franchise_name="Worst", pos_value=min(pos_value))
        best <- roster_pos_values %>%
            group_by(pos.x) %>%
            summarise(franchise_name="Best", pos_value=max(pos_value))
        ggplot(selected_roster) +
            geom_bar( aes(x=pos.x, y=pos_value), stat="identity", fill="skyblue", alpha=0.7) +
            xlab("Position") + ylab("Total Value") + 
            geom_errorbar( aes(x=worst$pos.x, ymin=worst$pos_value, ymax=best$pos_value), width=0.4, colour="orange", alpha=0.9, size=1.3)
    })
    
    output$tradeTargets = renderTable({
        if (is.null(input$selected_team)) return()
        rosters <- league_teams
        teams <- rosters %>% distinct(franchise_name)
        rosterRanks <- rosters %>%
            filter(franchise_name == input$selected_team) %>%
            group_by(pos.x) %>%
            filter(roster_pos_rank <= pos_starters_team) %>%
            summarise(worstStarter=max(pos_ecr, na.rm=TRUE))
        potentialTradeTargets <- rosters %>%
            left_join(rosterRanks, by = c("pos.x" = "pos.x")) %>%
            filter(franchise_name != input$selected_team) %>% # doesn't play on the selected team
            filter(pos_ecr <= pos_starters_league) %>% # ECR is within a starting range for the position
            filter(roster_pos_rank > pos_starters_team) %>% # they're not a starter on their current team
            filter(pos_ecr < worstStarter) %>% # they are better than the lowest ranked starter on the selected team
            select(franchise_name, player_name, team, age, value_2qb, pos_ecr,dyn_pos_ecr)
        return(potentialTradeTargets)
    })
    
    output$tradeAway = renderTable({
        if (is.null(input$selected_team)) return()
        rosters <- league_teams
        rosterRanks <- league_teams %>%
            filter(franchise_name == input$selected_team) %>%
            filter(dyn_pos_ecr > pos_starters_league) %>%
            filter(roster_pos_rank > pos_starters_team) %>% #Their ECR is outside a starting range next season
            filter(dyn_pos_ecr > pos_starters_team) %>% #Their Dynasty ECR is outside a starting range
            arrange(-value_2qb) %>%
            head(5) %>%
            select(franchise_name, player_name, team, age, value_2qb, pos_ecr,dyn_pos_ecr)
        return(rosterRanks)
    })
    
    output$draftPicks = renderTable({
        league_picks()
    })
    
    observeEvent(input$selected_team, {
        free_agents <- sleeper_players() %>%
            filter(pos %in% player_counts$pos) %>%
            left_join(player_values, by = c("player_id"="sleeper_id")) %>%
            select(player_id, player_name, pos.x, age, value_2qb, ecr_2qb, ecr_pos) %>%
            filter(!is.na(value_2qb)) %>%
            filter(!player_id %in% league_teams$player_id) %>%
            arrange(-value_2qb) %>%
            head(10)
        teamstate$free_agents <- free_agents
    })
    
    output$topFreeAgents = renderTable({
        if (is.null(teamstate$free_agents)) return()
        return(teamstate$free_agents)
    })
}

shinyApp(ui = ui, server = server)
