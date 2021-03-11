library(shiny)
library(ffscrapr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
#remotes::install_github("dynastyprocess/fpscrapr")
library(fpscrapr)

player_counts <- tibble(pos = c("QB","RB","WR","TE"), count = c(2,3,3,2))

fpRanks <- fp_rankings(page = "consensus-cheatsheets", sport = "nfl") %>%
    mutate(pos_ecr = as.integer(gsub(".*?([0-9]+).*", "\\1", pos_rank))  ) %>%
    select(player_id, pos_ecr)
dynEcr <- fp_rankings(page = "dynasty-overall", sport = "nfl") %>%
    mutate(dyn_pos_ecr = as.integer(gsub(".*?([0-9]+).*", "\\1", pos_rank))  ) %>%
    select(player_id, dyn_pos_ecr)

sleeper_league_id <- 649923060580864000


player_values <- dp_values("values-players.csv") %>%
    left_join(dp_playerids(), by = c("fp_id" = "fantasypros_id"))

league <- sleeper_connect(season = 2021, league_id = sleeper_league_id)
rosters <- ff_rosters(league) %>%
    left_join(player_values, by = c("player_id"="sleeper_id")) %>%
    mutate(fp_id = as.integer(fp_id)) %>%
    left_join(fpRanks, by = c("fp_id"="player_id")) %>%
    left_join(dynEcr, by = c("fp_id"="player_id")) %>%
    select(player_id, franchise_name, player_name, team, age, pos.x, value_2qb, pos_ecr, dyn_pos_ecr)

free_agents <- sleeper_players() %>%
    filter(pos %in% player_counts$pos) %>%
    left_join(player_values, by = c("player_id"="sleeper_id")) %>%
    select(player_id, player_name, pos.x, age, value_2qb, ecr_2qb, ecr_pos) %>%
    filter(!is.na(value_2qb)) %>%
    filter(!player_id %in% rosters$player_id)

teams <- unique(rosters$franchise_name)

ui <- fluidPage(
    titlePanel("Dynasty Positional Value vs Rest of League"),

    verticalLayout(
            HTML("<h4>Total value in position on roster (with league min and max)</h4>"),
            selectInput("team", "Team", choices = teams, multiple = FALSE),
            checkboxInput("startersOnly", "Starters Only", value = FALSE),
            plotOutput("graph"),
            HTML("<h4>Bench players in the league who could start for you this year</h4>"),
            tableOutput("tradeTargets"),
            HTML("<h4>Top valued bench players (Season ECR and Dynasty ECR is outside starting range)</h4>"),
            tableOutput("tradeAway"),
            HTML("<h4>Top valued Free agents</h4>"),
            tableOutput("topFreeAgents")
    )
)

server <- function(input, output) {
    rosters <- rosters %>%
        left_join(player_counts, by = c("pos.x" = "pos")) %>%
        group_by(pos.x) %>%
        mutate(league_pos_rank = rank(desc(value_2qb), na.last = TRUE, ties.method= "min")) %>%
        ungroup() %>%
        mutate(pos_starters_league = count*length(teams)) %>%
        mutate(pos_starters_team = count) %>%
        group_by(pos.x,franchise_name) %>%
        mutate(roster_pos_rank = rank(desc(value_2qb), na.last = TRUE, ties.method= "min"))
    
    players_included <- rosters
        
    output$graph <- renderPlot({
        if(input$startersOnly){
            players_included <- players_included %>%
                filter(roster_pos_rank <= count)
        }
        
        roster_pos_values <- players_included %>%
            summarise(pos_value=sum(value_2qb, na.rm=TRUE))
        
        selected_roster <- roster_pos_values %>%
            filter(franchise_name == input$team)
        
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
        rosterRanks <- rosters %>%
            filter(franchise_name == input$team) %>%
            group_by(pos.x) %>%
            filter(roster_pos_rank <= count) %>%
            summarise(worstStarter=max(pos_ecr))

        potentialTradeTargets <- rosters %>%
            left_join(rosterRanks, by = c("pos.x" = "pos.x")) %>%
            filter(franchise_name != input$team) %>%
            filter(pos_ecr <= pos_starters_league) %>%
            filter(roster_pos_rank > pos_starters_team) %>%
            filter(pos_ecr < worstStarter) %>%
            select(franchise_name, player_name, team, age, value_2qb, pos_ecr,dyn_pos_ecr)
        return(potentialTradeTargets)
    })
    
    output$tradeAway = renderTable({
        rosterRanks <- rosters %>%
            filter(franchise_name == input$team) %>%
            filter(dyn_pos_ecr > pos_starters_league) %>%
            filter(pos_ecr > pos_starters_league) %>%
            arrange(-value_2qb) %>%
            head(5) %>%
            select(franchise_name, player_name, team, age, value_2qb, pos_ecr,dyn_pos_ecr)
        return(rosterRanks)
    })
    
    output$topFreeAgents = renderTable({
        topFreeAgents <- free_agents %>%
            arrange(-value_2qb) %>%
            head(10)
        return(topFreeAgents)
    })
}

shinyApp(ui = ui, server = server)
