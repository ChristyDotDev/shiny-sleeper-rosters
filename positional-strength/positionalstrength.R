library(shiny)
library(ffscrapr)
library(tidyverse)
library(ggplot2)
library(ggrepel)

sleeper_league_id <- 0 #FIXME - configurable or take it in through a form

player_counts <- tibble(pos = c("QB","RB","WR","TE"), count = c(2,3,3,2))

player_values <- dp_values("values-players.csv") %>%
    left_join(dp_playerids(), by = c("fp_id" = "fantasypros_id"))

league <- sleeper_connect(season = 2021, league_id = sleeper_league_id)
rosters <- ff_rosters(league) %>%
    left_join(player_values, by = c("player_id"="sleeper_id")) %>%
    select(franchise_name, player_name, team, age, pos.x, value_2qb)
teams <- unique(rosters$franchise_name)
    
ui <- fluidPage(
    titlePanel("Dynasty Positional Value vs Rest of League"),

    sidebarLayout(
        sidebarPanel(
            selectInput("team", "Team", choices = teams, multiple = FALSE),
            checkboxInput("startersOnly", "Starters Only", value = FALSE)
        ),
        mainPanel(
           plotOutput("graph"),
           tableOutput("tradeTargets")
        )
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
        potentialTradeTargets <- rosters %>%
            filter(franchise_name != input$team) %>%
            filter(league_pos_rank <= pos_starters_league) %>%
            filter(roster_pos_rank > pos_starters_team) %>%
            select(franchise_name, player_name, team, age, roster_pos_rank)
        return(potentialTradeTargets)
    })
}

shinyApp(ui = ui, server = server)
