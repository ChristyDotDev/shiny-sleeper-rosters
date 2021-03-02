library(shiny)
library(ffscrapr)
library(tidyverse)
library(ggplot2)
library(ggrepel)

player_values <- dp_values("values-players.csv")
players <- player_values$player

ui <- fluidPage(
    titlePanel("Value vs ECR"),
    sidebarLayout(
        sidebarPanel(
            selectInput("player", "Player", choices = players, multiple = TRUE)
        ),
        mainPanel(
           plotOutput("graph")
        )
    )
)

server <- function(input, output) {
    output$graph <- renderPlot({
        values <- player_values %>%
            filter(player %in% input$player) %>%
            select(player, value_2qb, ecr_2qb)

        values %>%
            ggplot(aes(x = ecr_2qb, y = value_2qb)) +
            geom_point(alpha = .6) +
            geom_text_repel(aes(label = player)) +
            stat_smooth(geom = 'line', alpha = 0.5, se = FALSE, method = 'lm') +
            labs(x = "ECR",
                 y = "Trade Value",
                 caption = "Data: DynastyProcess") +
            theme_bw() +
            theme(
                aspect.ratio = 9 / 16,
                plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
            ) + xlim(0, max(values$ecr_2qb, na.rm = TRUE) + 5)
    })
}

shinyApp(ui = ui, server = server)
