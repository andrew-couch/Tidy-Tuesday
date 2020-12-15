library(shiny)
library(tidyverse)

df <- read_csv("fight_data.csv")

fighter_df <- df %>% 
    mutate(weight_class = str_replace(weight_class, "_", " ") %>% str_to_title()) %>% 
    select(fighter, weight_class, res, kd, kds_received, strike_attempts, strike_landed, strikes_avoided, strikes_received, 
           td_landed, tds_defended, tds_received) %>% 
    mutate(wins = if_else(res == "W", 1, 0),
           losses = if_else(res == "L", 1, 0)) %>% 
    group_by(fighter, weight_class) %>% 
    summarise(across(kd:losses, sum)) %>% 
    ungroup()


ui <- fluidPage(

    titlePanel("Fighter"),
    sidebarLayout(
        sidebarPanel(
            selectInput("weightclass_id", label = "Weight Class", 
                        choices = fighter_df %>% 
                            select(weight_class) %>% 
                            distinct() %>%
                            arrange(weight_class) %>% 
                            as.list()),
            plotOutput("fighter_dif")
        ),
        mainPanel(
           dataTableOutput("fighter_summary")
        )
    )
)



server <- function(input, output) {
    
    
    output$fighter_summary <- renderDataTable({
        fighter_df %>% 
            filter(weight_class == input$weightclass_id) %>% 
            select(-strike_attempts) %>% 
            mutate(total_fights = wins + losses) %>% 
            mutate_if(is.numeric, as.integer) %>% 
            select(fighter, wins, losses, 
                   strikes = strike_landed, kd, td = td_landed) %>% 
            rename_all(~str_to_title(.x))
    })
    
    output$fighter_dif <- renderPlot({
        fighter_df %>% 
            filter(weight_class == input$weightclass_id) %>% 
            select(fighter, 
                   strike_landed, strike_received = strikes_received, 
                   td_landed, td_received = tds_received) %>% 
            pivot_longer(-fighter) %>% 
            separate(name, c("type", "outcome"), sep = "_") %>% 
            pivot_wider(names_from = outcome, values_from = value, values_fill = 0) %>% 
            ggplot(aes(x = received, y = landed)) + 
            geom_abline(color = "red", linetype = "dashed") + 
            geom_point() + 
            geom_rug() + 
            facet_wrap(~type, scales = "free")
    })

}

shinyApp(ui = ui, server = server)



