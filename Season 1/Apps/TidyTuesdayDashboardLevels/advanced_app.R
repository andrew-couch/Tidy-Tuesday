# Video: https://www.youtube.com/watch?v=PHdIivFAq7Q&list=PLJfshcspBCYeJeO8YFT5e5HxuYOb5a_1W&index=12
library(shiny)
library(tidyverse)
library(elo)
library(plotly)
library(shinydashboard)
source("dashboard_helper.R")

ui <- dashboardPage(
    dashboardHeader(title = "UFC Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Weight Class",
                     tabName = "weight_class_tab",
                     icon = icon("dashboard")),
            menuItem("Head to Head",
                     tabName = "head_tab",
                     icon = icon("dashboard")),
            menuItem("Fighter",
                     tabName = "fighter_tab",
                     icon = icon("dashboard"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "weight_class_tab",
                    box(plotlyOutput("elo_timeseries")),
                    box(plotlyOutput("elo_dist")),
                    box(dataTableOutput("top_5_table")),
                    box(uiOutput("weight_class_selector_1")),
                    box(sliderInput(inputId = "v_k_1",
                                    label = "K for ELO",
                                    min = 1, 
                                    max = 100,
                                    value = 20))
                    ),
            tabItem(tabName = "head_tab",
                    fluidRow(box(uiOutput("fighter_selector")), box(uiOutput("opponent_selector"))),
                    fluidRow(box(valueBoxOutput("fighter_card")), box(valueBoxOutput("opponent_card"))),
                    box(uiOutput("weight_class_selector_2")),
                    box(sliderInput("v_k_2",
                                    label = "K for ELO",
                                    min = 1,
                                    max = 100,
                                    value = 20))),
            tabItem(tabName = "fighter_tab",
                    fluidRow(box(uiOutput("fighter_selector2")), box(uiOutput("weight_class_selector_3"))),
                    fluidRow(box(dataTableOutput("fighter_desc")), box(plotlyOutput("fighter_radar"))))
        )
    )
 
)

server <- function(input, output) {
    helper <- reactive({
      load("advanced_dashboard_helper.Rdata")
    })
    elo_1 <- reactive(create_elo_data(input$v_k_1))
    elo_2 <- reactive(create_elo_data(input$v_k_2))
    elo <- reactive(elo.run(winner ~ fighter + opponent,
                            k = input$v_k_2,
                            data = elo_df))
    output$weight_class_selector_1 <- renderUI({
        selectInput(inputId = "v_weight_class_1",
                    label = "Weight Class",
                    choices = elo_1() %>% clean_weight_class())
        
    })
    output$weight_class_selector_2 <- renderUI({
        selectInput(inputId = "v_weight_class_2",
                    label = "Weight Class",
                    choices = elo_2() %>% clean_weight_class())
    })
    output$weight_class_selector_3 <- renderUI({
      selectInput(inputId = "v_weight_class_3",
                  label = "Weight Class",
                  choices = elo_2() %>% clean_weight_class())
    })
    output$fighter_selector <- renderUI({
        selectInput(inputId = "v_fighter",
                    label = "Fighter",
                    choices = elo_2() %>% filter(weight_class == input$v_weight_class_2) %>% clean_fighter())
    })
    output$fighter_selector2 <- renderUI({
      selectInput(inputId = "v_fighter_desc",
                  label = "Fighter",
                  choices = elo_2() %>% filter(weight_class == input$v_weight_class_3) %>% clean_fighter())
    })
    output$opponent_selector <- renderUI({
        selectInput(inputId = "v_opponent",
                    label = "Opponent",
                    choices = elo_2() %>% 
                      filter(weight_class == input$v_weight_class_2) %>% 
                      filter(fighter != input$v_fighter) %>% 
                      clean_fighter())
    })
    output$top_5_table <- renderDataTable({
        elo_1() %>% 
            filter(weight_class == input$v_weight_class_1) %>% 
            group_by(fighter) %>% 
            arrange(desc(elo)) %>% 
            slice(1) %>% 
            ungroup() %>% 
            top_n(elo, n = 5) %>% 
            arrange(desc(elo)) %>% 
            select(fighter, elo) %>% 
            mutate(rank = row_number())     
    })
    output$elo_timeseries <- renderPlotly({
        elo_timeseries_df <- elo_1() %>% filter(weight_class == input$v_weight_class_1)
      
        top_5_fighters <- elo_timeseries_df %>% 
            group_by(fighter) %>% 
            arrange(desc(elo)) %>% 
            slice(1) %>% 
            ungroup() %>% 
            top_n(elo, n = 5) %>% 
            select(fighter)
        
        ggplotly(
            ggplot(data = elo_timeseries_df, aes(x = date, y = elo)) + 
                geom_point() + 
                geom_point(data = elo_timeseries_df %>% filter(fighter %in% top_5_fighters$fighter),
                           aes(x = date, y = elo, color = fighter)) +
                theme(legend.position = "top")
        )
    })
    output$elo_dist <- renderPlotly({
        ggplotly(ggplot(data = elo_1() %>% filter(weight_class == input$v_weight_class_1), aes(x = elo)) + geom_histogram())
    })
    output$fighter_card <- renderValueBox({
        valueBox(
            value = paste(round(100*predict(elo(), data.frame(fighter = input$v_fighter, opponent = input$v_opponent)),0), "%", sep = ""),
            subtitle = paste(input$v_fighter, " Probability", sep = ""),
            color = "blue",
            icon = icon("hand-rock")
        )
    })
        output$opponent_card <- renderValueBox({
        valueBox(
            value = paste(round(100*predict(elo(), data.frame(fighter = input$v_opponent, opponent = input$v_fighter)),0), "%", sep = ""),
            subtitle = paste(input$v_opponent, " Probability", sep = ""),
            color = "red",
            icon = icon("hand-rock")
        )
    })
    output$fighter_desc <- renderDataTable({
      df %>% 
        select(fighter, height = Height_cms, reach = Reach_cms, Stance, age, wins, losses) %>% 
        filter(fighter == input$v_fighter_desc) %>% 
        rename_all(~str_replace(.x, "_", " ") %>% str_to_title) %>% 
        mutate(Fights = Wins + Losses) %>% 
        filter(Fights == max(Fights)) 
    })
    output$fighter_radar <- renderPlotly({
      radar_df <- df %>%
        select(fighter, match_id, avg_LEG_landed, avg_BODY_landed, avg_CLINCH_landed, avg_GROUND_landed, avg_HEAD_landed) %>% 
        drop_na() %>% 
        group_by(fighter) %>% 
        filter(match_id == max(match_id)) %>% 
        ungroup() %>% 
        select(-match_id) %>% 
        rename_all(~str_replace_all(.x, "avg_|_landed", "") %>% str_to_title()) %>% 
        pivot_longer(-Fighter) %>% 
        filter(Fighter == input$v_fighter_desc)
      
      plot_ly(
        type = "scatterpolar",
        r = radar_df$value,
        theta = radar_df$name,
        fill = "toself"
      ) %>% 
        layout(title = paste0(input$v_fighter_desc))
    })
}

shinyApp(ui = ui, server = server)