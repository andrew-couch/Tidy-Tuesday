library(shiny)
library(tidyverse)
library(elo)
library(shinydashboard)

elo_df <- read_csv("elo_df.csv")
df <- read_csv("elo.csv")
elo_data <- read_csv("elo_data.csv")

ui <- dashboardPage(
    dashboardHeader(title = "UFC Dashboard"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Weight Class",
                     tabName = "weight_class_tab",
                     icon = icon("dashboard")),
            menuItem("Head to Head",
                     tabName = "head_tab",
                     icon = icon("dashboard"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "weight_class_tab",
                    fluidRow(box(plotOutput("elo_timeseries")), box(plotOutput("elo_dist"))),
                    fluidRow(box(tableOutput("top_5_table")),
                             column(6,
                                    fluidRow(box(uiOutput("weight_class_selector_1"), width = 12)),
                                    fluidRow(box(sliderInput(inputId = "v_k_1", 
                                                             label = "K for ELO", min = 20, max = 100, value = 20, step = 20), width = 12))))
            ),
            tabItem(tabName = "head_tab",
                    fluidRow(box(uiOutput("fighter_selector")), box(uiOutput("opponent_selector"))),
                    fluidRow(box(valueBoxOutput("fighter_card")), box(valueBoxOutput("opponent_card"))),
                    fluidRow(box(uiOutput("weight_class_selector_2")),
                             box(sliderInput("v_k_2", label = "K for ELO", min = 20, max = 100, value = 20, step = 20)),
                             box(actionButton("v_button", label = 'Predict'))))
            
        )
    )
    
    
)

server <- function(input, output) {
    
    elo_data_1 <- reactive({
        elo_data %>% 
        filter(k == input$v_k_1) %>% 
        filter(weight_class == input$v_weight_class_1)
      
    }) %>% bindEvent(input$v_weight_class_1, input$v_k_1)
    
    elo_data_2 <- reactive({
        elo_data %>% 
        filter(k == input$v_k_2) %>% 
        filter(weight_class == input$v_weight_class_2) 
    }) %>% bindEvent(input$v_weight_class_2, input$v_k_2)
    
    fighter_run <- eventReactive(input$v_button, {
        elo <- elo.run(winner ~ fighter + opponent,
                       k = input$v_k_2,
                       data = elo_df)
        
        round(100*predict(elo, data.frame(fighter = input$v_fighter, opponent = input$v_opponent)),0)
    })
    
    opponent_run <- eventReactive(input$v_button, {
        elo <- elo.run(winner ~ fighter + opponent,
                       k = input$v_k_2,
                       data = elo_df)
        
        round(100*predict(elo, data.frame(fighter = input$v_opponent, opponent = input$v_fighter)),0)
    })
    
    output$weight_class_selector_1 <- renderUI({
        
        selectInput(inputId = "v_weight_class_1",
                    label = "Weight Class",
                    choices = sort(unique(elo_data$weight_class)))
    })
    
    output$weight_class_selector_2 <- renderUI({
        
        selectInput(inputId = "v_weight_class_2",
                    label = "Weight Class",
                    choices = sort(unique(elo_data$weight_class)))
    })
    output$fighter_selector <- renderUI({
        
        fighter_selector_df <- elo_data_2() %>% 
            select(fighter) %>% 
            distinct() %>% 
            arrange(fighter)
        
        selectInput(inputId = "v_fighter",
                    label = "Fighter",
                    choices = fighter_selector_df)
    })
    
    output$opponent_selector <- renderUI({
        
        selectInput(inputId = "v_opponent",
                    label = "Opponent",
                    choices = sort(unique(elo_data_2()[which(elo_data_2()$fighter != input$v_fighter),]$fighter)))
        
        
    })
    output$top_5_table <- renderTable({
        
        req(elo_data_1())
        
      elo_data_1() %>% 
            group_by(fighter) %>% 
            arrange(desc(elo)) %>% 
            slice(1) %>% 
            ungroup() %>% 
            top_n(elo, n = 5) %>% 
            arrange(desc(elo)) %>% 
            select(fighter, elo) %>% 
            mutate(rank = row_number())
        
        
    }) %>% bindCache(input$v_weight_class_1, input$v_k_1)
    
    output$elo_timeseries <- renderPlot({
        
        req(elo_data_1())
        
        elo_timeseries_df <- elo_data_1()
        
        top_5_fighters <- elo_timeseries_df %>% 
            group_by(fighter) %>% 
            arrange(desc(elo)) %>% 
            slice(1) %>% 
            ungroup() %>% 
            top_n(elo, n = 5) %>% 
            select(fighter)
        
        ggplot(data = elo_timeseries_df, aes(x = date, y = elo)) + 
            geom_point() + 
            geom_point(data = elo_timeseries_df %>% filter(fighter %in% top_5_fighters$fighter),
                       aes(x = date, y = elo, color = fighter)) +
            theme(legend.position = "top")
        
        
    }) %>% bindCache(input$v_weight_class_1, input$v_k_1)
    
    output$elo_dist <- renderPlot({
        
        req(elo_data_1())
        
        ggplot(data = elo_data_1(), aes(x = elo)) + 
            geom_histogram()
    }) %>% bindCache(input$v_weight_class_1, input$v_k_1)
    
    output$fighter_card <- renderValueBox({
        
        req(fighter_run())
        
        valueBox(
            value = paste(fighter_run(), "%", sep = ""),
            subtitle = paste(input$v_fighter, " Probability", sep = ""),
            color = "blue",
            icon = icon("hand-rock")
        )
        
    })
    
    output$opponent_card <- renderValueBox({
        
        req(opponent_run())
        
        valueBox(
            value = paste(opponent_run(), "%", sep = ""),
            subtitle = paste(input$v_opponent, " Probability", sep = ""),
            color = "red",
            icon = icon("hand-rock")
        )
        
    })
    
    
}

shinyApp(ui = ui, server = server)
