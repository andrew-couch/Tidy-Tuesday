# Video: https://youtu.be/ARd5IldVFUs
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(tidyverse)
library(tidymodels)
library(ggridges)
source("modules.R")
load("Final_Model.RDS")
fighter_df <- read_csv("fighter_table.csv")
fighter_hist <- read_csv("fight_data.csv")
future_cards <- read_csv("future_card_predictions.csv")

ui <- dashboardPage(
  dashboardHeader(title = "UFC Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fighter Summary", tabName = "summary_tab", icon = icon("dashboard")),
      menuItem("Fight Predictions", tabName = "prediction_tab", icon = icon("dashboard")),
      menuItem("Upcoming Card Predictions", tabName = "future_tab", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary_tab",
              fluidRow(outcome_ui("outcome_plot"), elo_ui("elo_plot")),
              fluidRow(component_ui("component_plot"), best_ui("best_plot"))),
      tabItem(tabName = "prediction_tab",
              fluidRow(box(uiOutput("fighter_selector")), box(uiOutput("weight_class_selector")), box(uiOutput("opponent_selector"))),
              fluidRow(box(valueBoxOutput("fighter_card")), box(valueBoxOutput("opponent_card")))),
      tabItem(tabName = "future_tab",
              fluidRow(box(uiOutput("card_selector"))),
              fluidRow(card_prediction_ui("card_predictions")),
              fluidRow(card_probabilities_ui("card_probabilities")))
      )
  )
)

server <- function(input, output) {
  
  output$weight_class_selector <- renderUI({
    selectInput(inputId = "v_weight_class", label = "Weight Class", choices = fighter_df %>% select(weight_class) %>% distinct())
  })
  output$fighter_selector <- renderUI({
    selectInput(inputId = "v_fighter", label = "Fighter", choices = fighter_df %>% filter(weight_class == input$v_weight_class) %>% 
                  select(fighter) %>% arrange(fighter))
  })
  output$opponent_selector <- renderUI({
    selectInput(inputId = "v_opponent",
                label = "Opponent",
                choices = fighter_df %>% filter(weight_class == input$v_weight_class) %>% filter(fighter != input$v_fighter) %>% 
                  select(fighter) %>% arrange(fighter))
  })
  
  output$card_selector <- renderUI({
    selectInput(inputId = "v_card",
                label = "Card",
                choices = future_cards %>% select(cards) %>% distinct())
  })
  
  
  card_df <- reactive(future_cards %>% filter(card == input$v_card))
  
  fighter_input <- reactive(
    fighter_df %>% 
      filter(weight_class == input$v_weight_class) %>% 
      filter(fighter == input$v_fighter) %>% 
      filter(fight_pk == max(fight_pk)) %>% 
      left_join(fighter_df %>% 
                  filter(weight_class == input$v_weight_class) %>% 
                  filter(fighter == input$v_opponent) %>% 
                  filter(fight_pk == max(fight_pk)) %>% 
                  rename_all(~paste0("opp_", .x)),
                by = c("fight_pk" = "opp_fight_pk")))
  
  
  fighter_components <- reactive(get_fighter_components(fighter_input()))
  opponent_components <- reactive(get_opponent_components(fighter_input()))
  fight_outcome <- reactive(get_fight_outcome(outcome_model, fighter_components(), opponent_components()))
  
  
  output$fighter_card <- renderValueBox({
    prob_df <- fight_outcome() %>% filter(name == "W") %>% select(avg) %>% pluck(1)
    
    valueBox(
      value = paste0(round(100*as.numeric(prob_df)), "%"),
      subtitle = "Probability",
      color = "blue",
      icon = icon("hand-rock")
    )
  })
  
  output$opponent_card <- renderValueBox({
    
    prob_df <- fight_outcome() %>% filter(name == "L") %>% select(avg) %>% pluck(1)
    
    valueBox(
      value = paste0(round(100*as.numeric(prob_df)), "%"),
      subtitle = "Probability",
      color = "red",
      icon = icon("hand-rock")
    )
  })

  card_prediction_server("card_predictions", df = future_cards)
  card_probabilities_server("card_probabilities", df = future_cards)
  outcome_server("outcome_plot", df = fighter_hist)
  elo_server("elo_plot", df = fighter_df)
  component_server("component_plot", df = fighter_hist)
  best_server("best_plot", df = fighter_hist)
  
}

shinyApp(ui = ui, server = server)


