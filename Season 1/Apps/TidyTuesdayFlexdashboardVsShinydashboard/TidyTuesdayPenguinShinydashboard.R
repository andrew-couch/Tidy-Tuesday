# Video: https://www.youtube.com/watch?v=AVmfWL43B70&list=PLJfshcspBCYeJeO8YFT5e5HxuYOb5a_1W&index=8&t=7s
library(shiny)
library(shinydashboard)
library(palmerpenguins)
library(tidyverse)
library(ggridges)



df <- penguins

ui <- dashboardPage(
  dashboardHeader(title = "Penguin Shinydashboard"),
  dashboardSidebar(selectInput("v_species", label = "Species", choices = c("All", "Adelie", "Chinstrap", "Gentoo"), selected = "All")),
  dashboardBody(
    fluidRow(box(plotOutput("penguin_ridges"))),
    fluidRow(box(plotOutput("penguin_box")), box(plotOutput("penguin_col")))
  )
)

server <- function(input, output) { 
  
  output$penguin_ridges <- renderPlot({
    df %>% 
      select(-island, -sex) %>% 
      pivot_longer(-species) %>% 
      group_by(name) %>% 
      mutate(value = scale(value)) %>% 
      ungroup() %>% 
      filter(str_detect(species, if_else(input$v_species == "All", "", input$v_species))) %>% 
      ggplot(aes(x = value, y = name, fill = species)) + 
      geom_density_ridges(alpha = .7) + 
      theme(legend.position = "top") + 
      labs(y = "", x = "")
  })
  
  output$penguin_box <- renderPlot({
    df %>% 
      select(-island, -sex) %>% 
      pivot_longer(-species) %>% 
      group_by(name) %>% 
      mutate(value = scale(value)) %>% 
      ungroup() %>% 
      ggplot(aes(x = name, y = value, color = species)) + 
      geom_boxplot() + 
      theme(legend.position = "top") + 
      labs(x = "", y = "")
  })
  
  output$penguin_col <- renderPlot({
    df %>% 
      count(species, island) %>% 
      drop_na() %>% 
      group_by(species) %>% 
      mutate(n = n / sum(n)) %>% 
      ungroup() %>% 
      ggplot(aes(x = species, y = n, fill = island)) + 
      geom_col() + 
      theme(legend.position = "top") + 
      scale_y_continuous(labels = scales::label_percent()) + 
      labs(y = "", x = "")
  })
  
  }

shinyApp(ui, server)