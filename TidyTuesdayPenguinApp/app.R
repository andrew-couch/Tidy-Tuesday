library(shiny)
library(shinydashboard)
library(tidymodels)
library(tidyverse)

model <- readRDS("penguin_model.rds")


ui <- dashboardPage(
    dashboardHeader(title = "Penguin App"),
    
    dashboardSidebar(
        menuItem(
            "Penguin Species",
            tabName = "penguin_tab",
            icon = icon("snowflake")
        )
    ),
    dashboardBody(
        tabItem(
            tabName = "penguin_tab",
            box(valueBoxOutput("penguin_prediction")),
            box(selectInput("v_island", label = "Island",
                            choices = c("Dream", "Torgersen", "Biscoe"))),
            box(selectInput("v_sex", label = "Sex",
                            choices = c("male", "female"))),
            box(sliderInput("v_bill_length", label = "Bill Length (mm)",
                            min = 30, max = 60, value = 45)),
            box(sliderInput("v_bill_depth", label = "Bill Depth (mm)",
                            min = 10, max = 25, value = 17)),
            box(sliderInput("v_flipper_length", label = "Flipper Length (mm)",
                            min = 170, max = 235, value = 200)),
            box(sliderInput("v_body_mass", label = "Body Mass (g)",
                            min = 2700, max = 6300, value = 4000))
        )
    )
)

server <- function(input, output) { 
    
    output$penguin_prediction <- renderValueBox({
        
        prediction <- predict(
            model,
            tibble("island" = input$v_island,
                   "bill_length_mm" = input$v_bill_length,
                   "bill_depth_mm" = input$v_bill_depth,
                   "flipper_length_mm" = input$v_flipper_length,
                   "body_mass_g" = input$v_body_mass,
                   "sex" = input$v_sex)
        )
        
        prediction_prob <- predict(
            model,
            tibble("island" = input$v_island,
                   "bill_length_mm" = input$v_bill_length,
                   "bill_depth_mm" = input$v_bill_depth,
                   "flipper_length_mm" = input$v_flipper_length,
                   "body_mass_g" = input$v_body_mass,
                   "sex" = input$v_sex),
            type = "prob"
        ) %>% 
            gather() %>% 
            arrange(desc(value)) %>% 
            slice(1) %>% 
            select(value)
        
        prediction_color <- if_else(prediction$.pred_class == "Adelie", "blue", 
                                    if_else(prediction$.pred_class == "Gentoo", "red", "yellow"))
        
       valueBox(
           value = paste0(round(100*prediction_prob$value, 0), "%"),
           subtitle = paste0("Species: ", prediction$.pred_class),
           color = prediction_color,
           icon = icon("snowflake")
       )
        
    })
    
    }

shinyApp(ui, server)