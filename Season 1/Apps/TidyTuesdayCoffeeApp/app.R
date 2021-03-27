## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(broom)
library(kableExtra)

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')



ui <- dashboardPage(
    dashboardHeader(title = "Coffee Dashboard"),
    
    dashboardSidebar(
        selectInput("v_country", "Country", choices = coffee_ratings %>% 
                        select(country_of_origin) %>% 
                        distinct() %>% 
                        arrange(country_of_origin) %>% 
                        drop_na())
    ),
    dashboardBody(
        fluidRow(box(plotOutput("coffee_flavour")), box(plotOutput("coffee_variety"))),
        fluidRow(box(plotOutput("coffee_dif")), box(tableOutput("coffee_table")))
    )
)

server <- function(input, output) { 
    
    output$coffee_flavour <- renderPlot({
        
        coffee_ratings %>% 
            filter(country_of_origin == input$v_country) %>% 
            select(aroma:cupper_points) %>% 
            gather() %>% 
            group_by(key) %>% 
            summarise(value = mean(value)) %>% 
            ungroup() %>% 
            mutate(key = str_replace(key, "_", " ") %>% str_to_title()) %>% 
            mutate(key = fct_reorder(key, value)) %>% 
            ggplot(aes(x = key, y = value, color = key)) + 
            geom_point(size = 5) + 
            geom_segment(aes(x = key, xend = key, y = value, yend = 0)) + 
            theme(legend.position = "none") + 
            ylab("") + 
            xlab("") + 
            coord_flip() + 
            labs(title = "Avg Flavour Profile")
    })
    
    output$coffee_variety <- renderPlot({
        
        coffee_ratings %>% 
            filter(country_of_origin == input$v_country) %>% 
            select(variety) %>% 
            drop_na() %>% 
            count(variety) %>% 
            mutate(variety = fct_reorder(variety, n)) %>% 
            ggplot(aes(x = n, y = variety, fill = variety)) + 
            geom_col() + 
            ylab("") + 
            labs(title = "Bean Variety") + 
            theme(legend.position = "none")
    })
    
    
    output$coffee_dif <- renderPlot({
        coffee_ratings %>% 
            select(country_of_origin, aroma:cupper_points) %>% 
            mutate(highlight = if_else(country_of_origin == input$v_country, "Highlight", "No-Highlight")) %>% 
            select(-country_of_origin) %>% 
            gather(key = "key", value = "value", -highlight) %>% 
            group_by(key) %>% 
            do(t_test = t.test(value~highlight, data = .) %>% tidy()) %>% 
            unnest(t_test) %>% 
            mutate(diffference = case_when(
                conf.low < 0 & conf.high < 0 ~ "Different",
                conf.low > 0 & conf.high > 0 ~ "Different",
                TRUE ~ "Not-Different"
            )) %>% 
            mutate(key = str_replace(key, "_", " ") %>% str_to_title()) %>% 
            mutate(key = fct_reorder(key, -estimate)) %>% 
            ggplot(aes(x = key, y = estimate, color = diffference)) + 
            geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) + 
            geom_hline(yintercept = 0, linetype = "dashed") + 
            coord_flip() + 
            theme(legend.position = "none") + 
            xlab("") + 
            labs(title = "How different are the flavour profiles?")
    })
    
    
    output$coffee_table <- function(){
        
        coffee_ratings %>% 
            filter(country_of_origin == input$v_country) %>% 
            select(points = total_cup_points, species, country = country_of_origin, region) %>% 
            group_by(species, region) %>% 
            arrange(desc(points)) %>% 
            slice(1) %>% 
            ungroup() %>% 
            mutate(region = str_trunc(region, 12, "right")) %>% 
            arrange(desc(points)) %>% 
            kable() %>% 
            kable_styling() %>% 
            scroll_box(height = "400px", width = "300px")
    }
    
    }

shinyApp(ui, server)
