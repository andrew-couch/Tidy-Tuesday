#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
coin_flip <- function(n, p){
    
    df <- tibble("sim" = runif(n, min = 0, max = 1)) %>% 
        mutate("result" = if_else(sim <= p, "H", "T"))
    
    num_heads <- df %>% 
        count(result) %>% 
        filter(result == "H") %>% 
        select(n) %>% 
        as.numeric()
    
    prop_df <- tibble("prob" = seq(from = 0, to = 1, by = .01)) %>% 
        mutate("estimate" = dbinom(x = num_heads, size = nrow(df), prob = prob))
    
    
    return(prop_df)
    
}



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("flip",
                        "Number of flips:",
                        min = 1,
                        max = 25,
                        value = 10),
            sliderInput(head_prob,
                        "The true probability of flipping 1 head",
                        min = 0,
                        max = 1,
                        value = .7)
           
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("flip_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$flip_plot <- renderPlot({
        
        results <- coin_flip(input$flip, input$head_prob)
        
        results %>% 
            ggplot(aes(x = prob, y = estimate)) + 
            geom_area(alpha = .1) + 
            geom_line()
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
