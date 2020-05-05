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

coin_flip <- function(flips, prob) {
    
    df <- tibble("sim" = runif(flips, min = 0, max = 1)) %>% 
        mutate("flip" = if_else(sim <= prob, "H", "T"))
    
    num_of_heads <- df %>% 
        count(flip) %>% 
        filter(flip == "H") %>% 
        select(n) %>% 
        as.numeric()
    
    est_df <- tibble("estimate" = seq(from = 0, to = 1, by = .01)) %>% 
        mutate(prob = dbinom(num_of_heads, size = flips, prob = estimate))
    
    
    return(est_df)
    
}




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("flips",
                        "Number of flips:",
                        min = 1,
                        max = 100,
                        value = 5),
        sliderInput("prob",
                    "Probability of flipping 1 head:",
                    min = 0,
                    max = 1,
                    value = .7)
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("flipplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$flipplot <- renderPlot({
        
        coinplot_df <- coin_flip(flips = input$flips, prob = input$prob)
        
        coinplot_df %>% 
            ggplot(aes(x = estimate, y = prob)) + 
            geom_area(alpha = .1) + 
            geom_line() + 
            geom_segment(data = coinplot_df %>% filter(estimate == input$prob)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
