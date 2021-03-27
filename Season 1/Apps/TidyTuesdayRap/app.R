
library(shiny)
library(tidyverse)

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

rating_matrix <- polls %>% 
    select(rank, title, critic_name) %>% 
    arrange(title) %>% 
    pivot_wider(names_from = "title", values_from = "rank") %>% 
    as.matrix() %>% 
    as("realRatingMatrix")
final_engine <- Recommender(rating_matrix, "UBCF", param = list(nn = 8))

songs <- polls %>% 
    select(title) %>% 
    distinct() %>% 
    arrange(title) 

ui <- fluidPage(

    titlePanel("Tidy Tuesday Rap Dashboard"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
            ),
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {

    output$distPlot <- renderPlot({
        polls %>% 
            count(artist, sort = TRUE) %>% 
            slice(1:input$bins) %>% 
            ggplot(aes(x = reorder(artist, n), y = n, fill = artist)) + 
            geom_col() + 
            coord_flip() + 
            xlab("") + 
            ylab("Votes") + 
            theme(legend.position = "none")
    })
}

shinyApp(ui = ui, server = server)

