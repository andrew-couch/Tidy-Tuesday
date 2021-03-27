# Video: https://www.youtube.com/watch?v=gGEY82qA3BI&list=PLJfshcspBCYeJeO8YFT5e5HxuYOb5a_1W&index=9
library(shiny)
library(ggridges)
library(tidyverse)
library(shinydashboard)
library(recipes)

df <- palmerpenguins::penguins %>% drop_na()
options(scipen = 999)

table_UI <- function(id){
    DT::dataTableOutput(NS(id, "penguin_table"))
}
table_server <- function(id, df, type){
    moduleServer(id, function(input, output, session){
        output$penguin_table <- DT::renderDataTable({
            DT::datatable(
                df %>% 
                    filter(species == type) %>% 
                    select_if(is.numeric) %>% 
                    pivot_longer(everything()) %>% 
                    group_by(name) %>% 
                    summarise(Min = min(value),
                              Q1 = quantile(value, .25),
                              Median = median(value),
                              Q3 = quantile(value, .75),
                              Max = max(value),
                              Sd = sd(value)) %>% 
                    ungroup() %>% 
                    mutate_if(is.numeric, .funs = ~round(.x, digits = 0)),
                rownames = FALSE
            )
        })
    })
}
correlation_UI <- function(id){
    plotOutput(NS(id, "penguin_correlation"))
}
correlation_server <- function(id, df, type){
    moduleServer(id, function(input, output, session){
        output$penguin_correlation <- renderPlot({
            df %>% 
                filter(species == type) %>% 
                select_if(is.numeric) %>% 
                cor() %>% 
                as_tibble(rownames = "feature") %>% 
                pivot_longer(-feature) %>% 
                filter(feature > name) %>% 
                mutate(value = round(value, digits = 2)) %>% 
                ggplot(aes(x = feature, y = name, fill = value, label = value)) + 
                geom_tile(show.legend = FALSE) + 
                geom_text(color = "white") + 
                coord_flip() + 
                theme_minimal() + 
                labs(x = "", y = "")
        })
    })
}
pca_UI <- function(id){
    plotOutput(NS(id, "penguin_pca"))
}
pca_server <- function(id, df, type){
    moduleServer(id, function(input, output, session){
        output$penguin_pca <- renderPlot({
            pca <- recipe(species~., data = df) %>% 
                step_normalize(all_numeric()) %>% 
                step_pca(all_numeric(), num_comp = 2, id = "pca") %>% 
                prep() %>% 
                tidy(id = "pca") %>% 
                pivot_wider(names_from = component, values_from = value) %>% 
                select(terms, PC1, PC2)
            
            pca_data <- recipe(species~., data = df) %>% 
                step_normalize(all_numeric()) %>% 
                step_pca(all_numeric(), num_comp = 2) %>% 
                prep() %>% 
                juice() %>% 
                mutate(color = if_else(tolower(species) == tolower(type), paste0(type), "Other"))
            
            ggplot(data = pca_data, aes(x = PC1, y = PC2)) + 
                geom_point(aes(color = color)) + 
                geom_segment(data = pca, aes(x = 0, xend = PC1, y = 0, yend = PC2)) + 
                geom_text(data = pca, aes(x = PC1, y = PC2, label = terms)) + 
                theme_minimal() + 
                theme(legend.position = "top") +
                labs(color = "Species") 
        })
    })
}
pairs_UI <- function(id){
    plotOutput(NS(id, "penguin_pairs"))
}
pairs_server <- function(id, df, type){
    moduleServer(id, function(input, output, session){
        output$penguin_pairs <- renderPlot({
            df %>% 
                select(species, bill_length_mm:body_mass_g) %>% 
                mutate(species = if_else(tolower(species) == tolower(type), paste0(type), "Other")) %>%
                GGally::ggpairs(aes(color = species), progress = FALSE)
        })
    })
}

ui <- dashboardPage(
    dashboardHeader(title = "Penguin Modular"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Adelie", tabName = "adelie", icon = icon("dashboard")),
            menuItem("Chinstrap", tabName = "chinstrap", icon = icon("dashboard")),
            menuItem("Gentoo", tabName = "gentoo", icon = icon("dashboard"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "adelie",
                    fluidRow(box(table_UI("adelie_table")), box(correlation_UI("adelie_correlation"))),
                    fluidRow(box(pca_UI("adelie_pca")), box(pairs_UI("adelie_pairs")))),
            tabItem(tabName = "chinstrap",
                    fluidRow(box(table_UI("chinstrap_table")), box(correlation_UI("chinstrap_correlation"))),
                    fluidRow(box(pca_UI("chinstrap_pca")), box(pairs_UI("chinstrap_pairs")))),
            tabItem(tabName = "gentoo",
                    fluidRow(box(table_UI("gentoo_table")), box(correlation_UI("gentoo_correlation"))),
                    fluidRow(box(pca_UI("gentoo_pca")), box(pairs_UI("gentoo_pairs"))))
        )
    )
)

server <- function(input, output) { 
    
    # Adelie Tab --------------------------------------------------------------
    table_server("adelie_table", df = df, type = "Adelie")
    correlation_server("adelie_correlation", df = df, type = "Adelie")
    pca_server("adelie_pca", df = df, type = "Adelie")
    pairs_server("adelie_pairs", df = df, type = "Adelie")
    # Chinstrap ---------------------------------------------------------------
    table_server("chinstrap_table", df = df, type = "Chinstrap")
    correlation_server("chinstrap_correlation", df = df, type = "Chinstrap")
    pca_server("chinstrap_pca", df = df, type = "Chinstrap")
    pairs_server("chinstrap_pairs", df = df, type = "Chinstrap")
    # Gentoo ------------------------------------------------------------------
    table_server("gentoo_table", df = df, type = "Gentoo")
    correlation_server("gentoo_correlation", df = df, type = "Gentoo")
    pca_server("gentoo_pca", df = df, type = "Gentoo")
    pairs_server("gentoo_pairs", df = df, type = "Gentoo")
}

shinyApp(ui = ui, server = server)
