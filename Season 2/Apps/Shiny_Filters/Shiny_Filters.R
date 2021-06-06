library(shiny)
library(shinydashboard)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Filter Examples"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("All Filters", tabName = "all"),
      menuItem("Common Filters", tabName = "Common")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "all",
              
              fluidRow(
                box(checkboxInput("checkbox", label = "Is Setosa", value = TRUE), width = 4),
                box(radioButtons("radiobuttons",label = "Radio Buttons", choices = c(1, 2, 3), selected = 2), width = 4),
                box(selectInput("select", label = "Select Input", choices = c(1, 2, 3), selected = 2), width = 4)
                ),
              
              fluidRow(
                box(numericInput("numeric", label = "Numeric Input", value = 3), width = 4),
                box(textInput("text", label = "Text Input", value = "Setosa", placeholder = "Setosa"), width = 4),
                box(sliderInput("slider", label = "Slider Input", min = 0, max = 100, value = 50, step = 10), width = 4)
                ),
              
              fluidRow(
                box(dateInput("date", label = "Date Input", value = Sys.Date(), min = Sys.Date() - 365*2, 
                              max = Sys.Date() + 365*2), width = 4),
                box(dateRangeInput("daterange", label = "Date Range Input", start = Sys.Date() - 365, end = Sys.Date() + 365,
                                   min = Sys.Date() - 365*5, max = Sys.Date() + 365*5), width = 4),
                box(fileInput("file", label = "File Input", multiple = FALSE), width = 4)
              )
              ),
      
      tabItem(tabName = "Common",
              fluidRow(
                box(plotOutput("select_example"), width = 6),
                box(plotOutput("slider_example"), width = 6),
              ),
              fluidRow(
                box(selectInput("v_select", label = "Species", choices = unique(iris$Species), selected = "setosa"), width = 6),
                box(sliderInput("v_slider", label = "Sepal Length", min = min(iris$Sepal.Length), max = max(iris$Sepal.Length), 
                                value = median(iris$Sepal.Length), step = 0.5), width = 6)
                ),
              fluidRow(
                box(dateInput("v_date", label = "Date", value = as.Date("1980-01-01"), 
                              min = as.Date(min(economics$date)), max = as.Date(max(economics$date))),
                    width = 6),
                box(dateRangeInput("v_daterange", label = "Date Range", 
                                   start = as.Date(min(economics$date)), end = as.Date(max(economics$date)),
                                   min = as.Date(min(economics$date)), max = as.Date(max(economics$date))))
              ),
              fluidRow(
                box(plotOutput("date_example"), width = 6),
                box(plotOutput("daterange_example"), width = 6)
                )
              )
    )
  ) 
)

server <- function(input, output) { 
  
  output$select_example <- renderPlot({
    iris %>% 
      filter(Species == input$v_select) %>% 
      ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
      geom_point() +
      ggtitle("Select Filter")
  })
  
  output$slider_example <- renderPlot({
    iris %>% 
      filter(Sepal.Length < input$v_slider) %>% 
      ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
      geom_point() + 
      ggtitle("Slider Example")
  })
  
  output$date_example <- renderPlot({
    economics %>% 
      filter(date >= input$v_date) %>% 
      ggplot(aes(x = date, y = unemploy)) + 
      geom_line() +
      ggtitle("Date Example")
  })
  
  output$daterange_example <- renderPlot({
    economics %>% 
      filter(date >= input$v_daterange[1] & date <= input$v_daterange[2]) %>% 
      ggplot(aes(x = date, y = unemploy)) + 
      geom_line() + 
      ggtitle("Date Range Example")
  })
  
  }

shinyApp(ui, server)