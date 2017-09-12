library(shiny)
library(plotly)
library(dplyr)

weather <- read.csv("data/KSEA.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("City Weather", windowTitle = "AH NOPE"),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Temp Type", choices = c("Max", "Min", "Mean"), selected = "Max", multiple = TRUE),
      dateRangeInput(inputId = "date", label = "Date Range", start = "2014-7-1", end = "2015-6-29", min = "2014-7-1", max = "2015-6-30"),
      sliderInput(inputId = "temp", label = "Temp Range", min = 0, max = 100, value = c(25,75))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("tempplot")),
        tabPanel("Table", tableOutput("temptable"))
      )
    )
  )
)

server <- function(input, output) {
  temp.input <- reactive({
    weather %>% filter(actual_max_temp >= input$temp[1]) %>% filter(actual_max_temp <= input$temp[2]) %>% 
      filter(date >= input$date[1]) %>%  filter(date <= input$date[2])
  })
  output$tempplot <- renderPlotly({
    x <- list(
      title = "Date",
      tickangle=45,
      zeroline = FALSE
    )
    y <- list(
      title = "Temp",
      zeroline = FALSE
    )
    m <- list(
      b = 160
    )
    plot_ly(temp.input(), x = ~date, y = ~actual_max_temp, type = 'scatter', mode = 'markers',opacity = 0.5, hoverinfo ='text', 
            text = ~paste('Date: ', date, '<br> Max Temp: ', actual_max_temp,'<br> Mean Temp:', actual_mean_temp, '<br> Min Temp: ', actual_min_temp), name = 'Max Temp') %>% 
      add_trace(y = ~actual_mean_temp, name = 'Mean Temp', opacity = 0.5) %>% 
      add_trace(y = ~actual_min_temp, name = 'Min Temp', opacity = 1) %>% 
      layout(xaxis = x, yaxis = y, barmode = 'overlay', margin = m)
  })
  output$temptable <- renderTable({
    temp.input()
  })
}

shinyApp(ui = ui, server = server)