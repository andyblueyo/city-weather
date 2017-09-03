library(shiny)
library(plotly)
library(dplyr)

weather <- read.csv("data/KSEA.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("City Weather", windowTitle = "AH NOPE"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(inputId = "date", label = "Date Range", start = "2014-10-1", end = "2015-6-8", min = "2014-10-1", max = "2015-6-9"),
      sliderInput(inputId = "temp", label = "Temp Range", min = 0, max = 100, value = c(25,75))
    ),
    mainPanel(
      plotlyOutput("tempplot"),
      br(),
      tableOutput("temptable"))
  )
)

server <- function(input, output) {
  output$tempplot <- renderPlotly({
    temp.input <- weather %>% filter(actual_mean_temp > input$temp[1]) %>% filter(actual_mean_temp < input$temp[2])
    plot_ly(temp.input, x = ~actual_mean_temp, y = ~actual_min_temp)
  })
  output$temptable <- renderTable({
    temp.input <- weather %>% filter(actual_mean_temp > input$temp[1]) %>% filter(actual_mean_temp < input$temp[2])
    temp.input
  })
}

shinyApp(ui = ui, server = server)