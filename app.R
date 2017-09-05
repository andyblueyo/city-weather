library(shiny)
library(plotly)
library(dplyr)

weather <- read.csv("data/KSEA.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("City Weather", windowTitle = "AH NOPE"),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Temp Type", choices = c("1", "2", "3"), selected = "2", multiple = TRUE),
      dateRangeInput(inputId = "date", label = "Date Range", start = "2014-10-1", end = "2015-6-8", min = "2014-10-1", max = "2015-6-9"),
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
    weather %>% filter(actual_mean_temp > input$temp[1]) %>% filter(actual_mean_temp < input$temp[2])
  })
  output$tempplot <- renderPlotly({
    x <- list(
      title = "Actual Mean Temp"
    )
    y <- list(
      title = "Actual Min Temp"
    )
    plot_ly(temp.input(), x = ~actual_mean_temp, y = ~actual_min_temp) %>% layout(xaxis = x, yaxis = y)
  })
  output$temptable <- renderTable({
    temp.input()
  })
}

shinyApp(ui = ui, server = server)