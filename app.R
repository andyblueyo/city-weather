library(shiny)
library(plotly)
library(dplyr)

location <- read.csv("data/location.csv", stringsAsFactors = FALSE)


ui <- fluidPage(
  titlePanel("City Weather", windowTitle = "AH NOPE"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("cityOutput"),
      dateRangeInput(inputId = "date", label = "Date Range", start = "2014-7-1", end = "2015-6-29", min = "2014-7-1", max = "2015-6-30"),
      sliderInput(inputId = "temp", label = "Temp Range", min = -20, max = 150, value = c(20,75))
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
  output$cityOutput <- renderUI({
    selectInput(inputId = "cityInput", label = "City Name", choices = sort(unique(location$city)), selected = "Seattle")
  })
  myDat <- reactive({
    rightCity <- location %>% filter(input$cityInput == city)
    no <- rightCity[[2]]
    weather <- read.csv(paste0("data/", no, ".csv"), stringsAsFactors = FALSE)
    weather$date <- as.Date(weather$date, "%Y-%m-%d")
    return(weather)
  })
  temp.input <- reactive({
    myDat() %>% filter(actual_max_temp >= input$temp[1]) %>% filter(actual_max_temp <= input$temp[2]) %>% 
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
    plot_ly(temp.input(), x = ~date, y = ~actual_max_temp, type = 'scatter', mode = 'lines',opacity = 0.5, hoverinfo ='text', 
            text = ~paste('Date: ', date, '<br> Max Temp: ', actual_max_temp,'<br> Mean Temp:', actual_mean_temp, '<br> Min Temp: ', actual_min_temp), name = 'Max Temp') %>% 
      add_trace(y = ~actual_mean_temp, name = 'Mean Temp', opacity = 0.5) %>% 
      add_trace(y = ~actual_min_temp, name = 'Min Temp', opacity = 0.5) %>% 
      layout(xaxis = x, yaxis = y, title = paste("Temperature of", input$cityInput), barmode = 'overlay', margin = m)
  })
  output$temptable <- renderTable({
    temp.input()
  })
}

shinyApp(ui = ui, server = server)