library(shiny)
library(plotly)
library(dplyr)
library(lazyeval)
library(leaflet)
library(maps)
library(htmltools)
library(weathermetrics)

location <- read.csv("data/location.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("City Weather", windowTitle = "Fun with Data LOL"),
  p("This is an interactive data visualization focused on data from the ", 
    tags$a(html = "https://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/", "What 12 Months Of Record-Setting Temperatures Looks Like Across The U.S.")),
  sidebarLayout(
    sidebarPanel(
      uiOutput("tabUi")
    ),
    mainPanel(
      tabsetPanel(id = "tab",
        tabPanel( title = "Plot", value = "plot", plotlyOutput("tempplot")),
        tabPanel("Map", value = "map", leafletOutput("weathermap")),
        tabPanel("Table", value = "table", tableOutput("temptable"))
      )
    )
  ),
  tags$style(type="text/css", # from https://stackoverflow.com/questions/24652658/suppress-warning-message-in-r-console-of-shiny
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  )
)

server <- function(input, output) {
  output$tabUi <- renderUI({
    if (input$tab == "plot" | input$tab == "table") {
      uiList <- list(selectInput(inputId = "cityInput", label = "City Name", choices = sort(unique(location$city)), selected = "Seattle"),
                     dateRangeInput(inputId = "date", label = "Date Range", start = "2014-7-1", end = "2015-6-29", min = "2014-7-1", max = "2015-6-30"),
                     radioButtons(inputId = "tempType", label = "Temp Range Type", choices = list("Max" = "actual_max_temp", "Mean" = "actual_mean_temp", "Min" = "actual_min_temp"), inline = TRUE),
                     radioButtons(inputId = "tempUnit", label = "Temp Unit", choices = list("Farenheit" = "f", "Celsius" = "c"), inline = TRUE),
                     sliderInput(inputId = "temp", label = "Temp Range", min = -30, max = 130, value = c(20,75)))
    } else {
      uiList <- list(dateInput(inputId = "date", label = "Date", value = "2014-7-4", min = "2014-7-1", max = "2015-6-30"))
    }
    return(uiList)
  })
  output$cityOutput <- renderUI({
    selectInput(inputId = "cityInput", label = "City Name", choices = sort(unique(location$city)), selected = "Seattle")
  })
  cityWeatherData <- reactive({
    rightCity <- location %>% filter(input$cityInput == city)
    fileName <- rightCity[[2]]
    weather <- read.csv(paste0("data/", fileName, ".csv"), stringsAsFactors = FALSE)
    weather$date <- as.Date(weather$date, "%Y-%m-%d")
    if (input$tempUnit == "c") {
      weather$actual_mean_temp <- fahrenheit.to.celsius(weather$actual_mean_temp)
      weather$actual_min_temp <- fahrenheit.to.celsius(weather$actual_min_temp)
      weather$actual_max_temp <- fahrenheit.to.celsius(weather$actual_max_temp)
    }
    return(weather)
  })
  temp.input <- reactive({
    maxTemp <- paste0(input$tempType, ">=", input$temp[1])
    minTemp <- paste0(input$tempType, "<=", input$temp[2])
    cityWeatherData() %>% filter_(maxTemp) %>% filter_(minTemp) %>% 
      filter(date >= input$date[1]) %>%  filter(date <= input$date[2])
  })
  output$tempplot <- renderPlotly({
    x <- list(
      title = "Date",
      tickangle = 45,
      zeroline = FALSE
    )
    y <- list(
      title = "Temp",
      zeroline = FALSE
    )
    m <- list(
      b = 160,
      t = 50
    )
    plot_ly(temp.input(), x = ~date, y = ~actual_max_temp, type = 'scatter', mode = 'lines',opacity = 0.5, hoverinfo ='text', 
            text = ~paste('Date: ', date, '<br> Max Temp: ', actual_max_temp,'<br> Mean Temp:', actual_mean_temp, '<br> Min Temp: ', actual_min_temp), name = 'Max Temp') %>% 
      add_trace(y = ~actual_mean_temp, name = 'Mean Temp', opacity = 0.5) %>% 
      add_trace(y = ~actual_min_temp, name = 'Min Temp', opacity = 0.5) %>% 
      layout(xaxis = x, yaxis = y, title = paste("Temperature of", input$cityInput), barmode = 'overlay', margin = m)
  })
  output$weathermap <- renderLeaflet({
    mapStates <- map("state", fill = TRUE, plot = FALSE)
    leaflet(data = mapStates) %>% addTiles() %>% 
      addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>% 
      addMarkers(lat = location[,3], lng = location[,4], label = ~htmlEscape(location[,1]))
  })
  tableDate <- reactive({
    dateTable <- as.character(temp.input()[,1])
    temp.input() %>% mutate(date = dateTable)
  })
  output$temptable <- renderTable({
    tableDate()
  })
}

shinyApp(ui = ui, server = server)