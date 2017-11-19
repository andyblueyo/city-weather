library(shiny)
library(plotly)
library(dplyr)
library(lazyeval)
library(leaflet)
library(maps)
library(htmltools)
library(weathermetrics)
library(DT)

location <- read.csv("data/location.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("City Weather", windowTitle = "US City Weather"),
  p("This is an interactive data visualization focused on data from the ", 
    tags$a(href = "https://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/", "What 12 Months Of Record-Setting Temperatures Looks Like Across The U.S."),
    "article. The project's code is viewable on ", tags$a(href = "https://github.com/andyblueyo/city-weather", "GitHub")),
  sidebarLayout(
    sidebarPanel(
      uiOutput("tabUi")
    ),
    mainPanel(
      tabsetPanel(id = "tab",
        tabPanel( title = "Plot", value = "plot", plotlyOutput("tempplot")),
        tabPanel(title = "Table", value = "table", dataTableOutput("temptable")),
        tabPanel(title = "Map", value = "map", leafletOutput("weathermap"))
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
      uiList <- list(selectInput(inputId = "cityInput", label = "Select a city:", choices = sort(unique(location$city)), selected = "Seattle"),
                     dateRangeInput(inputId = "date", label = "Select the range of dates:", start = "2014-7-1", end = "2015-6-29", min = "2014-7-1", max = "2015-6-30"),
                     radioButtons(inputId = "tempType", label = "Select type of temperture to change:", choices = list("Max" = "actual_max_temp", "Mean" = "actual_mean_temp", "Min" = "actual_min_temp"), inline = TRUE),
                     radioButtons(inputId = "tempUnit", label = "Select temperture unit:", choices = list("Farenheit" = "f", "Celsius" = "c"), inline = TRUE),
                     sliderInput(inputId = "temp", label = "Select range of tempertures to display:", min = -30, max = 130, value = c(20,75)))
    } else {
      uiList <- list(dateInput(inputId = "map.date", label = "Select the date:", value = "2014-7-4", min = "2014-7-1", max = "2015-6-30"))
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
    rownames(weather) <- NULL
    weather$date <- as.Date(weather$date, "%Y-%m-%d")
    if (input$tempUnit == "c") {
      weather$actual_mean_temp <- fahrenheit.to.celsius(weather$actual_mean_temp)
      weather$actual_min_temp <- fahrenheit.to.celsius(weather$actual_min_temp)
      weather$actual_max_temp <- fahrenheit.to.celsius(weather$actual_max_temp)
      weather$average_min_temp <- fahrenheit.to.celsius(weather$average_min_temp)
      weather$average_max_temp <- fahrenheit.to.celsius(weather$average_max_temp)
    }
    return(weather)
  })
  temp.input <- reactive({
    maxTemp <- paste0(input$tempType, ">=", input$temp[1])
    minTemp <- paste0(input$tempType, "<=", input$temp[2])
    
    temp <- cityWeatherData() %>% filter_(maxTemp) %>% filter_(minTemp) %>% 
      filter(date >= input$date[1]) %>%  filter(date <= input$date[2])
    
    data.length <- length(temp$date)
    # Find min and max. Because the data is sorted, this will be
    # the first and last element.
    time.min <- temp$date[1]
    time.max <- temp$date[data.length]
    
    # generate a time sequence with 1 month intervals to fill in
    # missing dates
    all.dates <- seq(time.min, time.max, by="day")
    
    # Convert all dates to a data frame. Note that we're putting
    # the new dates into a column called "time" just like the
    # original column. This will allow us to merge the data.
    all.dates.frame <- data.frame(list(date=all.dates))
    
    # Merge the two datasets: the full dates and original data
    merged.data <- merge(all.dates.frame, temp, all=T)
    
    return(merged.data)
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
    plot_ly(temp.input(), x = ~date, y = ~record_max_temp, type = 'scatter', mode = 'lines', line = list(color = 'rgb(201, 224, 255)'), connectgaps = FALSE, name = 'Record Max Temp') %>% 
      add_trace(y = ~record_min_temp, name = 'Record Min Temp', line = list(color = 'rgb(201, 224, 255)'), connectgaps = FALSE) %>% 
      add_trace(y = ~actual_mean_temp, name = 'Actual Mean Temp', line = list(color = 'rgb(255, 0, 0)'), connectgaps = FALSE) %>%
      add_trace(y = ~average_min_temp, name = 'Average Min Temp',line = list(color = 'rgb(66, 148, 255)'), connectgaps = FALSE) %>% 
      add_trace(y = ~average_max_temp, name = 'Average Max Temp', line = list(color = 'rgb(66, 148, 255)'), connectgaps = FALSE) %>% 
      add_trace(y = ~actual_min_temp, name = 'Actual Min Temp', line = list(color = 'rgb(6, 54, 132)'), connectgaps = FALSE) %>% 
      add_trace(y = ~actual_max_temp, name = 'Actual Max Temp', line = list(color = 'rgb(6, 54, 132)'), connectgaps = FALSE) %>% 
      layout(xaxis = x, yaxis = y, title = paste("Temperature of", input$cityInput), barmode = 'overlay', margin = m)
  })
  weatherMapTemp <- reactive({
    files <- location$file_name
    charDate <- function(csv){
      csv <- read.csv(paste0("data/",csv,".csv"), stringsAsFactors = FALSE)
      csv$date <- as.Date(csv$date, "%Y-%m-%d")
      return(csv)
    }
    list.data <- lapply(files, charDate)
    for (i in seq(list.data)) {
      list.data[[i]] <- list.data[[i]] %>% filter(list.data[[i]]$date == input$map.date)
      location$actual_mean_temp[location$file_name == files[i]] <<- list.data[[i]]$actual_mean_temp
      location$actual_min_temp[location$file_name == files[i]] <<- list.data[[i]]$actual_min_temp
      location$actual_max_temp[location$file_name == files[i]] <<- list.data[[i]]$actual_max_temp
    }
  })
  output$weathermap <- renderLeaflet({
    weatherMapTemp()
    label.pls <- lapply(seq(nrow(location)), function(i) { # from https://stackoverflow.com/questions/43144596/r-and-leaflet-how-to-arrange-label-text-across-multiple-lines
      paste0(location[i,1], "<p></p>Actual Mean Temp (F):",location[i,5],
             "<p></p>Actual Min Temp (F):", location[i,6],
             "<p></p>Actual Max Temp (F):", location[i,7])
    })
    mapStates <- map("state", fill = TRUE, plot = FALSE)
    leaflet(data = mapStates) %>% addTiles() %>% 
      addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>% 
      addMarkers(lat = location[,3], lng = location[,4], label = lapply(label.pls, HTML))
  })
  tableDate <- reactive({
    dateTable <- as.character(temp.input()[,1])
    temp <- temp.input() %>% mutate(date = dateTable)
    temp <- temp[complete.cases(temp), ]
    rownames(temp) <- c()
    return(temp)
  })
  output$temptable <- renderDataTable({
    datatable(tableDate(), colnames = c("Date", "Actual Mean Temp", "Actual Min Temp", "Actual Max Temp", "Average Min Temp", "Average Max Temp", "Record Min Temp", "Record Max Temp", "Record Min Temp Year", "Record Max Temp Year", "Actual Precipitation", "Average Precipitation", "Record Precipitation"))
  })
}

shinyApp(ui = ui, server = server)