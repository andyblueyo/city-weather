library(shiny)
weather <- read.csv("data/KSEA.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("City Weather", windowTitle = "AH NOPE"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(inputId = "date", label = "Date Range", start = "2014-10-1", end = "2015-6-8", min = "2014-10-1", max = "2015-6-9"),
      sliderInput(inputId = "temp", label = "Temp Range", min = 0, max = 100, value = c(25,75))
    ),
    mainPanel(
      plotOutput("tempplot"),
      br(),
      tableOutput("temptable"))
  )
)

server <- function(input, output) {
  output$tempplot <- renderPlot({
    plot(rnorm(input$temp[1]))
  })
}

shinyApp(ui = ui, server = server)