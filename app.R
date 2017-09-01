library(shiny)
weather <- read.csv("data/KSEA.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)