library(shiny)
library(shinydashboard)
library(flexdashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Bipolar Scale Visualization"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    gaugeOutput("bipolar_gauge")
  )
)

server <- function(input, output) {
  output$bipolar_gauge <- renderGauge({
    value <- 40  # Replace with your dynamic value
    gauge(value, min = -100, max = 100, 
          symbol = "%", 
          gaugeSectors(success = c(50, 100), danger = c(-100, 0), warning = c(0, 50)))
  })
}

shinyApp(ui, server)