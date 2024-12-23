library(shiny)
library(shinydashboard)
library(flexdashboard)
library(ggiraph)

ui <- dashboardPage(
  dashboardHeader(title = "Bipolar Scale Visualization"),
  dashboardSidebar(
    sliderInput('value', 'Value', -100, 100, step = 1, value = 40)
  ),
  dashboardBody(
    gaugeOutput("bipolar_gauge")
    # girafeOutput('bipolar_gauge')
  )
)

server <- function(input, output) {
  output$bipolar_gauge <- renderGauge({
    req(input$value)
    value <- input$value
    ggauge <- gauge(value, min = -100, max = 100, 
      symbol = "%", 
      gaugeSectors(
        success = c(50, 100),
        danger = c(-100, 0),
        warning = c(0, 50)
      ),
    )
    return(ggauge)
    # girafe(ggobj = ggauge)
  })
}

shinyApp(ui, server)