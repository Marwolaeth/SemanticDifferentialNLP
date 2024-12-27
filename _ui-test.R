

# Define UI
ui <- fluidPage(
  actionButton("rmv", "Remove UI"),
  textInput("txt", "This is no longer useful"),
  
  actionButton("add", "Add UI")
)

# Server logic
server <- function(input, output, session) {
  observeEvent(input$rmv, {
    removeUI(
      selector = "div:has(> #txt)"
    )
  })
  
  observeEvent(input$add, {
    insertUI(
      selector = "#add",
      where = "afterEnd",
      ui = textInput(paste0("txt", input$add),
                     "Insert some text")
    )
  })
}

# Complete app with UI and server components
shinyApp(ui, server)