library(shiny)
library(dplyr)
library(readr)

# Define UI for the app
ui <- fluidPage(
  titlePanel('Natural Language Inference Data Preparation'),
  sidebarLayout(
    sidebarPanel(
      width = 12,
      textAreaInput(
        'premise',
        'Premise:',
        '',
        rows = 5,
        resize = 'vertical'
      ),
      textAreaInput(
        'hypothesis',
        'Hypothesis:',
        '',
        rows = 3,
        resize = 'vertical'
      ),
      radioButtons(
        'label',
        'Label:',
        choices = list(
          'Entailment' = 'entailment',
          'Not entailment' = 'not_entailment'
        ),
        selected = 'not_entailment'
      ),
      fileInput(
        'file',
        'Select CSV file to save data:',
        accept = c(
          'text/csv', 'text/comma-separated-values,text/plain', '.csv')
      ),
      actionButton('submit', 'Submit'),
      hr(),
      downloadButton('download', 'Download CSV'),
      textOutput('status')
    ),
    mainPanel(
      h3('Instructions'),
      p(
        'Enter the premise and hypothesis, select a label, and submit to save the data.'
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  current_data <- reactiveVal()
  
  observeEvent(input$submit, {
    req(input$premise, input$hypothesis, input$label, input$file)
    
    # Read existing data from the CSV file
    if (file.exists(input$file$datapath)) {
      data <- read_csv(
        input$file$datapath,
        col_select = 1:4,
        col_types = 'ccci'
      )
      idx <- nrow(data) + 1 # Auto-increment index
    } else {
      data <- tibble(
        premise = character(),
        hypothesis = character(),
        label = character(),
        idx = integer()
      )
      idx <- 1L # Start index at 1 if file does not exist
    }
    current_data(bind_rows(data, current_data()))
    
    # Create a new row to append
    new_row <- tibble(
      premise = input$premise,
      hypothesis = input$hypothesis,
      label = input$label,
      idx = idx,
    )
    idx <- idx + 1L
    
    # Append new row to the data
    current_data(
      bind_rows(current_data(), new_row) |>
        distinct(premise, hypothesis, .keep_all = TRUE) |>
        mutate(idx = 1:n())
    )
    
    print(current_data())
    
    output$status <- renderText('Data added successfully!')
    
    # Clear inputs after submission
    # updateTextInput(session, 'premise', value = '')
    updateTextInput(session, 'hypothesis', value = '')
    updateRadioButtons(session, 'label', selected = 'not_entailment')
  })
  
  # Download handler for the CSV file
  output$download <- downloadHandler(
    filename = function() {
      paste0("nli_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(
        current_data(),
        file,
        quote = 'all'
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
