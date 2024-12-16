library(shiny)
library(readr)
library(dplyr)
library(forcats)

DATA_VARIABLES <- c(
  'premise',
  'hypothesis',
  'label2',
  'label3',
  'idx'
)

LABELS3 <- c(
  'Entailment' = 'entailment',
  'Contradiction' = 'contradiction',
  'Neutral' = 'neutral'
)

LABELS2 <- list(
  'entailment' = 'entailment',
  'not_entailment' = c('contradiction', 'neutral')
)

# UI ----
ui <- fluidPage(
  titlePanel('Natural Language Inference Data Preparation'),
  sidebarLayout(
    sidebarPanel(
      width = 12,
      
      ## File Management ----
      fileInput(
        'file',
        'Load existing CSV file:',
        accept = c(
          'text/csv', 'text/comma-separated-values,text/plain', '.csv'
        )
      ),
      fluidRow(
        column(width = 6, actionButton('load', 'Load')),
        column(width = 6, downloadButton('download', 'Download CSV'))
      ),
      hr(),
      
      ## Text Input ----
      textAreaInput(
        'premise',
        'Premise:',
        '',
        rows = 4,
        resize = 'vertical'
      ),
      textAreaInput(
        'hypothesis',
        'Hypothesis:',
        '',
        rows = 2,
        resize = 'vertical'
      ),
      ## Labels ----
      radioButtons(
        'label',
        'Label:',
        choices = LABELS3,
        selected = 'neutral',
        inline = TRUE
      ),
      actionButton('submit', 'Submit'),
      textOutput('status')
    ),
    mainPanel(
      h3('Instructions'),
      p(
        'Load an existing project, enter the premise and hypothesis, select a label, and submit to save the data.'
      )
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  current_data <- reactiveVal()
  
  ## Load ----
  # Load existing data when the Load button is clicked
  observeEvent(input$load, {
    req(input$file)
    
    # Read the CSV file
    loaded_data <- read_csv(input$file$datapath)
    print(loaded_data)
    
    # Validate the loaded data
    print(all(DATA_VARIABLES %in% colnames(loaded_data)))
    if (all(DATA_VARIABLES %in% colnames(loaded_data))) {
      # Update the reactive data store with loaded data
      current_data(
        loaded_data |>
          select(all_of(DATA_VARIABLES))
      )
      output$status <- renderText('Data loaded successfully!')
    } else {
      output$status <- renderText(
        "Error: Loaded file must contain 'premise', 'hypothesis', 'label2', ', 'label3', and 'idx' columns."
      )
    }
  })
  
  ## Submit ----
  observeEvent(input$submit, {
    req(input$premise, input$hypothesis, input$label)
    
    # Read existing data from the CSV file
    if (is.null(current_data())) {
      data <- tibble(
        premise = character(),
        hypothesis = character(),
        label2 = factor(levels = LABELS2),
        label3 = factor(levels = names(LABELS2)),
        idx = integer()
      )
      idx <- 1L # Start index at 1 if file does not exist
      current_data(data)
    } else {
      idx <- nrow(current_data()) + 1L
    }
    
    # Create a new row to append
    new_row <- tibble(
      premise = input$premise,
      hypothesis = input$hypothesis,
      label3 = input$label,
      idx = idx,
    ) |>
      mutate(
        label2 = fct_collapse(label3, !!!LABELS2),
        .before = label3
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
    updateRadioButtons(session, 'label', selected = 'neutral')
  })
  
  ## Download ----
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
