## GLOBAL ----
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(readxl)
library(text)
library(tokenizers)
library(tibble)
library(tidyr)
library(dplyr)
library(glue)

source('functions.R', encoding = 'UTF-8')

### Данные для примеров ----
examples <- read_excel('../data/xcellent-sentences.xlsx')

### Список моделей ----
load('../data/models/models.RData')
models_df <- models_df |>
  dplyr::filter(lang == 'ru' & task == 'NLI' & ok) |>
  dplyr::mutate(id = row_number())
models <- tibble::deframe(dplyr::select(models_df, model, id))

### Шкала ----
poles <- c('отсталый', 'инновационный')

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = 'Семантический дифференциал'),
  ### Ввод ----
  dashboardSidebar(
    width = '50%',
    #### Модель ----
    selectInput(
      'model',
      label = 'Выберите модель',
      choices = models,
      selected = 11,
      width = '95%'
    ),
    #### Объект ----
    textInput(
      'object',
      'Объект оценки',
      value = 'XCellent'
    ),
    #### Текст ----
    textAreaInput(
      'text',
      'Текст:',
      '',
      rows = 5,
      resize = 'vertical',
      width = '95%'
    ),
    #### Кнопки ----
    actionButton('example', 'Случайный пример', class = 'btn-warning'),
    actionButton('submit', 'Оценить', class = 'btn-success')
  ),
  ### Вывод ----
  dashboardBody(
    gaugeOutput('bipolar_gauge'),
    verbatimTextOutput('result')
  )
)

## SERVER ----
server <- function(input, output, session) {
  ## Обработка ----
  
  #### Шаблон гипотезы ----
  hypotheses <- reactive({
    req(input$object)
    glue::glue('{input$object} – {{}}')
  })
  
  #### Результат
  result <- reactiveVal()
  
  #### Кнопки ----
  ##### Случайный пример ----
  observeEvent(input$example, {
    ex <- slice_sample(examples, n = 1) |>
      pull(sentence)
    
    updateTextInput(
      session,
      'text',
      value = ex
    )
  })
  
  ##### Анализ ----
  observeEvent(input$submit, {
    req(hypotheses)
    req(input$model)
    req(input$text)
    
    texts <- paragraphs(input$text)
    model_name <- names(models[as.numeric(input$model)])
    
    print(model_name)
    
    res <- semdiff_zeroshot(
      texts,
      model_name,
      polarities = poles,
      template = hypotheses()
    )
    
    result(res)
  })
  
  ## Вывод ----
  output$result <- renderPrint(result())
  
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