## GLOBAL ----
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(shinybusy)
library(readxl)
library(glue)
library(stringr)

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
poles <- list(
  'Инновационность' = list(
    c('стабильный'     = -1, 'инновационный' = 1),
    c('консервативный' = -1, 'прогрессивный' = 1),
    c('подражатель'    = -1, 'новатор'       = 1),
    c('отстающий'      = -1, 'развивающийся' = 1),
    c('отсталый'       = -1, 'рвется вперед' = 1),
    c('умеренный'      = -1, 'ищущий новое'  = 1),
    c('осторожный'     = -1, 'передовой'     = 1),
    c('допотопный'     = -1, 'технологичный' = 1),
    c('наблюдатель'    = -1, 'изобретатель'  = 1),
    c('скромный'       = -1, 'продвинутый'   = 1),
    # c('догоняющий'     = -1, 'драйвер развития' = 1),
    c('устаревший'     = -1, 'устремленный в будущее' = 1),
    c('стабильная компания' = -1, 'инновационная компания' = 1)
  ),
  'Популярность' = list(
    c('немодный'     = -1, 'модный' = 1),
    c('неактуальный' = -1, 'модный' = 1),
  #   'неактуальный'  = -1,
  #   'непопулярный'  = -1,
  #   'эксклюзивный'  = -1,
  #   'малоизвестный' = -1,
  #   'элитарный'     = -1,
  #   'не для всех'   = -1,
  #   'специфический' = -1,
  #   'модный'            = 1,
  #   'популярный'        = 1,
  #   'молодежный'        = 1,
  #   'хайповый'          = 1,
  #   'понятный для всех' = 1,
  #   'народная марка'    = 1,
  #   'знаменитый'        = 1,
  #   'распространенный'  = 1,
  #   'общеизвестный'     = 1
  ),
  # 'Надежность' = tibble::tibble(
  #   'ненадежный'        = -1,
  #   'недолговечный'     = -1,
  #   'хрупкий'           = -1,
  #   'некачественный'    = -1,
  #   'глючный'           = -1,
  #   'непредсказуемый'   = -1,
  #   'дефектный'         = -1,
  #   'хлипкий'           = -1,
  #   'низкокачественный' = -1,
  #   'непрочный'         = -1,
  #   'надежный'     = 1,
  #   'качественный' = 1,
  #   'долговечный'  = 1,
  #   'прочный'      = 1,
  #   'крепкий'      = 1,
  #   'безотказный'  = 1,
  #   'проверенный'  = 1,
  #   'испытанный'   = 1,
  #   'добротный'    = 1,
  #   'достойный доверия'   = 1
  # )
)
# poles_matrix <- poles |>
#   dplyr::bind_rows() |>
#   as.matrix()
# poles_matrix[is.na(poles_matrix)] <- 0
# rownames(poles_matrix) <- names(poles)

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
    add_busy_bar(color = '#dd0000', height = '13px', timeout = 1000),
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
    ex <- dplyr::slice_sample(examples, n = 1) |>
      dplyr::mutate(
        sentence = stringr::str_replace_all(
          sentence,
          '[Xx][Cc]ellent',
          input$object
        )
      ) |>
      dplyr::pull(sentence)
    
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
      polarities = names(poles[['Инновационность']][[1]]),
      template = hypotheses(),
      mask = c(-1, 1),
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