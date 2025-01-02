## GLOBAL ----
library(shiny)
library(flexdashboard)
library(shinydashboard)
library(shinydashboardPlus)
library(shinybusy)
library(readxl)
library(glue)
library(stringr)
library(tictoc)

source('functions.R', encoding = 'UTF-8')

### Данные для примеров ----
examples <- read_excel('../data/xcellent-sentences.xlsx')

### Список моделей ----
load('../data/models/models.RData')

### Шкалы ----
# Роскомнадзор разрабатывает систему для выявления запрещенного контента на основе искусственного интелекта.
# В Роскомнадзоре работают криворукие недоучки.
# scaleset <- list(
#   'Инновационность' = list(
#     c('устаревший' = -1, 'сдержанный' = 0, 'инновационный'   = 1),
#     c('отсталый'   = -1, 'стабильный' = 0, 'изобретательный' = 1)
#   ),
#   'Популярность' = list(
#     c('немодный'      = -1, 'адекватный'    = 0, 'модный'     = 1),
#     c('неактуальный'  = -1, 'специфический' = 0, 'молодежный' = 1),
#     c('непопулярный'  = -1, 'известный'     = 0, 'популярный' = 1),
#     c('малоизвестный' = -1, 'элитарный'     = 0, 'знаменитый' = 1)
#   ),
#   'Надежность' = list(
#     c('ненадежный'     = -1, 'нормальный'  = 0, 'надежный'     = 1),
#     c('некачественный' = -1, 'обычный'     = 0, 'качественный' = 1),
#     c('хлипкий'        = -1, 'стандартный' = 0, 'прочный'      = 1),
#     c('дефектный'      = -1, 'интересный'  = 0, 'проверенный'  = 1)
#   )
# )

## UI ----
ui <- dashboardPage(
  title = 'Brand Semantics',
  skin = 'red',
  
  dashboardHeader(title = 'Brand Semantics', titleWidth = '25%'),
  
  ### Боковая панель ----
  dashboardSidebar(
    width = '30%',
    sidebarMenu(
      menuItem('Оценка', tabName = 'assessment', icon = icon('check')),
      menuItem(
        'Настройки шкал', tabName = 'settings', icon = icon('scale-unbalanced'))
    )
  ),
  
  ### Панель настроек ----
  controlbar = dashboardControlbar(
    width = 450,
    tags$div(
      style = 'margin:10px;',
      radioButtons(
        'method',
        label = 'Метод оценки',
        choices = list(
          'Классификация' = 'classification',
          'Семантическая близость' = 'similarity',
          'Искусственный интеллект' = 'llm'
        ),
        selected = 'classification'
      ),
      uiOutput('model'),
      radioButtons(
        'device',
        label = 'Устройство',
        choices = c('CPU', 'GPU'),
        selected = 'CPU'
      ),
      numericInput(
        'seed',
        label = 'Начальное случайное значение',
        min = 1,
        max = 11111,
        value = 111
      )
    )
  ),
  
  ### Основная панель ----
  dashboardBody(
    tabItems(
      #### Интерфейс оценки ----
      tabItem(
        tabName = 'assessment',
        ##### Ввод ----
        fluidRow(
          column(
            12,
            fluidRow(
              column(
                8,
                textInput(
                  'object',
                  'Объект оценки',
                  value = 'XCellent',
                  width = '100%'
                )
              ),
              column(
                4,
                actionButton(
                  'example', 'Случайный пример', class = 'btn-warning',
                  style = 'margin-top:25px;'
                )
              )
            ),
            textAreaInput(
              'text',
              'Текст:',
              '',
              rows = 5,
              resize = 'vertical',
              width = '100%'
            ),
            actionButton(
              'submit',
              'Оценить',
              class = 'btn-success',
              icon = icon('check-double')
            )
          )
        ),
        ##### Вывод ----
        fluidRow(
          style = 'padding:10px;',
          tabsetPanel(
            id = 'scales_output',
            tabPanel(
              'Результат',
              value = 'scales_output_visual',
              uiOutput('gauges')
            ),
            tabPanel(
              'Просмотр расчетов',
              value = 'scales_output_raw',
              verbatimTextOutput('result')
            )
          )
        )
      ),
      #### Настройка шкал ----
      tabItem(
        tabName = 'settings',
        h2('Редактирование семантических шкал'),
        tabsetPanel(
          tabPanel(
            'Редактирование',
            value = 'edit_scales',
            uiOutput('scale_inputs'),
            fluidRow(
              column(
                6,
                actionButton(
                  'add_scale',
                  'Добавить шкалу',
                  class = 'btn-primary'
                )
              ),
              column(
                6,
                actionButton(
                  'submit_scales',
                  'Сохранить',
                  class = 'btn-success'
                )
              )
            )
          ),
          tabPanel(
            'Предпросмотр',
            value = 'edit_scales_preview',
            verbatimTextOutput('scales_output')
          )
        )
      )
    )
  )
)

## SERVER ----
server <- function(input, output, session) {
  ## Интерфейс ----
  models <- reactive({
    models_df |>
      dplyr::filter(
        lang == 'ru' & (task == 'NLI' | input$method == 'similarity') & ok
      ) |>
      dplyr::mutate(id = row_number()) |>
      dplyr::select(model, id) |>
      tibble::deframe()
  })
  
  output$model <- renderUI({
    selectInput(
      'model',
      label = 'Выберите модель',
      choices = models(),
      selected = 7,
      width = '100%'
    )
  })
  
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
    
    model_name <- names(models()[as.numeric(input$model)])
    
    print(model_name)
    
    # Добавим префиксы, если модель их принимает
    is_sentence_transformer <- stringr::str_detect(
      model_name,
      '([Ss]enten)|([Ss][Bb][Ee][Rr][Tt])|(s\\-encoder)'
    )
    if (is_sentence_transformer) {
      prefix <- TRUE
    } else {
      prefix <- FALSE
    }
    
    tictoc::tic()
    withProgress(
      session = session,
      message = 'Анализируем…',
      {
        res <- purrr::map2(
          scaleset(),
          seq_along(scaleset()),
          function(semantic_scale, i) {
            incProgress(
              1/ length(scaleset()),
              message = 'Анализируем',
              detail = names(scaleset())[[i]]
            )
            scale_result <- semdiff_zeroshot_map(
              input$text,
              model_name,
              polarities = semantic_scale,
              template = hypotheses(),
              prefix = prefix,
              append_neutral = TRUE,
              seed = input$seed,
              device = tolower(input$device)
            )
            return(scale_result)
          }
        ) |>
          purrr::set_names(names(scaleset()))
      })
    tictoc::toc()
    
    result(res)
  })
  
  ## Вывод ----
  output$result <- renderPrint({
    req(input$submit)
    result() |>
      show_scales_result()
  })
  
  output$gauges <- renderUI({
    req(input$submit)
    gauges <- lapply(seq_along(result()), function(scale_i) {
      gaugeOutput(outputId = paste0('gauge_', scale_i), width = '100%')
    })
    do.call(fluidRow, gauges)
  })
  
  # Генерация тахометров
  observe({
    req(input$submit)
    lapply(seq_along(result()), function(scale_i) {
      scale_name <- names(result())[scale_i]
      output[[paste0('gauge_', scale_i)]] <- renderGauge({
        value <- mean(result()[[scale_name]][['.score']]) * 100
        gauge(
          label = scale_name,
          value,
          min = -100, max = 100, 
          symbol = '', 
          gaugeSectors(
            success = c(50, 100),
            danger = c(-100, 0),
            warning = c(0, 50)
          )
        )
      })
    })
  })
  
  ## Редактирование шкал ----
  # Хранение шкал
  scaleset <- reactiveVal(
    list(
      'Инновационность' = list(
        c('устаревший' = -1, 'сдержанный' = 0, 'инновационный'   = 1),
        c('отсталый'   = -1, 'стабильный' = 0, 'изобретательный' = 1)
      ),
      'Популярность' = list(
        c('немодный'      = -1, 'адекватный'    = 0, 'модный'     = 1),
        c('неактуальный'  = -1, 'специфический' = 0, 'молодежный' = 1),
        c('непопулярный'  = -1, 'известный'     = 0, 'популярный' = 1),
        c('малоизвестный' = -1, 'элитарный'     = 0, 'знаменитый' = 1)
      ),
      'Надежность' = list(
        c('ненадежный'     = -1, 'нормальный'  = 0, 'надежный'     = 1),
        c('некачественный' = -1, 'обычный'     = 0, 'качественный' = 1),
        c('хлипкий'        = -1, 'стандартный' = 0, 'прочный'      = 1)
      )
    )
  )
  new_scaleset <- reactiveVal()
  
  output$scale_inputs <- renderUI({
    scales <- scaleset()
    scale_inputs <- lapply(seq_along(scales), function(i) {
      scaleEditorUI(paste0('scale_', i))  # Вызов модуля для каждой шкалы
    })
    do.call(tagList, scale_inputs)
  })
  
  observe({
    lapply(seq_along(scaleset()), function(i) {
      scaleEditorServer(
        paste0('scale_', i),
        i,
        scaleset,
        new_scaleset
      )  # Передача реактивного значения
    })
  })
  
  observeEvent(input$add_scale, {
    scales <- scaleset()
    scales[[paste0('Шкала ', length(scales) + 1)]] <- list(
      c('отрицательная' = -1, 'нейтральная' = 0, 'положительная' = 1)
    )
    scaleset(scales)
  })
  
  observeEvent(input$submit_scales, {
    req(new_scaleset)
    
    scaleset(new_scaleset())
  })
  
  output$scales_output <- renderPrint({
    scaleset()
  })
}

shinyApp(ui, server)