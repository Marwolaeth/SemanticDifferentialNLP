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
  title = 'Семантический дифференциал',
  skin = 'red',
  
  dashboardHeader(title = 'Семантический дифференциал', titleWidth = '40%'),
  
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
          'Семантическая близость' = 'similarity'
        ),
        selected = 'classification'
      ),
      uiOutput('model')
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
          style = 'padding-top:10px;',
          column(6, uiOutput('gauges')),
          column(6, verbatimTextOutput('result'))
        )
      ),
      #### Настройка шкал ----
      tabItem(
        tabName = 'settings',
        h2('Редактирование семантических шкал'),
        uiOutput('scale_inputs'),
        actionButton('add_scale', 'Добавить шкалу', class = 'btn-primary'),
        actionButton('save_scales', 'Сохранить шкалы', class = 'btn-success'),
        verbatimTextOutput('scales_output')
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
      res <- purrr::map(
        scaleset(),
        function(semantic_scale) {
          scale_result <- semdiff_zeroshot_map(
            input$text,
            model_name,
            polarities = semantic_scale,
            template = hypotheses(),
            prefix = prefix,
            append_neutral = TRUE
          )
          incProgress(1/ length(scaleset()))
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
    result()
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
  
  output$scale_inputs <- renderUI({
    scales <- scaleset()
    scale_inputs <- lapply(seq_along(scales), function(i) {
      scale <- scales[[i]]
      tagList(
        textInput(paste0('scale_name_', i), 'Название шкалы', value = names(scales)[i]),
        lapply(seq_along(scale), function(j) {
          markers <- scale[[j]]
          tagList(
            textInput(paste0('marker_name_', i, '_', j, '_neg'), 'Отрицательная характеристика', value = names(markers)[1]),
            textInput(paste0('marker_name_', i, '_', j, '_neu'), 'Нейтральная характеристика', value = names(markers)[2]),
            textInput(paste0('marker_name_', i, '_', j, '_pos'), 'Положительная характеристика', value = names(markers)[3])
          )
        })
      )
    })
    do.call(tagList, scale_inputs)
  })
  
  observeEvent(input$add_scale, {
    scales <- scaleset()
    scales[[paste0('Шкала ', length(scales) + 1)]] <- list(
      c('отрицательная' = -1, 'нейтральная' = 0, 'положительная' = 1)
    )
    scaleset(scales)
  })
  
  observeEvent(input$save_scales, {
    scales <- scaleset()
    new_scales <- list()
    
    for (i in seq_along(scales)) {
      scale_name <- input[[paste0('scale_name_', i)]]
      new_markers <- list()
      
      for (j in seq_along(scales[[i]])) {
        neg <- input[[paste0('marker_name_', i, '_', j, '_neg')]]
        neu <- input[[paste0('marker_name_', i, '_', j, '_neu')]]
        pos <- input[[paste0('marker_name_', i, '_', j, '_pos')]]
        
        new_markers[[j]] <- purrr::set_names(c(-1, 0, 1), c(neg, neu, pos))
      }
      
      new_scales[[scale_name]] <- new_markers
    }
    
    scaleset(new_scales)
    output$scales_output <- renderPrint({
      scaleset()
    })
  })
}

shinyApp(ui, server)