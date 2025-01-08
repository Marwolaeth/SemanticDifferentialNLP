## GLOBAL ----
library(shiny)
library(shinyjs)
library(flexdashboard)
library(shinydashboard)
library(shinydashboardPlus)
library(readxl)
library(glue)
library(stringr)
library(tictoc)

source('functions.R', encoding = 'UTF-8')

### Данные для примеров ----
examples <- read_excel('../data/xcellent-sentences.xlsx')
# Роскомнадзор разрабатывает систему для выявления запрещенного контента на основе искусственного интелекта.
# В Роскомнадзоре работают криворукие недоучки.

### Список моделей ----
load('../data/models/models.RData')

### Единый объект для оценки ----
universal_brand_name <- 'Y'

## UI ----
ui <- dashboardPage(
  title = 'Brand Semantics',
  skin = 'red',
  
  header = dashboardHeader(title = 'Brand Semantics', titleWidth = '25%'),
  
  ### Боковая панель ----
  sidebar = dashboardSidebar(
    width = '30%',
    sidebarMenu(
      menuItem('Оценка', tabName = 'assessment', icon = icon('check')),
      menuItem(
        'Настройки шкал', tabName = 'settings', icon = icon('sliders')
      ),
      menuItem(
        'Настройки оценщиков',
        tabName = 'method-settings',
        icon = icon('magnifying-glass-chart')
      )
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
  body = dashboardBody(
    useShinyjs(),
    
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
                  value = 'Umbrella',
                  width = '100%',
                  placeholder = paste(
                    'Объекты через запятую, например:',
                    '«Бренд А, наша компания, мы»'
                  )
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
              fluidRow(uiOutput('gauges')),
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
        ##### Экспорт и импорт ----
        fluidRow(
          column(
            6,
            downloadButton(
              'download_scaleset',
              'Экспортировать шкалы',
              class = 'btn-success'
            )
          ),
          column(
            6,
            fileInput(
              'upload_scaleset',
              'Импортировать шкалы',
              accept = '.RData')
          )
        ),
        ##### Редактирование ----
        tabsetPanel(
          tabPanel(
            'Редактирование',
            value = 'edit-scales',
            uiOutput('scale_inputs'),
            ##### Добавить и Сохранить ----
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
                style='text-align:right;',
                actionButton(
                  'submit_scales',
                  'Сохранить',
                  class = 'btn-success'
                )
              )
            )
          ),
          ##### Предпросмотр ----
          tabPanel(
            'Предпросмотр',
            value = 'edit-scales-preview',
            verbatimTextOutput('scales_output')
          )
        )
      ),
      #### Настройка оценщиков ----
      tabItem(
        tabName = 'method-settings',
        h2('Настройка оценщиков'),
        ##### NLI ----
        tabsetPanel(
          tabPanel(
            'Классификация',
            value = 'edit-classification',
          ),
          ##### Similarity ----
          tabPanel(
            'Семантическая близость',
            value = 'edit-similarity',
            checkboxInput(
              'similarity_group_items',
              label = 'Объединять элементы шкалы в одну фразу',
              value = FALSE
            ),
            radioButtons(
              'similarity_aggregation',
              label = 'Метод агрегирования',
              choices = c(
                'Автоматически'       = 'auto',
                'CLS-токен'           = 'cls',
                'Среднее по токенам'  = 'mean',
                'Минимум по токенам'  = 'min',
                'Максимум по токенам' = 'max'
              )
            ),
            radioButtons(
              'similarity_metric',
              label = 'Метрика расстояния',
              choices = c(
                'Косинусная близость / Корреляция Пирсона' = 'cosine',
                'Корреляция Спирмена' = 'spearman'
              ),
              width = '80%'
            )
          ),
          ##### LLM ----
          tabPanel(
            'Искусственный интеллект',
            value = 'edit-llm'
          )
        )
      )
    )
  )
)

## SERVER ----
server <- function(input, output, session) {
  ## Интерфейс ----
  
  ### Выбор моделей в зависимости от метода ----
  models <- reactive({
    if (input$method == 'llm') {
      c('Llama 3.2' = 1)
    } else {
      models_df |>
        dplyr::filter(
          lang == 'ru' & (task == 'NLI' | input$method == 'similarity') & ok
        ) |>
        dplyr::mutate(id = row_number()) |>
        dplyr::select(model, id) |>
        tibble::deframe()
    }
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
  
  ### Префикс ----
  prefix <- reactive({
    req(models)
    req(input$model)
    model_name <- names(models()[as.numeric(input$model)])
    
    # Добавим префиксы, если модель их принимает
    stringr::str_detect(
      model_name,
      '([Ss]enten)|([Ss][Bb][Ee][Rr][Tt])|(s\\-encoder)'
    )
  })
  
  ## Обработка ----
  
  ### Шаблон гипотезы ----
  hypotheses <- reactive({
    req(input$object)
    glue::glue('{universal_brand_name} – {{}}')
  })
  
  ### Случайный пример ----
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
  
  ### Нормы ----
  scaleset_norms <- reactive({
    req(input$method == 'similarity')
    req(input$model)
    req(scaleset())
    if (is.null(scaleset_norms())) print('Ok, I’m NULL.')
  })
  
  ### Анализ ----
  result <- reactive({
    req(input$object)
    req(input$model)
    req(input$text)
    req(hypotheses)
    req(prefix)
    
    #### Получение модели ----
    model_name <- names(models()[as.numeric(input$model)])
    print(model_name)
    
    #### Универсальное название ----
    if (stringr::str_detect(input$object, fixed(','))) {
      objects <- input$object |>
        stringr::str_split(fixed(',')) |>
        unlist() |>
        stringr::str_squish()
    } else {
      objects <- input$object
    }
    
    text <- .replace_object(input$text, objects, universal_brand_name)
    
    print(text)
    print(hypotheses())
    
    #### Функции анализа ----
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
              1 / length(scaleset()),
              message = 'Анализируем',
              detail = names(scaleset())[[i]]
            )
            scale_result <- semdiff_zeroshot_map(
              text,
              model_name,
              items = semantic_scale,
              template = hypotheses(),
              prefix = prefix(),
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
    
    res
  }) |>
    bindCache(input$model, input$text, scaleset()) |>
    bindEvent(input$submit)
    
  
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
    # Generate columns of equal width
    wd = 12 / length(gauges)
    do.call(tagList, lapply(gauges, column, width = wd))
  })
  
  # Генерация тахометров
  observe({
    req(input$submit)
    lapply(seq_along(result()), function(scale_i) {
      scale_name <- names(result())[scale_i]
      output[[paste0('gauge_', scale_i)]] <- renderGauge({
        value <- mean(result()[[scale_name]][['.score']]) * 10
        gauge(
          label = scale_name,
          value,
          min = -10, max = 10, 
          symbol = '', 
          gaugeSectors(
            success = c(5, 10),
            danger = c(-10, 0),
            warning = c(0, 5)
          )
        )
      })
    })
  })
  
  ## Редактирование шкал ----
  ### Исходная шкала ----
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
  
  ### Экспорт и импорт ----
  output$download_scaleset <- downloadHandler(
    filename = function() {
      paste('scaleset', Sys.Date(), '.RData', sep = '-')
    },
    content = function(file) {
      scaleset <- scaleset()
      save(scaleset, file = file)
    }
  )
  observeEvent(input$upload_scaleset, {
    req(input$upload_scaleset)
    
    # Load the RData file and update the scaleset
    scales_data <- new.env()
    load(input$upload_scaleset$datapath, envir = scales_data)
    
    # Assuming the scaleset is stored as a list in the RData file
    scaleset(get(ls(scales_data)[1], envir = scales_data))
  })
  
  
  ### Редактирование ----
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