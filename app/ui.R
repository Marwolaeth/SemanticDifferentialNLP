library(shiny)
library(flexdashboard)
library(shinydashboard)
library(shinydashboardPlus)

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
      menuItem('Шкалы', tabName = 'settings', icon = icon('sliders')),
      menuItem(
        'Настройки оценщиков',
        tabName = 'method-settings',
        icon = icon('magnifying-glass-chart')
      ),
      menuItem('История', tabName = 'history', icon = icon('clock-rotate-left'))
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
              with_red_spinner(
                verbatimTextOutput('result'),
                size = 2,
                caption = NULL
              )
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
              class = 'btn-success',
              style = 'margin-top:25px;'
            )
          ),
          column(
            6,
            fileInput(
              'upload_scaleset',
              'Импортировать шкалы',
              accept = c('.sds', '.RData'),
              buttonLabel = 'Обзор…',
              placeholder = 'Файл не выбран'
            )
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
            tags$br(),
            textInput(
              'hypothesis_template',
              'Шаблон гипотезы',
              value = '{brand_name} – {hypothesis}'
            ),
            tags$strong('Предпросмотр: '),
            tags$div(
              textOutput(
                'hypothesis_preview',
                inline = FALSE
              ),
              style = 'color:#888;'
            )
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
                'Токен объекта'       = 'token'
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
      ),
      #### История оценок ----
      tabItem(
        tabName = 'history',
        h2('История оценок'),
        tags$button(
          tagList(fontawesome::fa('download'), 'Сохранить CSV'),
          onclick = "Reactable.downloadDataCSV(
            'history-table', 'history.csv',
            {headers: true, sep: ';', dec: ','}
          )",
          class = '
            btn btn-default shiny-download-link btn-success shiny-bound-output',
          style = 'margin-bottom: 20px;'
        ),
        reactableOutput('history')
      )
    )
  )
)