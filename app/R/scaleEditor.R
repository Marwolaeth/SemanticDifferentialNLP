# R/scaleEditor.R

# Интерфейс модуля для редактирования шкалы
scaleEditorUI <- function(id) {
  ns <- NS(id)  # Создание пространства имен
  tagList(
    div(
      style = 'border: 2px solid #ccc; padding: 10px; margin-bottom: 10px;',
      uiOutput(ns('scale_name_ph')),
      h5('Характеристики:'),
      uiOutput(ns('markers')),
      fluidRow(
        column(
          6,
          actionButton(ns('add_marker'), 'Добавить характеристики')
        ),
        column(
          6,
          actionButton(ns('save_scale'), 'Сохранить', class = 'btn-success')
        )
      )
    )
  )
}

# Логика модуля для редактирования шкалы
scaleEditorServer <- function(id, i, scales_reactive, container) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    scale <- reactiveVal(scales_reactive()[[i]])  # Получаем текущую шкалу
    scale_name <- names(scales_reactive())[i]  # Имя текущей шкалы
    
    # Отображение названия шкалы
    output$scale_name_ph <- renderUI({
      textInput(ns('scale_name'), 'Название шкалы', value = scale_name)
    })
    
    # Отображение характеристик шкалы
    output$markers <- renderUI({
      lapply(seq_along(scale()), function(j) {
        markers <- scale()[[j]]
        div(
          style = 'border: 1px solid #ddd; padding: 5px; margin-bottom: 10px;',
          textInput(
            ns(paste0('marker_name_', j, '_neg')),
            'Отрицательная характеристика',
            value = names(markers)[1]
          ),
          textInput(
            ns(paste0('marker_name_', j, '_neu')),
            'Нейтральная характеристика',
            value = names(markers)[2]
          ),
          textInput(
            ns(paste0('marker_name_', j, '_pos')),
            'Положительная характеристика',
            value = names(markers)[3]
          )
        )
      })
    })
    
    # Обработка события добавления новой тройки характеристик
    observeEvent(input$add_marker, {
      new_scale <- scale()
      scale(append(
        new_scale,
        list(setNames(c(-1, 0, 1), c('neg', 'neu', 'pos')))
      ))
      
      print(length(scale()))
    })
    
    # Сохранение изменений в шкале
    observeEvent(input$save_scale, {
      new_scaleset <- scales_reactive()  # Получаем текущее значение шкал
      new_scale_name <- input$scale_name
      new_markers <- list()
      
      for (j in seq_along(scale())) {
        
        neg <- input[[paste0('marker_name_', j, '_neg')]]
        neu <- input[[paste0('marker_name_', j, '_neu')]]
        pos <- input[[paste0('marker_name_', j, '_pos')]]
        
        new_markers[[j]] <- purrr::set_names(c(-1, 0, 1), c(neg, neu, pos))
      }
      
      # Обновляем название шкалы и характеристики
      names(new_scaleset)[i] <- new_scale_name  # Обновляем имя шкалы
      new_scaleset[[i]] <- new_markers
      
      container(new_scaleset)  # Обновляем реактивное значение
    })
  })
}
