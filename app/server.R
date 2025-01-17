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
    current_model <- isolate(input$model)
    selectInput(
      'model',
      label = 'Модель',
      choices = models(),
      selected = current_model,
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
          'Umbrella',
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
    
    model_name <- names(models()[as.numeric(input$model)])
    print(model_name)
    
    if (input$similarity_aggregation %in% c('auto', 'token')) {
      aggregation <- ifelse(prefix(), 'cls', 'mean')
    } else {
      aggregation <- input$similarity_aggregation
    }
    
    as_phrases <- (aggregation == 'cls')
    
    tictoc::tic()
    withProgress(
      session = session,
      message = 'Векторизация шкал…',
      {
        res <- purrr::map2(
          scaleset(),
          seq_along(scaleset()),
          function(semantic_scale, i) {
            incProgress(
              1 / length(scaleset()),
              message = 'Векторизация шкалы',
              detail = names(scaleset())[[i]]
            )
            scale_result <- .items_to_norms(
              items = semantic_scale,
              model = model_name,
              as_phrases = as_phrases,
              template = hypotheses(),
              prefix = prefix(),
              group_items = input$similarity_group_items,
              aggregation = aggregation,
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
    bindCache(
      input$model,
      input$similarity_aggregation,
      input$similarity_group_items,
      scaleset()
    )
  
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
    tictoc::tic('Analysing sigle text')
    withProgress(
      session = session,
      message = 'Анализируем…',
      {
        # Get text embeddings and other parameters once
        if (input$method == 'similarity') {
          if (input$similarity_aggregation == 'auto') {
            aggregation <- ifelse(prefix(), 'cls', 'token')
          } else {
            aggregation <- input$similarity_aggregation
          }
          
          tic(msg = 'Embedding text')
          
          prgrphs <- paragraphs(text)
          if (prefix()) prgrphs <- paste('classification:', prgrphs)
          
          incProgress(
            1 / (length(scaleset())^2),
            message = 'Векторизация текста',
          )
          text_embeddings <- text_embed(
            prgrphs,
            model = model_name,
            aggregation_from_tokens_to_texts = aggregation,
            select_token = universal_brand_name,
            device = tolower(input$device)
          )
          toc()
        }
        res <- purrr::map2(
          scaleset(),
          seq_along(scaleset()),
          function(semantic_scale, i) {
            incProgress(
              1 / length(scaleset()),
              message = 'Анализируем',
              detail = names(scaleset())[[i]]
            )
            ##### NLI ----
            if (input$method == 'classification') {
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
            ##### Similarity ----
            } else if (input$method == 'similarity') {
              scale_result <- semdiff_similarity(
                text_embeddings = text_embeddings,
                norm_embeddings = scaleset_norms()[[i]],
                similarity_metric = input$similarity_metric,
                use_softmax = T # Check
              )
            }
            return(scale_result)
          }
        ) |>
          purrr::set_names(names(scaleset()))
      })
    tictoc::toc()
    
    res
  }) |>
    bindCache(input$model, input$text, scaleset(), input$method) |>
    bindEvent(input$submit)
    
  
  ## Вывод ----
  output$result <- renderPrint({
    req(input$submit)
    if (input$method == 'classification') {
      result() |>
        show_scales_result()
    } else {
      result()
    }
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
        value <- mean(result()[[scale_name]][['.score']]) * GAUGE_SCALE
        gauge(
          label = scale_name,
          value,
          min = -GAUGE_SCALE, max = GAUGE_SCALE, 
          symbol = '', 
          gaugeSectors(
            success = c(GAUGE_SCALE * .8, GAUGE_SCALE),
            danger = c(-GAUGE_SCALE, -GAUGE_SCALE * .4),
            warning = c(-GAUGE_SCALE * .4, GAUGE_SCALE * .8)
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
      paste('scaleset-', Sys.Date(), '.sds', sep = '')
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
