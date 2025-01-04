library(text)
library(tokenizers)
library(purrr)
library(tibble)
library(tidyr)
library(dplyr)

## Text Utils ----
reticulate::source_python(
  system.file(
    'python',
    'huggingface_Interface3.py',
    package = 'text',
    mustWork = TRUE
  )
)

## Utils ----
# Функция для токенизации текста на параграфы
#' Токенизация текста на параграфы
#'
#' Эта функция принимает текст в виде строки и разбивает его на параграфы,
#' используя символы новой строки для определения границ параграфов.
#'
#' @param text Строка, содержащая текст, который необходимо токенизировать.
#'             Ожидается, что параграфы отделены символом новой строки ('\n').
#'
#' @return Возвращает вектор параграфов, полученных из входного текста.
#'         Если текст пустой, возвращается пустой вектор.
#'
#' @examples
#' # Пример использования функции
#' text <- "Первый параграф.\nВторой параграф.\n\nТретий параграф."
#' paragraphs(text)
#' # Возвращает: c("Первый параграф.", "Второй параграф.", "Третий параграф.")
#'
#' # Пример с пустым текстом
#' paragraphs("")
#' # Возвращает: character(0)
#'
#' @export
paragraphs <- function(text) {
  tokenizers::tokenize_paragraphs(
    text,
    paragraph_break = '\n',
    simplify = TRUE
  )
}

.check_scale <- function(scale) {
  proper_format <- vapply(
    scale,
    \(x) !any(is.null(names(x))) & all(is.numeric(x)),
    logical(1)
  ) |> all()
  
  if (!proper_format) {
    stop('The labels must be named numeric vectors')
  }
}

## Analysis ----
### Backend Functions ----
# Функция для семантического дифференциала с нулевым обучением
#' Семантический дифференциал с нулевым обучением
#'
#' Эта функция выполняет анализ текста с использованием метода семантического
#' дифференциала, применяя модель нулевого обучения для классификации текстов
#' по заданным полярностям. Она позволяет оценить текстовые данные по 
#' различным критериям, таким как инновационность или популярность.
#'
#' @param texts Вектор строк, содержащий тексты для анализа.
#' @param model Строка, указывающая модель, используемую для анализа.
#' @param candidate_labels Вектор строк, содержащий названия полярностей для оценки.
#' @param template Шаблон гипотезы, который будет использоваться для классификации.
#' @param prefix Логическое значение, указывающее, следует ли добавлять префикс
#'                к текстам перед классификацией. По умолчанию FALSE.
#' @param aggregation Метод агрегации для объединения оценок (можно выбрать 'max' или 'mean').
#' @param mask Вектор, определяющий влияние каждой полярности на финальный результат.
#'              По умолчанию c(-1, 1).
#' @param multi_label Логическое значение, указывающее, поддерживает ли модель
#'                    многоклассовую классификацию. По умолчанию FALSE.
#'
#' @return Возвращает числовое значение, представляющее собой итоговый балл
#'         для текста, рассчитанный на основе оценок по полярностям и маске.
#'         Результат можно интерпретировать как обобщенную оценку текста:
#'         - Положительное значение указывает на более высокую оценку по
#'           положительным полярностям.
#'         - Отрицательное значение указывает на более высокую оценку по
#'           отрицательным полярностям.
#'         - Значение близкое к нулю может указывать на сбалансированное
#'           восприятие текста по рассматриваемым полярностям.
#'
#' @examples
#' # Пример использования функции
#' texts <- c("Это новый и интересный продукт.", "Старая модель неэффективна.")
#' model <- "model_name"
#' candidate_labels <- c("инновационный", "устаревший")
#' template <- "Этот продукт является {}."
#'
#' result <- semdiff_zeroshot(
#'   texts = texts,
#'   model = model,
#'   candidate_labels = candidate_labels,
#'   template = template,
#'   mask = c(1, -1),
#'   prefix = TRUE
#' )
#' print(result)
#'
#' @export
semdiff_zeroshot <- function(
    texts,
    model,
    candidate_labels,
    template,
    prefix = FALSE,
    aggregation = c('max', 'mean'),
    mask = c(-1, 1),
    multi_label = FALSE,
    append_neutral = FALSE,
    seed = 111,
    device = 'cpu'
) {
  aggregation <- match.arg(aggregation, c('max', 'mean'))
  aggregation <- match.fun(aggregation)
  
  if (prefix) texts <- paste('classification:', texts)
  
  if (append_neutral) {
    candidate_labels <- c(candidate_labels, 'ничего из перечисленного')
    mask <- c(mask, 0)
  }
  
  res <- hgTransformerGetZeroShot(
    sequences = texts,
    candidate_labels = candidate_labels,
    hypothesis_template = template,
    multi_label = FALSE,
    model = model,
    device = device,
    tokenizer_parallelism = FALSE, # To be checked!
    logging_level = 'error',
    force_return_results = FALSE,
    set_seed = seed
  )
  
  res_wide <- res |>
    dplyr::bind_rows() |>
    tidyr::pivot_wider(
      id_cols = sequence,
      names_from = labels,
      values_from = scores
    ) |>
    dplyr::select(sequence, dplyr::all_of(candidate_labels))
  
  if (length(texts) > 1) {
    res_wide <- res_wide |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(candidate_labels), aggregation)
      )
  }
  
  res_wide |>
    dplyr::mutate(
      score = sum(
        dplyr::c_across(dplyr::all_of(candidate_labels)) * mask
      ) / sum(mask > 0)
    ) |>
    dplyr::pull(score)
}

text_embed <- function(
    texts,
    model,
    decontextualize = FALSE,
    keep_token_embeddings = TRUE,
    device = 'cpu',
    tokenizer_parallelism = FALSE,
    trust_remote_code = TRUE,
    logging_level = 'error',
    max_token_to_sentence = 4,
    sort = TRUE
) {
  layers <- get_number_of_hidden_layers(
    model,
    trust_remote_code = trust_remote_code
  )
  
  data_character_variables <- text:::select_character_v_utf8(texts)
  
  if (!decontextualize) {
    x <- data_character_variables
    sorted_layers_ALL_variables <- list()
    sorted_layers_ALL_variables$context_tokens <- list()
    # Loop over all character variables; i_variables = 1
    for (i_variables in seq_len(length(data_character_variables))) {
      T1_variable <- Sys.time()
      # Python file function to HuggingFace
      hg_embeddings <- hgTransformerGetEmbedding(
        text_strings = x[[i_variables]],
        model = model,
        layers = layers,
        return_tokens = TRUE,
        device = reticulate::r_to_py(device),
        tokenizer_parallelism = tokenizer_parallelism,
        model_max_length = NULL,
        max_token_to_sentence = max_token_to_sentence,
        hg_gated = FALSE,
        hg_token = Sys.getenv('HUGGINGFACE_TOKEN', unset = ''),
        trust_remote_code = trust_remote_code,
        logging_level = logging_level
      )
      
      if (sort) {
        variable_x <- text:::sortingLayers(
          x = hg_embeddings,
          layers = layers,
          return_tokens = TRUE
        )
      } else {
        variable_x <- hg_embeddings
      }
      
      sorted_layers_ALL_variables$context_tokens[[i_variables]] <- variable_x
      names(
        sorted_layers_ALL_variables$context_tokens
      )[[i_variables]] <- names(x)[[i_variables]]
    }
  }
}
  
  similarity_norm <- function(
    text_embeddings,
    norm_embeddings,
    metric = 'cosine'
  ) {
    metric <- match.arg(metric, c('cosine', 'spearman', 'pearson', 'kendall'))
    
    texts <- text_embeddings |>
      dplyr::select(dplyr::starts_with('Dim')) |>
      as.matrix()
    concepts <- norm_embeddings$texts |>
      purrr::map(as.matrix) |>
      purrr::reduce(rbind)
    
    if (metric == 'cosine') {
      texts_norms <- sqrt(rowSums(texts^2))
      concept_norms <- sqrt(rowSums(concepts^2))
      
      S <- (texts %*% t(concepts)) / (texts_norms %*% t(concept_norms))
    } else {
      S <- cor(t(texts), t(concepts), method = metric)
    }
    
    colnames(S) <- names(norm_embeddings$texts)
    return(S)
  }
  similarity_norm <- compiler::cmpfun(similarity_norm, options = list(optimize=3))
  
  # texts <- c(
  #   'Для истинных ценителей моды XCellent представляет собой идеальный выбор, который не поддается массовым трендам.',
  #   'Аналитики предполагают, что XCellent сделает шаг вперед, внедрив уникальные решения в свои устройства.',
  #   'Каждый раз, когда я ношу вещи от XCellent, получаю комплименты от тех, кто разбирается в моде.',
  #   'Благодаря новым разработкам XCellent, многие компании начинают пересматривать свои стратегии.',
  #   'XCellent стал символом утонченного вкуса, привлекающим только самых взыскательных покупателей.',
  #   'Среди лидеров отрасли, таких как Innovatech и Quantum Systems, XCellent наблюдает за их новыми разработками.',
  #   'Кто-нибудь еще помнит, когда XCellent был на слуху? Кажется, это было давно.'
  # )
  # items <- poles[['Инновационность']]
  # template <- 'Xcellent - {}'
  # model <- 'DeepPavlov/xlm-roberta-large-en-ru-mnli'
  # model <- 'Marwolaeth/rosberta-nli-terra-v0'
  # prefix <- TRUE
  
  ### Wrapper Functions ----
  # Функция для обработки нескольких текстов с нулевым обучением
  # Функция для семантического дифференциала с нулевым обучением (векторизированная)
  #' Семантический дифференциал с нулевым обучением (векторизированный)
  #'
  #' Эта функция выполняет анализ текста с использованием метода семантического
  #' дифференциала, применяя модель нулевого обучения для классификации текстов
  #' по заданным полярностям. В отличие от функции `semdiff_zeroshot()`, где
  #' аргумент `candidate_labels` используется для указания возможных меток,
  #' в этой функции аргумент `items` представляет собой список именованных
  #' числовых векторов, где имена служат потенциальными метками классов, а
  #' значения используются в качестве маски для расчета результата.
  #'
  #' @param texts Вектор строк, содержащий тексты для анализа.
  #' @param model Строка, указывающая модель, используемую для анализа.
  #' @param items Список именованных числовых векторов, содержащих полярности,
  #'                   которые будут оцениваться для каждого текста. Имена векторов
  #'                   служат метками классов, а значения — маской для расчета результата.
  #' @param template Шаблон гипотезы, который будет использоваться для классификации.
  #' @param prefix Логическое значение, указывающее, следует ли добавлять префикс
  #'                к текстам перед классификацией. По умолчанию FALSE.
  #' @param ... Прочие аргументы, передаваемые в `semdiff_zeroshot()`.
  #'
  #' @return Возвращает дата-фрейм, содержащий для каждого текста
  #'         усредненные оценки по всем указанным полярностям. Каждая строка
  #'         соответствует тексту, а переменная `.score` – оценке данного текста.
  #'         Результат можно интерпретировать
  #'         как обобщенные оценки текстов:
  #'         - Положительные значения указывают на более высокую оценку по
  #'           положительным полярностям.
  #'         - Отрицательные значения указывают на более высокую оценку по
  #'           отрицательным полярностям.
  #'         - Значения близкие к нулю могут указывать на сбалансированное
  #'           восприятие текста по рассматриваемым полярностям.
  #'
  #' @examples
  #' # Пример использования функции
  #' texts <- c("Это новый и интересный продукт.", "Старая модель неэффективна.")
  #' model <- "model_name"
  #' items <- list(
  #'   c('устаревший' = -1, 'сдержанный' = 0, 'инновационный'   = 1),
  #'   c('отсталый'   = -1, 'стабильный' = 0, 'изобретательный' = 1)
  #' )
  #' template <- "Этот продукт является {}."
  #'
  #' result <- semdiff_zeroshot_map(
  #'   texts = texts,
  #'   model = model,
  #'   items = items,
  #'   template = template,
  #'   prefix = TRUE
  #' )
  #' print(result)
  #'
  #' @export
  semdiff_zeroshot_map <- function(
    texts,
    model,
    items,
    template,
    prefix = FALSE,
    ...
  ) {
    
    # Можно добавить несколько наборов меток классов
    ## Тогда их придется обрабатывать последовательно
    if (!is.list(items)) items <- list(items)
    
    .check_scale(items)
    
    # Текстам нужен свой ID
    ids <- tibble::tibble(
      texts = texts,
      text_id = 1:length(texts)
    )
    
    # Генерация всех комбинаций текстов и полярностей
    tidyr::expand_grid(
      texts, items
    ) |>
      dplyr::mutate(
        mask = items,
        items = lapply(items, names),
        .score = purrr::pmap_dbl(
          list(texts, items, mask),
          function(texts, items, mask) {
            semdiff_zeroshot(
              texts = paragraphs(texts),
              model = model,
              candidate_labels = items,
              template = template,
              mask = mask,
              multi_label = FALSE,
              prefix = prefix,
              ...
            )
          }
        )
      )
    
    # Объединение результатов с идентификаторами текстов
    # res <- analysis_grid |>
    #   dplyr::left_join(ids, by = 'texts') |>
    #   dplyr::group_by(text_id) |>
    #   dplyr::summarise(across(.score, mean)) |>
    #   dplyr::left_join(ids, by = 'text_id') |>
    #   dplyr::relocate(texts, .after = 1)
  }
  
  scaleset <- list(
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
  
  scale <- scaleset[[1]]
  items <- scale
  model <- 'Marwolaeth/rosberta-nli-terra-v0'
  prefix <- TRUE
  
  semdiff_similarity_map <- function(
    texts,
    model,
    items,
    template,
    prefix = FALSE,
    ...
  ) {
    if (!is.list(items)) items <- list(items)
    
    .check_scale(items)
    
    items <- do.call(cbind, lapply(items, t))
    concepts <- colnames(items)
    
    if (prefix) {
      texts    <- paste('classification:', texts)
      concepts <- paste('classification:', concepts)
    }
    
    concepts_df <- tibble::as_tibble(
      as.list(concepts) |> setNames(colnames(items))
    )
    
    concept_embeds <- textEmbed(
      concepts_df,
      model = model,
      layers = -1,
      # keep_token_embeddings = FALSE,
      # decontextualize = TRUE,
      aggregation_from_tokens_to_texts = 'mean',
      trust_remote_code = TRUE,
      remove_non_ascii = FALSE
    )
  }
  
  ## Results Processing ----
  show_scales_result <- function(result) {
    if (tibble::is_tibble(result)) {
      result |>
        dplyr::rowwise() |>
        dplyr::mutate(
          items = paste(items, collapse = ' – ')
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-mask, -texts)
    } else if (is.list(result) | !is.data.frame(result)) {
      purrr::map(result, show_scales_result)
    }
  }