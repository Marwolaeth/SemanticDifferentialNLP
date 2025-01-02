library(text)
library(tokenizers)
library(purrr)
library(tibble)
library(tidyr)
library(dplyr)

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
    # mask_matrix = NULL,
    multi_label = FALSE,
    append_neutral = FALSE
) {
  aggregation <- match.arg(aggregation, c('max', 'mean'))
  aggregation <- match.fun(aggregation)
  
  if (prefix) texts <- paste('classification:', texts)
  
  if (append_neutral) {
    candidate_labels <- c(candidate_labels, 'ничего из перечисленного')
    mask <- c(mask, 0)
  }
  
  res <- textZeroShot(
    texts,
    model = model,
    candidate_labels = candidate_labels,
    hypothesis_template = template,
    multi_label = multi_label,
    tokenizer_parallelism = TRUE
  )
  
  res_wide <- purrr::map(
    seq_along(candidate_labels),
    function(position) {
      p <- as.character(position)
      res |>
        dplyr::select(sequence, dplyr::ends_with(p)) |>
        dplyr::mutate(rating = position) |>
        dplyr::rename(
          label = glue::glue('labels_x_{p}'),
          score = glue::glue('scores_x_{p}')
        )
    }
  ) |>
    dplyr::bind_rows() |>
    tidyr::pivot_wider(
      id_cols = sequence,
      names_from = label,
      values_from = score
    ) |>
    dplyr::select(sequence, dplyr::all_of(candidate_labels))
  
  if (length(texts) > 1) {
    res_wide <- res_wide |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(candidate_labels), aggregation)
      )
  }
  
  # if (!is.null(mask_matrix)) {
  #   res_matrix <- res_wide |>
  #     dplyr::select(dplyr::all_of(candidate_labels)) |>
  #     as.matrix()
  #   
  #   max_scores <- apply(mask_matrix, 1, \(x) sum(x > 0))
  #   
  #   return(res_matrix %*% t(mask_matrix) / max_scores)
  # }
  
  res_wide |>
    dplyr::mutate(
      score = sum(
        dplyr::c_across(dplyr::all_of(candidate_labels)) * mask
      ) / sum(mask > 0)
    ) |>
    dplyr::pull(score)
}

# texts <- c(
#   'Для истинных ценителей моды XCellent представляет собой идеальный выбор, который не поддается массовым трендам.',
#   'Аналитики предполагают, что XCellent сделает шаг вперед, внедрив уникальные решения в свои устройства.',
#   'Каждый раз, когда я ношу вещи от XCellent, получаю комплименты от тех, кто разбирается в моде.',
#   'Благодаря новым разработкам XCellent, многие компании начинают пересматривать свои стратегии.',
#   'XCellent стал символом утонченного вкуса, привлекающим только самых взыскательных покупателей.',
#   'Среди лидеров отрасли, таких как Innovatech и Quantum Systems, XCellent наблюдает за их новыми разработками.',
#   'Кто-нибудь еще помнит, когда XCellent был на слуху? Кажется, это было давно.'
# )
# polarities <- poles[['Инновационность']]
# template <- 'Xcellent - {}'
# model <- 'DeepPavlov/xlm-roberta-large-en-ru-mnli'
# model <- 'Marwolaeth/rosberta-nli-terra-v0'
# prefix <- TRUE

# Функция для обработки нескольких текстов с нулевым обучением
# Функция для семантического дифференциала с нулевым обучением (векторизированная)
#' Семантический дифференциал с нулевым обучением (векторизированный)
#'
#' Эта функция выполняет анализ текста с использованием метода семантического
#' дифференциала, применяя модель нулевого обучения для классификации текстов
#' по заданным полярностям. В отличие от функции `semdiff_zeroshot()`, где
#' аргумент `candidate_labels` используется для указания возможных меток,
#' в этой функции аргумент `polarities` представляет собой список именованных
#' числовых векторов, где имена служат потенциальными метками классов, а
#' значения используются в качестве маски для расчета результата.
#'
#' @param texts Вектор строк, содержащий тексты для анализа.
#' @param model Строка, указывающая модель, используемую для анализа.
#' @param polarities Список именованных числовых векторов, содержащих полярности,
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
#' polarities <- list(
#'   c('устаревший' = -1, 'сдержанный' = 0, 'инновационный'   = 1),
#'   c('отсталый'   = -1, 'стабильный' = 0, 'изобретательный' = 1)
#' )
#' template <- "Этот продукт является {}."
#'
#' result <- semdiff_zeroshot_map(
#'   texts = texts,
#'   model = model,
#'   polarities = polarities,
#'   template = template,
#'   prefix = TRUE
#' )
#' print(result)
#'
#' @export
semdiff_zeroshot_map <- function(
    texts,
    model,
    polarities,
    template,
    prefix = FALSE,
    ...
) {
  
  # Можно добавить несколько наборов меток классов
  ## Тогда их придется обрабатывать последовательно
  if (!is.list(polarities)) polarities <- list(polarities)
  
  proper_format <- vapply(
    polarities,
    \(x) !any(is.null(names(x))) & all(is.numeric(x)),
    logical(1)
  ) |> all()
  
  if (!proper_format) {
    stop('The labels must be named numeric vectors')
  }
  
  # Текстам нужен свой ID
  ids <- tibble::tibble(
    texts = texts,
    text_id = 1:length(texts)
  )
  
  # Генерация всех комбинаций текстов и полярностей
  tidyr::expand_grid(
    texts, polarities
  ) |>
    dplyr::mutate(
      mask = polarities,
      polarities = lapply(polarities, names),
      .score = purrr::pmap_dbl(
        list(texts, polarities, mask),
        function(texts, polarities, mask) {
          semdiff_zeroshot(
            texts = paragraphs(texts),
            model = model,
            candidate_labels = polarities,
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

show_scales_result <- function(result) {
  if (tibble::is_tibble(result)) {
    result |>
      dplyr::rowwise() |>
      dplyr::mutate(
        polarities = paste(polarities, collapse = ' – ')
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-mask, -texts)
  } else if (is.list(result) | !is.data.frame(result)) {
    purrr::map(result, show_scales_result)
  }
}