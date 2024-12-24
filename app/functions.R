library(text)
library(tokenizers)
library(purrr)
library(tibble)
library(tidyr)
library(dplyr)

paragraphs <- function(text) {
  tokenizers::tokenize_paragraphs(
    text,
    paragraph_break = '\n',
    simplify = TRUE
  )
}

semdiff_zeroshot <- function(
    texts,
    model,
    polarities,
    template,
    prefix = FALSE,
    aggregation = c('max', 'mean'),
    mask = c(-1, 1),
    # mask_matrix = NULL,
    multi_label = FALSE
) {
  aggregation <- match.arg(aggregation, c('max', 'mean'))
  aggregation <- match.fun(aggregation)
  
  if (prefix) texts <- paste('classification:', texts)
  
  res <- textZeroShot(
    texts,
    model = model,
    candidate_labels = polarities,
    hypothesis_template = template,
    multi_label = multi_label,
    tokenizer_parallelism = TRUE
  )
  
  res_wide <- purrr::map(
    seq_along(polarities),
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
    dplyr::select(sequence, dplyr::all_of(polarities))
  
  if (length(texts) > 1) {
    res_wide <- res_wide |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(polarities), aggregation)
      )
  }
  
  # if (!is.null(mask_matrix)) {
  #   res_matrix <- res_wide |>
  #     dplyr::select(dplyr::all_of(polarities)) |>
  #     as.matrix()
  #   
  #   max_scores <- apply(mask_matrix, 1, \(x) sum(x > 0))
  #   
  #   return(res_matrix %*% t(mask_matrix) / max_scores)
  # }
  
  res_wide |>
    dplyr::mutate(
      score = sum(
        dplyr::c_across(dplyr::all_of(polarities)) * mask
      ) / sum(mask > 0)
    ) |>
    pull(score)
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
  
  # каждый текст анализируем по каждой паре гипотез
  tidyr::expand_grid(
    texts, polarities
  ) |>
    mutate(
      mask = polarities,
      polarities = lapply(polarities, names),
      score = purrr::pmap_dbl(
        list(texts, polarities, mask),
        function(texts, polarities, mask) {
          semdiff_zeroshot(
            texts = paragraphs(texts),
            model = model,
            polarities = polarities,
            template = template,
            mask = mask,
            multi_label = FALSE,
            prefix = prefix
          )
        }
      )
    )
  
  # res <- analysis_grid |>
  #   dplyr::left_join(ids, by = 'texts') |>
  #   dplyr::group_by(text_id) |>
  #   dplyr::summarise(across(score, mean)) |>
  #   dplyr::left_join(ids, by = 'text_id') |>
  #   dplyr::relocate(texts, .after = 1)
}