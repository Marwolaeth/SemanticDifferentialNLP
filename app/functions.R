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
    aggregation = c('max', 'mean'),
    mask = c(-1, 1)
) {
  aggregation <- match.arg(aggregation, c('max', 'mean'))
  aggregation <- match.fun(aggregation)
  
  res <- textZeroShot(
    texts,
    model = model,
    candidate_labels = polarities,
    hypothesis_template = template,
    multi_label = FALSE,
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
  
  res_wide |>
    summarise(
      across(all_of(polarities), aggregation)
    ) |>
    mutate(score = sum(c_across(all_of(polarities)) * mask))
}