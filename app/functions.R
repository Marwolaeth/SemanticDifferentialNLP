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
    mask = c(-1, 1),
    multi_label = FALSE,
    mask_matrix = NULL
) {
  aggregation <- match.arg(aggregation, c('max', 'mean'))
  aggregation <- match.fun(aggregation)
  
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
  
  if (!is.null(mask_matrix)) {
    res_matrix <- res_wide |>
      dplyr::select(dplyr::all_of(polarities)) |>
      as.matrix()
    
    max_scores <- apply(mask_matrix, 1, \(x) sum(x > 0))
    
    return(res_matrix %*% t(mask_matrix) / max_scores)
  }
  
  res_wide |>
    dplyr::mutate(
      score = sum(
        dplyr::c_across(dplyr::all_of(polarities)) * mask
      ) / sum(mask > 0)
    )
}