library(text)

# Utility Functions ----
## Embedding Utilities ----
extract_duration <- function(text_embeddings) {
  m <- regexpr(
    '\\d*\\.\\d+(?=\\s+secs)',
    comment(text_embeddings),
    perl = TRUE
  )
  
  dur <- regmatches(comment(text_embeddings), m)
  
  return(as.numeric(dur))
}

#' Concatenate tokens from transformer embeddings into words
#'
#' @param embeddings A data frame containing token embeddings with at least a 'tokens' column and embedding columns named according to dimension names from the `text` package.
#' @param aggregation A function or character string specifying the aggregation method .
#                ('mean', 'min', 'max', 'sum'). Default is mean.
#'
#' @return A data frame with concatenated tokens and aggregated embeddings.
#' @export
#'
#' @examples 
#' library(text)
#' embeddings <- textEmbed('I adore dogs')
#' tokens <- embeddings$tokens$texts[[1]]
#' concatenate_tokens(tokens, 'mean')
concatenate_tokens <- function(token_embeddings, aggregation = mean) {
  # Check if the embeddings table is empty
  if (nrow(token_embeddings) == 0) stop('The embeddings table is empty')
  
  # Check if the provided aggregation function is valid
  if (is.character(aggregation)) {
    aggregation <- match.arg(aggregation, c('mean', 'min', 'max', 'sum'))
  }
  aggregation <- match.fun(aggregation)
  
  
  is_bert <- token_embeddings$tokens[[1]] == '[CLS]'
  
  special_token_start <- if_else(is_bert, '[', '<')
  word_start <- c('Ġ', '▁')
  subword_indicator <- '#'
  
  result <- token_embeddings |>
    dplyr::mutate(
      first_symbol = substr(tokens, 1, 1),
      special = first_symbol == special_token_start,
      empty = tokens %in% word_start,
      begin = (
        (first_symbol %in% word_start) |
          (!(first_symbol == subword_indicator) & is_bert)
      ) |
        special |
        dplyr::lag(special) |
        empty |
        dplyr::lag(empty),
      group = cumsum(begin),
      tokens = gsub('#+', '', tokens)
    )
  
  if (all(result$begin)) {
    result <- result |>
      dplyr::select(dplyr::all_of(names(token_embeddings)))
  } else {
    result <- result |>
      dplyr::summarise(
        # Concatenate tokens into words
        tokens = paste(tokens, collapse = ''),   
        # Aggregate embeddings
        dplyr::across(
          dplyr::starts_with('Dim'),
          aggregation
        ),
        .by = group
      ) |>
      dplyr::select(-group)
  }
  
  return(result)
}

## Visualisation ----
colours_heatmap <- colorRampPalette(
  rev(
    RColorBrewer::brewer.pal(
      n = 8,
      name = "RdYlBu"
    )
  )
)(200)

#' Create a heatmap for visualizing similarity matrices
#'
#' @param m A numeric matrix, usually a result of `textSimilarityMatrix()` call.
#' @param ... 	Additional arguments passed on to `pheatmap::pheatmap()`.
#'
#' @return Invisibly a `pheatmap` object.
#' @export
#'
#' @examples 
#' library(text)
#' sentences <- c('I adore dogs', 'I like cats')
#' embeddings <- textEmbed(sentences)
#' similarity <- textSimilarityMatrix(embeddings$texts$texts)
#' text_sumularity_heatmap(similarity, labels_row = sentences)
text_sumularity_heatmap <- function(m, ...) {
  pheatmap::pheatmap(
    m,
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    display_numbers = TRUE,
    number_format = '%.2f',
    ...
  )
}

## Similarity with multiple concepts ----
## Computes normalized similarities between text embeddings and concept embeddings.
mapTextSimilarityNorm <- function(
    text_embeddings,
    norm_embeddings,
    method = 'cosine'
) {
  vapply(
    norm_embeddings$texts,
    \(norm) textSimilarityNorm(text_embeddings, norm, method = method),
    numeric(nrow(text_embeddings)),
    USE.NAMES = TRUE
  )
}

norm_cosine_similarity <- function(
    text_embeddings,
    norm_embeddings
) {
  texts <- text_embeddings |>
    dplyr::select(dplyr::starts_with('Dim')) |>
    as.matrix()
  concepts <- norm_embeddings$texts |>
    purrr::map(as.matrix) |>
    purrr::reduce(rbind)
  
  
  texts_norms <- sqrt(rowSums(texts^2))
  concept_norms <- sqrt(rowSums(concepts^2))

  S <- (texts %*% t(concepts)) / (texts_norms %*% t(concept_norms))
  colnames(S) <- names(norm_embeddings$texts)
  return(S)
}

## Evaluation ----
### For standalone experiments ----
expectation_match <- function(
    similarity_matrix,
    contrast_matrix
) {
  if (
    !(is.null(dim(similarity_matrix)) & is.null(dim(contrast_matrix))) &
    !all(dim(similarity_matrix) == dim(contrast_matrix))
  ) {
    stop("Dimensions of similarity and expectation mask matrices must match")
  }
  
  if (is.null(dim(similarity_matrix))) {
    return(t(similarity_matrix) %*% contrast_matrix)
  } else {
    return(sum(similarity_matrix * contrast_matrix))
  }
}

### Test different parts of the model output ----
#### Select a token from text embedding object by position or regex
select_tokens <- function(
    text_embeddings,
    which_token = 1L
) {
  
  if (is.character(which_token)) {
    matcher <- function(doc) doc[grep(which_token, doc$tokens), ]
  } else if (is.numeric(which_token)) {
    matcher <- function(doc) doc[which_token, ]
  } else {
    stop('Selection predicate type is not supported')
  }
  
  target_token_embeddings <- dplyr::bind_rows(
    lapply(
      text_embeddings$tokens[[1]], matcher
    )
  )
  return(target_token_embeddings)
}

#### The extent to which hidden states are similar or dissimilar
##### according to the given expectations
semantic_divergence <- function(
    embeddings,
    contrast_matrix,
    plot = FALSE,
    ...
) {
  if (!(length(embeddings[[1]]) == nrow(contrast_matrix))) {
    stop('Expectation mask matrix dimensions must math the number of documents')
  }
  
  sim <- textSimilarityMatrix(embeddings)
  
  score <- sum(sim * contrast_matrix)
  
  if (plot) text_sumularity_heatmap(
    sim,
    color = colours_heatmap,
    breaks = seq(-1, 1, length.out = length(colours_heatmap) + 1),
    ...
  )
  
  return(score)
}

#### The extent to which a concept leaves a trace in a hidden state
##### of a text or related tokens
contextual_influence <- function(
    embeddings,
    concept_embeddings,
    contrast_matrix,
    metric = 'cosine',
    plot = FALSE,
    ...
) {
  if (
    !(
      length(embeddings[[1]]) == nrow(contrast_matrix) &
      length(concept_embeddings[[1]]) == ncol(contrast_matrix)
    )
  ) {
    stop('Expectation mask matrix must be of shape (n_texts × n_concepts)')
  }
  
  sim <- mapTextSimilarityNorm(embeddings, concept_embeddings, method = metric)
  
  score <- sum(sim * contrast_matrix)
  
  if (plot) text_sumularity_heatmap(
    sim,
    color = colours_heatmap,
    breaks = seq(-1, 1, length.out = length(colours_heatmap) + 1),
    ...
  )
  
  return(score)
}

# Tests Functions ----

## Test embeddings capturing meaning ----
.semantic_divergence_safe <- purrr::safely(
  function(embeddings, contrast_matrix) {
    sim <- textSimilarityMatrix(embeddings)
    sum(sim * contrast_matrix)
  },
  otherwise = NaN
)

.contextual_influence_safe <- purrr::safely(
  function(
    embeddings,
    concept_embeddings,
    contrast_matrix,
    metric = 'cosine'
  ) {
    sim <- mapTextSimilarityNorm(
      embeddings,
      concept_embeddings,
      method = metric
    )
    sum(sim * contrast_matrix)
  },
  otherwise = NaN
)

.check_inner_contrast <- function(m, n) {
  if (!all(nrow(m) ==  n, ncol(m) == n)) {
    stop('Both inner contrast matrix dimensions must math the number of documents')
  }
}

.check_outer_contrast <- function(m, n_docs, n_norms) {
  if (
    !(n_docs == nrow(m) & n_norms == ncol(m))
  ) {
    stop(
      'Expectation outer similarity matrices must be of shape (n_texts × n_concepts)'
    )
  }
}

.evaluate_embedding <- function(
    test_type,
    test_level,
    token,
    metric = 'cosine',
    text_embeddings,
    norm_embeddings,
    contrast_matrices,
    benchmark_values
) {
  test_type <- match.arg(test_type, c('inner', 'outer'), several.ok = FALSE)
  if (test_type == 'inner') {
    test <- function(e, norms, m, metric) {
      .semantic_divergence_safe(e, m)[['result']]
    }
  } else {
    test <- function(e, norms, m) {
      .contextual_influence_safe(e, norms, m, metric)[['result']]
    }
  }
  
  contrasts <- contrast_matrices[[test_type]]
  benchmark <- benchmark_values[[test_type]]
  
  if (test_level == 'token') {
    if (grepl('^\\d+$', token)) token <- as.integer(token)
    embeddings <- select_tokens(text_embeddings, token)
  } else {
    embeddings <- text_embeddings$texts$texts
  }
  
  purrr::map2(
    contrasts,
    benchmark,
    \(m, b) {
      tibble::tibble(
        value = test(embeddings, norm_embeddings, m),
        rating = value / b
      )
    }
  ) |>
    dplyr::bind_rows(.id = 'contrast')
}

test_embeddings <- function(
    model,
    corpus,
    expected_inner_similarities,
    concepts = NULL,
    expected_outer_similarities = NULL,
    tokens = NULL,
    layers = -1,
    similarity_metrics = 'cosine',
    bind_aggregate_scores = FALSE,
    ...
) {
  #### Check argument validity ----
  if (is.list(expected_inner_similarities)) {
    purrr::walk(
      expected_inner_similarities,
      .check_inner_contrast,
      length(corpus)
    )
    
    # Contrast names to be used in the output
    if (is.null(names(expected_inner_similarities))) {
      names(expected_inner_similarities) <- paste0(
        'contrast', seq_along(expected_inner_similarities)
      )
      warning(
        'No names for expected inner similarity matrices provided. Using auto names'
      )
    }
  } else {
    .check_inner_contrast(expected_inner_similarities, length(corpus))
    
    # So that we don't have to check whether it is a list
    expected_inner_similarities <- list(
      inner_contrast = expected_inner_similarities
    )
  }
  
  if (!is.null(concepts)) {
    if (is.null(expected_outer_similarities)) {
      warning(
        'Contrast matrices for outer similarities are not provided. Skipping outer match tests'
      )
      concepts <- NULL
    } else {
      if (is.list(expected_outer_similarities)) {
        purrr::walk(
          expected_outer_similarities,
          .check_outer_contrast,
          length(corpus), length(concepts)
        )
        
        if (is.null(names(expected_outer_similarities))) {
          names(expected_outer_similarities) <- paste0(
            'contrast', seq_along(expected_outer_similarities)
          )
          warning(
            'No names for expected outer similarity matrices provided. Using auto names'
          )
        }
      } else {
        .check_outer_contrast(
          expected_outer_similarities, length(corpus), length(concepts)
        )
        
        # So that we don't have to check whether it is a list
        expected_outer_similarities <- list(
          outer_contrast = expected_outer_similarities
        )
      }
    }
  }
  
  #### Benchmark (maximum possible) values ----
  benchmarks <- list(
    inner = purrr::map(
      expected_inner_similarities,
      \(x) sum(x^2) # Element-wise power
    ),
    outer = if (is.null(expected_outer_similarities)) {
      list(contrasts = NaN)
    } else {
      purrr::map(
        expected_outer_similarities,
        \(x) sum(x^2)
      )
    }
  )
  
  #### Create embeddings ----
  ##### Word Norms ----
  if (!is.null(concepts)) {
    concepts_df <- tibble::as_tibble(as.list(concepts) |> setNames(concepts))
    norms <- textEmbed(
      concepts_df,
      model = model,
      layers = layers,
      keep_token_embeddings = FALSE,
      aggregation_from_layers_to_tokens = 'concatenate',
      aggregation_from_tokens_to_texts = 'mean',
      trust_remote_code = TRUE,
      remove_non_ascii = FALSE,
      tokenizer_parallelism = TRUE,
      ...
    )
    duration_norms <- extract_duration(norms)
  } else {
    norms <- NULL
    duration_norms <- NaN
  }
  
  ##### Documents ----
  docs <- textEmbed(
    corpus,
    model = model,
    layers = layers,
    aggregation_from_layers_to_tokens = 'concatenate',
    aggregation_from_tokens_to_texts = 'mean',
    keep_token_embeddings = TRUE,
    decontextualize = FALSE,
    trust_remote_code = TRUE,
    remove_non_ascii = FALSE,
    tokenizer_parallelism = TRUE,
    ...
  )
  duration_docs <- extract_duration(docs)
  
  #### The Tests ----
  contrast_matrices <- list(
    inner = expected_inner_similarities,
    outer = expected_outer_similarities
  )

  result <- tidyr::expand_grid(
    test_type = c('inner', if (!is.null(concepts)) 'outer' else NA_character_),
    test_level = c('document', 'token'),
    token = c(NA_character_, unlist(tokens)),
    metric = similarity_metrics
  ) |>
    dplyr::filter(
      xor(test_level == 'token', is.na(token)) & !is.na(test_type)
    )
  result <- result |>
    dplyr::mutate(
      res = purrr::pmap(
        result,
        .evaluate_embedding,
        text_embeddings = docs,
        norm_embeddings = norms,
        contrast_matrices = contrast_matrices,
        benchmark_values = benchmarks
      )
    ) |>
    tidyr::unnest(res) |>
    dplyr::mutate(
      token = dplyr::if_else(token == '1', '[CLS]', token),
      dplyr::across(
        c(test_type, test_level, contrast, metric),
        factor
      )
    )
  
  ##### Aggregate Scores ----
  if (bind_aggregate_scores) {
    model_scores <- result |>
      dplyr::summarise(
        dplyr::across(
          value,
          c(mean = mean, max = max),
          .names = '{.fn}'
        ),
        .by = c(test_type, contrast)
      ) |>
      tidyr::pivot_wider(
        names_from = c(test_type, contrast),
        values_from = c(mean, max)
      )
  } else {
    model_scores <- NULL
  }

  #### Final Value ----
  tibble::tibble(
    model = model,
    layers = paste(as.character(layers), collapse = ':'),
    duration_corpus = duration_docs,
    duration_concepts = duration_norms,
    results = result
  ) |>
    dplyr::bind_cols(model_scores)
}
test_embeddings <- compiler::cmpfun(test_embeddings, options = list(optimize=3))

## Test zero-shot classification quality ----
test_zero_shot <- function(...) {...}
