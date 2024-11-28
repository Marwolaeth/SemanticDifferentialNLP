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
#' @param embeddings A data frame containing token embeddings with at least a 'tokens' column and embedding columns named according to dimension names from the `text` package
#' @param aggregation A function or character string specifying the aggregation method 
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
mapTextSimilarityNorm <- function(
    text_embeddings,
    norm_embeddings
) {
  vapply(
    norm_embeddings$texts,
    \(norm) textSimilarityNorm(text_embeddings, norm),
    numeric(nrow(text_embeddings)),
    USE.NAMES = TRUE
  )
}

## Evaluation ----
### For standalone experiments ----
expectation_match <- function(
    similarity_matrix,
    expectation_mask
) {
  if (
    !(is.null(dim(similarity_matrix)) & is.null(dim(expectation_mask))) &
    !all(dim(similarity_matrix) == dim(expectation_mask))
  ) {
    stop("Dimensions of similarity and expectation mask matrices must match")
  }
  
  if (is.null(dim(similarity_matrix))) {
    return(t(similarity_matrix) %*% expectation_mask)
  } else {
    return(sum(similarity_matrix * expectation_mask))
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
    expectation_mask,
    plot = FALSE,
    ...
) {
  if (!(length(embeddings[[1]]) == nrow(expectation_mask))) {
    stop('Expectation mask matrix dimensions must math the number of documents')
  }
  
  sim <- textSimilarityMatrix(embeddings)
  
  score <- sum(sim * expectation_mask)
  
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
    expectation_mask,
    plot = FALSE,
    ...
) {
  if (
    !(
      length(embeddings[[1]]) == nrow(expectation_mask) &
      length(concept_embeddings[[1]]) == ncol(expectation_mask)
    )
  ) {
    stop('Expectation mask matrix must be of shape (n_texts × n_concepts)')
  }
  
  sim <- mapTextSimilarityNorm(embeddings, concept_embeddings)
  
  score <- sum(sim * expectation_mask)
  
  if (plot) text_sumularity_heatmap(
    sim,
    color = colours_heatmap,
    breaks = seq(-1, 1, length.out = length(colours_heatmap) + 1),
    ...
  )
  
  return(score)
}

# Tests Functions ----
corpus <- texts
concepts <- verbs
expectation_mask_texts <- expectation_mask_concepts <- eval_matrix
layers <- -2:-1

test_embeddings <- function(
    model,
    corpus,
    expectation_mask_texts,
    concepts = NULL,
    expectation_mask_concepts = NULL,
    tokens = NULL,
    layers = -1,
    ...
) {
  #### Check argument validity ----
  if (!(length(corpus) == nrow(expectation_mask_texts))) {
    stop('Expectation mask matrix dimensions must math the number of documents')
  }
  
  if (!is.null(concepts)) {
    if (is.null(expectation_mask_concepts)) {
      warning(
        'Expectation mask for concept match is not provided. Skipping concept match test'
      )
      concepts <- NULL
    } else {
      if (
        !(
          length(corpus) == nrow(expectation_mask_concepts) &
          length(concepts) == ncol(expectation_mask_concepts)
        )
      ) {
        stop(
          'Expectation mask matrix must be of shape (n_texts × n_concepts)'
        )
      }
    }
  }
  
  #### Create embeddings ----
  ##### Concept word-norms ----
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
    tokenizer_parallelism = TRUE
  )
  
  ##### The Documents ----
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
    tokenizer_parallelism = TRUE
  )
  
  #### The Tests ----
  
  doc_divergence <- tryCatch(
    semantic_divergence(
      docs$texts$texts,
      expectation_mask_texts,
      plot = FALSE
    ),
    error = function(e) NaN
  )
  
  tibble::tibble(
    model = model,
    duration_corpus = extract_duration(docs),
    duration_concepts = extract_duration(norms),
    result = doc_divergence
  )
}
