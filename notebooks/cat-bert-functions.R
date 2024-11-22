library(text)
library(pheatmap)

## Evaluation vectors ----
(eval_matrix <- as.matrix(verbs_data) %*% t(verbs_data))
isSymmetric(eval_matrix)

# Functions ----
## Visualisation ----
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

#### The extent to which hidden states are similar or dissimialar
##### according to the given expectations
meaning_divergence <- function(
    embeddings,
    expectation_mask,
    plot = FALSE
) {
  if (!(length(embeddings[[1]]) == nrow(expectation_mask))) {
    stop('Expectation mask matrix dimensions must math the number of documents')
  }
  
  sim <- textSimilarityMatrix(embeddings)
  
  score <- sum(sim * expectation_mask)
  
  if (plot) text_sumularity_heatmap(sim)
  
  return(score)
}

#### The extent to which a concept leaves a trace in a hidden state
##### of a text or related tokens
concept_admixture <- function(
    embeddings,
    concept_embeddings,
    expectation_mask,
    plot = FALSE
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
  
  if (plot) text_sumularity_heatmap(sim)
  
  return(score)
}