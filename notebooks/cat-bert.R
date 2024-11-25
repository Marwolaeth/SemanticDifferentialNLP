library(text)
library(pheatmap)

# Model Selection ----
model <- 'bert-base-uncased' # Love and hate are very close to me tonight©
# model <- 'ynie/roberta-large-snli_mnli_fever_anli_R1_R2_R3-nli' # Way better
# model <- 'abbasgolestani/ag-nli-DeTS-sentence-similarity-v2' # A lesser shit
# model <- 'TFLai/Bert-Multilingual-NLI' # Awful
# model <- 'ynie/electra-large-discriminator-snli_mnli_fever_anli_R1_R2_R3-nli' # Wow
# model <- 'Capreolus/electra-base-msmarco' # Raw
# model <- 'ChrisZeng/electra-large-discriminator-nli-efl-hateval' # Electra <3
# model <- 'ynie/albert-xxlarge-v2-snli_mnli_fever_anli_R1_R2_R3-nli' # Awful
model <- 'cross-encoder/nli-deberta-v3-base' # WOW
model <- 'cross-encoder/nli-deberta-v3-large' # Good for Zero-shot, bad otherwise
model <- 'cross-encoder/nli-deberta-v3-small' # So-so
model <- 'cross-encoder/nli-deberta-v3-xsmall' # Not bad for Zero-shot
model <- 'nvidia/NV-Embed-v2'
model <- 'cross-encoder/qnli-electra-base' # Poor
model <- 'cross-encoder/mmarco-mMiniLMv2-L12-H384-v1' # Awful
model <- 'MendelAI/nv-embed-v2-ontada-twab-peft'
model <- 'facebook/bart-large-mnli'
model <- 'isolation-forest/setfit-absa-polarity' # Pretty
model <- 'sentence-transrormers/paraphrase-MiniLM-L6-v2'
model <- 'sentence-transformers/roberta-large-nli-stsb-mean-tokens'

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

#### The extent to which hidden states are similar or dissimilar
##### according to the given expectations
semantic_divergence <- function(
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
contextual_influence <- function(
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

# The Data ----
## The Concepts ----
### Polarity encoding ----
(verbs_data <- c(
  'love'         =  1,
  'hate'         = -1,
  'like'         =  1,
  "don't like"   = -1,
  'take care of' =  1,
  'screw'        = -1,
  'save'         =  1,
  'dislike'      = -1,
  'adore'        =  1
))
(verbs <- names(verbs_data))

### Word norms ----
(verbs_tbl <- tibble::as_tibble(as.list(verbs) |> setNames(verbs)))
(verb_norms <- textEmbed(
  verbs_tbl,
  model = model,
  layers = -1,
  keep_token_embeddings = FALSE,
  tokenizer_parallelism = TRUE,
  trust_remote_code = TRUE
))
verb_norms$texts$love

### The Texts ----
(texts <- paste(
  'I', verbs, 'cats'
))

## Expectation vectors ----
(eval_matrix <- as.matrix(verbs_data) %*% t(verbs_data))
isSymmetric(eval_matrix)

## The Documents ----
docs <- textEmbed(
  tolower(texts),
  model = model,
  layers = -1,
  # layers = 11:12,
  aggregation_from_layers_to_tokens = 'concatenate',
  keep_token_embeddings = TRUE,
  tokenizer_parallelism = TRUE
)

# The Experiment ----
## Embedding Comparison ----
### Document Embeddings ----
#### Divergence ----
(m_docs <- textSimilarityMatrix(docs$texts$texts))
######## These 3 lines are equivalent ########
sum(m_docs * eval_matrix)
expectation_match(m_docs, eval_matrix)
semantic_divergence(docs$texts$texts, eval_matrix, plot = TRUE)

#### Concept Similarity ----
##### A single concept ----
(love_similarity <- textSimilarityNorm(
  docs$texts$texts,
  verb_norms$texts$love
)) |> setNames(verbs)
expectation_match(love_similarity, verbs_data)
contextual_influence(docs$texts$texts, verb_norms, eval_matrix, plot = TRUE)

### Tokens: Cats ----
(cats <- select_tokens(docs, '.?cats?'))

#### Divergence ----
semantic_divergence(cats, eval_matrix, plot = TRUE)

##### Concept Similarity ----
contextual_influence(cats, verb_norms, eval_matrix, plot = TRUE)

### Tokens: Classifer Token ----
(cls <- select_tokens(docs, 1L))

#### Divergence ----
# Classifier tokens gets the most of meaning
semantic_divergence(cls, eval_matrix, plot = TRUE)

#### Concept Similarity ----
contextual_influence(cls, verb_norms, eval_matrix, plot = TRUE)

### Tokens: I ----
(i <- select_tokens(docs, '.?i$'))
#### Divergence ----
semantic_divergence(i, eval_matrix, plot = TRUE)

#### Concept Similarity ----
contextual_influence(i, verb_norms, eval_matrix, plot = TRUE)

# Repeat for the next model
## or better run a function to test them all!
