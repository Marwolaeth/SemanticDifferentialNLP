library(text)
library(data.table) # for %like% and %chin% operators
library(pheatmap)

# The Verbs ----
## Polarity encoding ----
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

## Word norms ----
(verbs_tbl <- tibble::as_tibble(as.list(verbs) |> setNames(verbs)))
(texts <- paste(
  'I', verbs, 'cats'
))

(verb_norms <- textEmbed(
  verbs_tbl,
  model = model,
  layers = -1,
  keep_token_embeddings = FALSE,
  tokenizer_parallelism = TRUE
))
verb_norms$texts$love

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

token_meaning_divergence <- function(
    token_embeddings,
    expectation_mask,
    plot = FALSE
) {
  if (!(length(token_embeddings$tokens[[1]]) == nrow(expectation_mask))) {
    stop('Expectation mask matrix dimensions must math the number of documents')
  }
  
  sim <- textSimilarityMatrix(token_embeddings)
  
  score <- sum(sim * expectation_mask)
  
  if (plot) text_sumularity_heatmap(sim)
  
  return(score)
}
token_concept_discrimination <- function(
    token_embeddings,
    concept_embeddings,
    expectation_mask,
    plot = FALSE
) {
  
}

# TBD
text <- 'В 2024 году Роскомнадзор удалил более 670 тыс. ссылок на пиратский контент. Под защиту РКН попали более 17 тыс. объектов авторских и смежных прав'

# Now


# Model Explorer ----
## English ----
model <- 'bert-base-uncased' # Love and hate are very close to me tonight©
model <- 'ynie/roberta-large-snli_mnli_fever_anli_R1_R2_R3-nli' # Way better
model <- 'abbasgolestani/ag-nli-DeTS-sentence-similarity-v2' # A lesser shit
model <- 'TFLai/Bert-Multilingual-NLI' # Awful
model <- 'ynie/electra-large-discriminator-snli_mnli_fever_anli_R1_R2_R3-nli' # Wow
model <- 'Capreolus/electra-base-msmarco' # Raw
model <- 'ChrisZeng/electra-large-discriminator-nli-efl-hateval' # Electra <3
# textEmbedRawLayers(
#   tolower(texts),
#   model = model,
#   layers = -1,
#   tokenizer_parallelism = TRUE,
#   word_type_embeddings = TRUE
# )
docs <- textEmbed(
  tolower(texts),
  model = model,
  layers = -1,
  # layers = 11:12,
  aggregation_from_layers_to_tokens = 'concatenate',
  keep_token_embeddings = TRUE,
  tokenizer_parallelism = TRUE
)
# str(docs, 1)
# docs$word_types
# docs$tokens
# comment(docs$word_types)
(m_docs <- textSimilarityMatrix(docs$texts$texts))
text_sumularity_heatmap(eval_matrix)
text_sumularity_heatmap(m_docs, labels_row = texts)
sum(m_docs * eval_matrix)
expectation_match(m_docs, eval_matrix)

(love_similarity <- textSimilarityNorm(
  docs$texts$texts,
  verb_norms$texts$love
))
text_sumularity_heatmap(love_similarity, labels_row = texts)
(love_similarity * verbs_data) |> sum()
t(love_similarity) %*% verbs_data
expectation_match(love_similarity, verbs_data)

doc_verb_similarity <- mapTextSimilarityNorm(docs$texts$texts, verb_norms)
text_sumularity_heatmap(
  doc_verb_similarity,
  labels_row = texts,
  labels_col = verbs,
  main = 'Similarity of sentence embeddings with verb lexeme embeddings'
)
expectation_match(doc_verb_similarity, eval_matrix)

(cats <- purrr::map_dfr(
  docs$tokens$texts, \(x) x[which(x$tokens %like% '.?cats'), ])
)
(m_obj <- textSimilarityMatrix(cats))
text_sumularity_heatmap(m_obj, labels_row = texts, labels_col = texts)
token_meaning_divergence(docs, '.?cats', eval_matrix, plot = TRUE)
expectation_match(m_obj, eval_matrix)
sum(m_obj * eval_matrix)

cat_verb_similarity <- mapTextSimilarityNorm(cats, verb_norms)
text_sumularity_heatmap(
  cat_verb_similarity,
  labels_row = texts,
  labels_col = verbs,
  main = 'Similarity of “cat” embeddings with verb lexeme embeddings from corresponding sentences'
)
# microbenchmark::microbenchmark(
#   purrr = purrr::map(
#     verb_norms$texts,
#     \(norm) textSimilarityNorm(docs$texts$texts, norm),.progress = TRUE
#   ) |> purrr::reduce(cbind),
#   vapply = vapply(
#     verb_norms$texts,
#     \(norm) textSimilarityNorm(docs$texts$texts, norm),
#     numeric(length(verbs)),
#     USE.NAMES = TRUE
#   ),
#   times = 1e3,
#   check = 'equivalent'
# )

(verbs_ <- purrr::map_dfr(docs$tokens$texts, \(x) x[3,]))
(m_pred <- textSimilarityMatrix(verbs_))
text_sumularity_heatmap(m_pred, labels_row = verbs)
verb_verb_similarity <- mapTextSimilarityNorm(verbs_, verb_norms)
text_sumularity_heatmap(
  verb_verb_similarity,
  labels_row = texts,
  labels_col = verbs,
  main = 'Similarity of verb embeddings with corresponding verb lexeme embeddings'
)

(cls <- purrr::map_dfr(docs$tokens$texts, \(x) x[1,]))
(m_cls <- textSimilarityMatrix(cls))
text_sumularity_heatmap(m_cls, labels_row = texts, labels_col = texts)
cls_verb_similarity <- mapTextSimilarityNorm(cls, verb_norms)
text_sumularity_heatmap(
  cls_verb_similarity,
  labels_row = texts,
  labels_col = verbs,
  main = 'Similarity of classifier token embeddings with verb lexeme embeddings from corresponding sentences'
)

(i <- purrr::map_dfr(
  docs$tokens$texts, \(x) x[which(x$tokens %like% '.?i$'), ])
)
(m_i <- textSimilarityMatrix(i))
text_sumularity_heatmap(m_i, labels_row = texts, labels_col = texts)
i_verb_similarity <- mapTextSimilarityNorm(i, verb_norms)
text_sumularity_heatmap(
  i_verb_similarity,
  labels_row = texts,
  labels_col = verbs,
  main = 'Similarity of “I” token embeddings with verb lexeme embeddings from corresponding sentences'
)

textZeroShot(
  tolower(texts),
  model = model,
  candidate_labels = verbs,
  hypothesis_template = '{}'
)

textZeroShot(
  tolower(texts),
  model = model,
  candidate_labels = verbs,
  hypothesis_template = 'I {} cats'
)

(sentiment_labels <- c('good', 'bad'))
(sentiment_labels <- c('good', 'bad', 'ok'))
textZeroShot(
  tolower(texts),
  model = model,
  candidate_labels = sentiment_labels,
  hypothesis_template = 'cats are {}',
  tokenizer_parallelism = TRUE,
  logging_level = 'error'
)

hypotheses_good_bad <- textEmbed(
  tibble::tibble(good = 'cats are good', bad = 'cats are bad'),
  model = model,
  layers = -1,
  keep_token_embeddings = FALSE,
  tokenizer_parallelism = TRUE
)
(cls_good_bad_similarity <- mapTextSimilarityNorm(cls, hypotheses_good_bad))
text_sumularity_heatmap(
  cls_good_bad_similarity,
  labels_row = texts
)
(i_good_bad_similarity <- mapTextSimilarityNorm(i, hypotheses_good_bad))
text_sumularity_heatmap(
  i_good_bad_similarity,
  labels_row = texts
)
(cats_good_bad_similarity <- mapTextSimilarityNorm(cats, hypotheses_good_bad))
text_sumularity_heatmap(
  cats_good_bad_similarity,
  labels_row = texts
)

hypotheses_good_bad <- textEmbed(
  tibble::tibble(friend = 'friend', foe = 'foe'),
  model = model,
  layers = -1,
  keep_token_embeddings = FALSE,
  tokenizer_parallelism = TRUE
)

## Russian ----
model_translate <- 'Helsinki-NLP/opus-mt-en-ru'
# model_translate <- 'ktadzjibov/opus-mt-en-ru-finetuned-en-to-ru-amls' # lol
(texts_ru <- textTranslate(
  texts,
  source_lang = 'en',
  target_lang = 'ru',
  model = model_translate
) |> dplyr::pull())

model_ru <- 'cointegrated/rubert-base-cased-nli-threeway'
docs <- textEmbed(
  tolower(texts_ru),
  model = model_ru,
  layers = -1
)

(sentiment_labels <- c('хорошие', 'плохие'))
(sentiment_labels <- c('хорошие', 'плохие', 'норм'))
textZeroShot(
  tolower(texts_ru),
  model = model_ru,
  candidate_labels = sentiment_labels,
  hypothesis_template = 'кошки {}',
  tokenizer_parallelism = TRUE,
  logging_level = 'error'
)
