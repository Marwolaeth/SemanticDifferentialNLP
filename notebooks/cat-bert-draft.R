source('notebooks/cat-bert-functions.R')

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
model <- 'mjwong/mcontriever-msmarco-xnli'
model <- 'DeepPavlov/distilrubert-small-cased-conversational'
model <- 'fyaronskiy/ruRoberta-large-ru-go-emotions'

### The Verbs ----
#### Polarity encoding ----
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

#### Word norms ----
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

# textEmbedRawLayers(
#   tolower(texts),
#   model = model,
#   layers = -1,
#   tokenizer_parallelism = TRUE,
#   word_type_embeddings = TRUE
# )
### Create Documents ----
docs <- textEmbed(
  texts,
  model = model,
  layers = -1,
  # layers = 11:12,
  aggregation_from_layers_to_tokens = 'concatenate',
  keep_token_embeddings = TRUE,
  tokenizer_parallelism = TRUE,
  remove_non_ascii = FALSE
)
# str(docs, 1)
# docs$word_types
# docs$tokens
# comment(docs$word_types)

### Performance Benchmarks ----
a <- mapTextSimilarityNorm(docs$texts$texts, verb_norms)
b <- norm_cosine_similarity(docs$texts$texts, verb_norms)
a-b
all.equal(a, b)
sum((a-b)^2)
bench <- microbenchmark::microbenchmark(
  vapply = mapTextSimilarityNorm(docs$texts$texts, verb_norms),
  matrix = norm_cosine_similarity(docs$texts$texts, verb_norms),
  times = 100L
)
bench
save(bench, file = file.path('benchmarks', 'similarity.RData'))

mapTextSimilarityNorm(docs$texts$texts, verb_norms, method = 'spearman')

### Compare ----
#### Documents ----
(m_docs <- textSimilarityMatrix(docs$texts$texts))
text_sumularity_heatmap(eval_matrix)

##### Divergence ----
text_sumularity_heatmap(m_docs, labels_row = texts)
sum(m_docs * eval_matrix)
expectation_match(m_docs, eval_matrix)

semantic_divergence(docs$texts$texts, eval_matrix)

##### Concept Similarity ----
(love_similarity <- textSimilarityNorm(
  docs$texts$texts,
  verb_norms$texts$love
))
text_sumularity_heatmap(love_similarity, labels_row = texts)
(love_similarity * verbs_data) |> sum()
t(love_similarity) %*% verbs_data
expectation_match(love_similarity, verbs_data)

doc_verb_similarity <- mapTextSimilarityNorm(
  docs$texts$texts,
  verb_norms,
  method = 'euclidean'
)
verb_norms_test <- verb_norms
verb_norms_test$texts <- verb_norms_test$texts[1:6]
mapTextSimilarityNorm(docs$texts$texts, verb_norms_test)
text_sumularity_heatmap(
  doc_verb_similarity,
  labels_row = texts,
  labels_col = verbs,
  main = 'Similarity of sentence embeddings with verb lexeme embeddings'
)
expectation_match(doc_verb_similarity, eval_matrix)
contextual_influence(docs$texts$texts, verb_norms, eval_matrix, plot = TRUE)
contextual_influence(
  docs$texts$texts, verb_norms_test, eval_matrix[, 1:6], plot = TRUE
)

#### Tokens: Cats ----
(cats <- purrr::map_dfr(
  docs$tokens$texts, \(x) x[which(x$tokens %like% '.?cats'), ])
)
(cats <- select_tokens(docs, '.?cats?'))

##### Divergence ----
(m_obj <- textSimilarityMatrix(cats))
text_sumularity_heatmap(m_obj, labels_row = texts, labels_col = texts)
expectation_match(m_obj, eval_matrix)
sum(m_obj * eval_matrix)
semantic_divergence(cats, eval_matrix)

##### Concept Similarity ----
(cat_verb_similarity <- mapTextSimilarityNorm(cats, verb_norms))
text_sumularity_heatmap(
  cat_verb_similarity,
  labels_row = texts,
  labels_col = verbs,
  main = 'Similarity of “cat” embeddings with verb lexeme embeddings from corresponding sentences'
)
sum(cat_verb_similarity * eval_matrix)
contextual_influence(cats, verb_norms, eval_matrix, plot = TRUE)
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

# TBD: Token Unification
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

#### Tokens: Classifer Token ----
(cls <- purrr::map_dfr(docs$tokens$texts, \(x) x[1,]))
(cls <- select_tokens(docs, 1L))

##### Divergence ----
(m_cls <- textSimilarityMatrix(cls))
text_sumularity_heatmap(m_cls, labels_row = texts, labels_col = texts)
expectation_match(m_cls, eval_matrix)
semantic_divergence(cls, eval_matrix, plot = TRUE)

##### Concept Similarity ----
cls_verb_similarity <- mapTextSimilarityNorm(cls, verb_norms)
text_sumularity_heatmap(
  cls_verb_similarity,
  labels_row = texts,
  labels_col = verbs,
  main = 'Similarity of classifier token embeddings with verb lexeme embeddings from corresponding sentences'
)
sum(cls_verb_similarity * eval_matrix)
contextual_influence(cls, verb_norms, eval_matrix)

#### Tokens: I ----
(i <- purrr::map_dfr(
  docs$tokens$texts, \(x) x[which(x$tokens %like% '.?i$'), ])
)
(i <- select_tokens(docs, '.?i$'))
##### Divergence ----
(m_i <- textSimilarityMatrix(i))
text_sumularity_heatmap(m_i, labels_row = texts, labels_col = texts)
semantic_divergence(i, eval_matrix)

##### Concept Similarity ----
i_verb_similarity <- mapTextSimilarityNorm(i, verb_norms)
text_sumularity_heatmap(
  i_verb_similarity,
  labels_row = texts,
  labels_col = verbs,
  main = 'Similarity of “I” token embeddings with verb lexeme embeddings from corresponding sentences'
)
contextual_influence(i, verb_norms, eval_matrix, plot = TRUE)

### Zero-shot ----
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
model_ru <- 'Marwolaeth/rubert-tiny-nli-terra-v0'
docs <- textEmbed(
  enc2utf8(texts_ru),
  model = model_ru,
  layers = -1,
  remove_non_ascii = FALSE
)

(sentiment_labels <- c('хорошие', 'плохие'))
(sentiment_labels <- c('хорошие', 'плохие', 'норм'))
textZeroShot(
  texts_ru,
  model = model_ru,
  candidate_labels = sentiment_labels,
  hypothesis_template = 'кошки {}',
  tokenizer_parallelism = TRUE,
  logging_level = 'error'
)

### Коварный вопрос ---
texts <- c(
  'Для наших врагов хорошо, что это здание синее',
  'Для наших врагов плохо, что это здание синее',
  'Наши враги в восторге, что это здание синее',
  'Для наших врагов не хорошо, что это здание синее',
  'Нам повезло, что это здание синее'
)
textZeroShot(
  texts,
  model = model_ru,
  candidate_labels = c('Хорошо', 'Плохо'),
  hypothesis_template = '{}, что это здание синее',
  tokenizer_parallelism = TRUE,
  logging_level = 'error'
)

textZeroShot(
  texts,
  model = model_ru,
  candidate_labels = c('хорошо', 'плохо'),
  hypothesis_template = 'Для нас {}, что это здание синее',
  tokenizer_parallelism = TRUE,
  logging_level = 'error'
)

textZeroShot(
  'Для наших конкурентов не хорошо, что наша компания расширяется',
  model = model_ru,
  candidate_labels = c('хорошо', 'плохо'),
  hypothesis_template = 'Для нас {}, что наша компания расширяется',
  tokenizer_parallelism = TRUE,
  logging_level = 'error'
)
