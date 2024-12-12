library(text)

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
model <- 'cross-encoder/qnli-electra-base' # Poor
model <- 'Marwolaeth/rosberta-nli-terra-v0'

source('notebooks/cat-bert-functions.R')

# The Data ----
## The Concepts ----
### Polarity encoding ----
#### Attitudes ----
(verbs_data <- c(
  'love'            =  1,
  'hate'            = -1,
  'like'            =  1,
  "don't like"      = -1,
  'sympathize with' =  1,
  'despise'         = -1,
  'adore'           =  1,
  'dislike'         = -1
))
(verbs <- names(verbs_data))
(verbs_tbl <- tibble::as_tibble(as.list(verbs) |> setNames(verbs)))

#### Judgements ----
(judgement_data <- c(good = 1, bad = -1))
(judgements <- names(judgement_data))
(judgements_tbl <- tibble::as_tibble(
  as.list(judgements) |> setNames(judgements))
)

### The Texts ----
(texts <- paste(
  'I', verbs, 'cats'
))

## Expectation vectors ----
(polarity_matrix <- as.matrix(verbs_data) %*% t(verbs_data))
isSymmetric(polarity_matrix)

(bipolarity_matrix <- as.matrix(verbs_data) %*% t(judgement_data))

identity_matrix <- matrix(-1, nrow = length(verbs), ncol = length(verbs))
diag(identity_matrix) <- 1
dimnames(identity_matrix) <- dimnames(polarity_matrix)
isSymmetric(identity_matrix)
identity_matrix
sum(identity_matrix * identity_matrix)

# The Experiment ----
## Embeddings ----
### Many Models (benchmark) ----
#### English ----
load(file.path('data', 'models', 'english.RData'))
models <- models_english |>
  dplyr::filter(ok)

model_data <- expand.grid(
  model = models$model,
  layers = list(-1, -2:-1, -2, -3)
)

embquality_test <- purrr:::pmap(
  model_data,
  purrr::safely(test_embeddings),
  similarity_metrics = c('cosine', 'spearman'),
  corpus = texts,
  expected_inner_similarities = list(polarity = polarity_matrix),
  concepts = verbs,
  expected_outer_similarities = list(
    polarity = polarity_matrix,
    identity = identity_matrix
  ),
  tokens = list(1L, '.?cat.?', '.?I')
)
embquality_test
purrr::map(embquality_test, 'error')
model_data[213:214,]

embquality <- embquality_test |>
  purrr::map('result') |>
  dplyr::bind_rows() |>
  tidyr::unnest(results)

save(
  embquality_test, embquality,
  file = file.path('benchmarks', 'embedding-quality-en.RData')
)

embquality |>
  dplyr::mutate(
    token = forcats::fct_explicit_na(token),
    layers = forcats::fct(layers)
  ) |>
  dplyr::select(-model, -token) |>
  na.omit() |>
  # summary()
  correlationfunnel::binarize(one_hot = TRUE, thresh_infreq = .5, n_bins = 6) |>
  correlationfunnel::correlate()

### Illustrations ----
model <- 'sentence-transformers/all-roberta-large-v1'
layers <- -1
metric <- 'cosine'
docs <- textEmbed(
  texts,
  model = model,
  layers = layers,
  aggregation_from_layers_to_tokens = 'concatenate',
  keep_token_embeddings = TRUE,
  tokenizer_parallelism = TRUE,
  remove_non_ascii = FALSE
)
verb_norms <- textEmbed(
  verbs_tbl,
  model = model,
  layers = layers,
  keep_token_embeddings = FALSE,
  tokenizer_parallelism = TRUE,
  trust_remote_code = TRUE
)
judgement_norms <- textEmbed(
  judgements_tbl,
  model = model,
  layers = layers,
  keep_token_embeddings = FALSE,
  tokenizer_parallelism = TRUE,
  trust_remote_code = TRUE
)
semantic_divergence(
  docs$texts$texts,
  polarity_matrix,
  metric = metric,
  plot = TRUE,
  labels_col = verbs
)
contextual_influence(
  docs$texts$texts,
  verb_norms,
  polarity_matrix,
  metric = metric,
  plot = TRUE
)
contextual_influence(
  docs$texts$texts,
  verb_norms,
  identity_matrix,
  metric = metric,
  plot = TRUE
)
contextual_influence(
  docs$texts$texts,
  judgement_norms,
  bipolarity_matrix,
  metric = metric,
  plot = TRUE
)

## Zero-shot ----
model <- 'ynie/electra-large-discriminator-snli_mnli_fever_anli_R1_R2_R3-nli'

### Hypotheses Sets ----
#### Identity + Polarity ----
(hypothesis_template_identity <- 'I {} cats')
(hypotheses_identity <- verbs)

#### BiPolarity ----
(hypothesis_template_bipolarity <- 'Cats are {}')
(hypotheses_bipolarity <- judgements)

### Results ----
#### Identity ----
res <- textZeroShot(
  texts,
  model = model,
  candidate_labels = hypotheses_identity,
  # hypothesis_template = hypothesis_template_identity,
  hypothesis_template = '{}',
  multi_label = FALSE,
  tokenizer_parallelism = TRUE
)

res_wide <- purrr::map(
  seq_along(hypotheses_identity),
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
  dplyr::select(sequence, dplyr::all_of(hypotheses_identity))

(res_matrix <- res_wide |>
    tibble::column_to_rownames('sequence') |>
    as.matrix())

text_sumularity_heatmap(res_matrix)
sum(res_matrix * identity_matrix)

#### Polarity ----
res <- textZeroShot(
  texts,
  model = model,
  candidate_labels = hypotheses_identity,
  hypothesis_template = hypothesis_template_identity,
  multi_label = TRUE,
  tokenizer_parallelism = TRUE
)

res_wide <- purrr::map(
  seq_along(hypotheses_identity),
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
  dplyr::select(sequence, dplyr::all_of(hypotheses_identity))

(res_matrix <- res_wide |>
  tibble::column_to_rownames('sequence') |>
  as.matrix())

text_sumularity_heatmap(res_matrix)
sum(res_matrix * polarity_matrix)

#### BiPolarity ----
res <- textZeroShot(
  texts,
  model = model,
  candidate_labels = hypotheses_bipolarity,
  hypothesis_template = hypothesis_template_bipolarity,
  multi_label = FALSE,
  tokenizer_parallelism = TRUE
)

res_wide <- purrr::map(
  seq_along(hypotheses_bipolarity),
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
  dplyr::select(sequence, dplyr::all_of(hypotheses_bipolarity))

(res_matrix <- res_wide |>
    tibble::column_to_rownames('sequence') |>
    as.matrix())

sum(bipolarity_matrix * (bipolarity_matrix > 0))
text_sumularity_heatmap(res_matrix)
sum(res_matrix * bipolarity_matrix)

argmax(res_matrix) == argmax(bipolarity_matrix)

zeroShotTest <- setClass(
  'zeroShotTest',
  contains = 'list',
  slots = c(
    num_examples='integer',
    classes='character',
    template='character',
    contrasts='matrix'
  ),
  prototype = list(template='{}')
)
zeroShotTest

new(
  'zeroShotTest',
  num_examples = length(texts),
  classes = judgements,
  template = 'Cats are {}',
  contrasts = bipolarity_matrix
)
