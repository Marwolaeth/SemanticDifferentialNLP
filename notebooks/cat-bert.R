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
model <- 'nvidia/NV-Embed-v2'
model <- 'cross-encoder/qnli-electra-base' # Poor
model <- 'cross-encoder/mmarco-mMiniLMv2-L12-H384-v1' # Awful
model <- 'MendelAI/nv-embed-v2-ontada-twab-peft'
model <- 'facebook/bart-large-mnli'
model <- 'isolation-forest/setfit-absa-polarity' # Pretty
# model <- 'sentence-transrormers/paraphrase-MiniLM-L6-v2'
# model <- 'sentence-transformers/roberta-large-nli-stsb-mean-tokens'

source('cat-bert-functions.R')

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

#### Judgements ----
(judgement_data <- c(good = 1, bad = -1))

### The Texts ----
(texts <- paste(
  'I', verbs, 'cats'
))

## Expectation vectors ----
(polarity_matrix <- as.matrix(verbs_data) %*% t(verbs_data))
isSymmetric(polarity_matrix)

identity_matrix <- diag(length(verbs))
dimnames(identity_matrix) <- dimnames(polarity_matrix)
isSymmetric(identity_matrix)
identity_matrix

## The Documents ----
docs <- textEmbed(
  texts,
  # tolower(texts),
  model = model,
  layers = -1,
  # layers = 11:12,
  aggregation_from_layers_to_tokens = 'concatenate',
  keep_token_embeddings = TRUE,
  tokenizer_parallelism = TRUE,
  remove_non_ascii = FALSE
)

# The Experiment ----
## Embeddings ----
### One Model (visualisation) ----
semantic_divergence(
  docs$texts$texts, polarity_matrix, plot = TRUE, labels_col = verbs
)
contextual_influence(docs$texts$texts, verb_norms, polarity_matrix, plot = TRUE)

### Many Models (benchmark) ----
model_data <- expand.grid(
  model = list(
    # 'cross-encoder/mmarco-mMiniLMv2-L12-H384-v1',
    # 'facebook/bart-large-mnli',
    'isolation-forest/setfit-absa-polarity',
    'DeepPavlov/bert-base-cased-conversational'
  ),
  layers = list(-1, -2:-1, -2),
  similarity_metrics = c('cosine', 'spearman', 'euclidean')
)

test <- purrr:::pmap_dfr(
  model_data,
  test_embeddings,
  corpus = texts,
  expected_inner_similarities = list(polarity = polarity_matrix),
  concepts = verbs,
  expected_outer_similarities = list(
    polarity = polarity_matrix,
    identity = identity_matrix
  ),
  tokens = list(1L, '.?cat.?', '.?I')
)
test

test2 <- test |>
  tidyr::unnest(results)

## Zero-shot ----
