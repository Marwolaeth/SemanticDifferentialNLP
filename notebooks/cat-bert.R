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

#### Judgements ----
(judgement_data <- c(good = 1, bad = -1))

### The Texts ----
(texts <- paste(
  'I', verbs, 'cats'
))

## Expectation vectors ----
(eval_matrix <- as.matrix(verbs_data) %*% t(verbs_data))
isSymmetric(eval_matrix)

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
semantic_divergence(cls, eval_matrix, plot = TRUE, labels_col = verbs)

#### Concept Similarity ----
contextual_influence(
  cls, verb_norms, eval_matrix, plot = TRUE, labels_col = verbs
)

### Tokens: I ----
(i <- select_tokens(docs, '.?i$'))
#### Divergence ----
semantic_divergence(i, eval_matrix, plot = TRUE)

#### Concept Similarity ----
contextual_influence(i, verb_norms, eval_matrix, plot = TRUE)

# Repeat for the next model
## or better run a function to test them all!
