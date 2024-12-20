# devtools::install_github('mlverse/mall', build = TRUE)
install.packages('mall', type = 'source', INSTALL_opts = '--byte-compile')

library(mall)

## DATA ----
### The Verbs ----
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

### The Texts ----
(texts <- paste(
  'I', names(verbs_data), 'cats'
))

### The Tibble ----
df <- tibble::tibble(
  text = texts,
  verb = names(verbs_data),
  polarity = verbs_data
)
df

## The Model ----
ollamar::pull("llama3.2")

## The Analysis ----
df |>
  llm_sentiment(text) |>
  llm_extract(text, 'verb') |>
  llm_classify(text, c('good', 'bad')) |>
  llm_verify(text, 'positive sentiment towards cats', yes_no = c(TRUE,FALSE))
