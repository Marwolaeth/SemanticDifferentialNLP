source('app/functions.R', encoding = 'UTF-8')

# Data ----
texts <- c(
  'Для истинных ценителей моды XCellent представляет собой идеальный выбор, который не поддается массовым трендам.',
  'Аналитики предполагают, что XCellent сделает шаг вперед, внедрив уникальные решения в свои устройства.',
  #   'Каждый раз, когда я ношу вещи от XCellent, получаю комплименты от тех, кто разбирается в моде.',
  'Благодаря новым разработкам XCellent, многие компании начинают пересматривать свои стратегии.'#,
  #   'XCellent стал символом утонченного вкуса, привлекающим только самых взыскательных покупателей.',
  #   'Среди лидеров отрасли, таких как Innovatech и Quantum Systems, XCellent наблюдает за их новыми разработками.',
  #   'Кто-нибудь еще помнит, когда XCellent был на слуху? Кажется, это было давно.'
)
template <- 'Xcellent - {}'
# model <- 'DeepPavlov/xlm-roberta-large-en-ru-mnli'
scaleset <- list(
  'Инновационность' = list(
    c('устаревший' = -1, 'сдержанный' = 0, 'инновационный'   = 1),
    c('отсталый'   = -1, 'стабильный' = 0, 'изобретательный' = 1)
  ),
  'Популярность' = list(
    c('немодный'      = -1, 'адекватный'    = 0, 'модный'     = 1),
    c('неактуальный'  = -1, 'специфический' = 0, 'молодежный' = 1),
    c('непопулярный'  = -1, 'известный'     = 0, 'популярный' = 1),
    c('малоизвестный' = -1, 'элитарный'     = 0, 'знаменитый' = 1)
  ),
  'Надежность' = list(
    c('ненадежный'     = -1, 'нормальный'  = 0, 'надежный'     = 1),
    c('некачественный' = -1, 'обычный'     = 0, 'качественный' = 1),
    c('хлипкий'        = -1, 'стандартный' = 0, 'прочный'      = 1)
  )
)
scale <- scaleset[[1]]
items <- scale
# model <- 'Marwolaeth/rosberta-nli-terra-v0'
# model <- 'ai-forever/ru-en-RoSBERTa'
model <- 'MoritzLaurer/ernie-m-base-mnli-xnli'
prefix <- TRUE
object <- c('XCellent', 'наша компания', 'Атлас+', '[BERT]')
select_token <- 'Y'
similarity_group_items <- TRUE

# Examples ----
## Object Masking ----
.object_regex(object)

text <- c(
  'XCellent это самый инновационный бренд на сегодняшнем рынке микропроцессоров.',
  'Наша компания постоянно совершенствует свои технологии.',
  'Xcellent работает для вас!',
  'Для нашей компании нет ничего невозможного! xcellent может всё!',
  '[BERT] + Xcellent = успех',
  'Мы представляем «Атлас+» в Европе.'
)
.replace_object(text, object, 'Y')

## Py Interface ----
mdl <- get_model(model)
str(mdl, 1)
mdl[[2]]

## Text Embeddings ----
(txts <- .replace_object(texts, 'Xcellent', 'Y'))
word_embeddings_layers <- text_embed_raw(txts, model)
variable_list_i <- 1

textEmbed(
  txts,
  model = model,
  remove_non_ascii = FALSE,
  keep_token_embeddings = FALSE,
  layers = get_number_of_hidden_layers(model)
)
text_embed(txts, model = model, aggregation_from_tokens_to_texts = 'mean')

bench_embeddings <- microbenchmark::microbenchmark(
  text = textEmbed(
    txts,
    model = model,
    remove_non_ascii = FALSE,
    keep_token_embeddings = FALSE,
    layers = get_number_of_hidden_layers(model)
  ),
  otus = text_embed(
    txts,
    model = model,
    aggregation_from_tokens_to_texts = 'mean'
  ),
  times = 30L
)
save(bench_embeddings, file = 'benchmarks/embedding-functions.RData')

tic()
e1 <- text_embed_raw(texts, model)
toc()

tic()
e2 <- textEmbedRawLayers(texts, model = model, layers = -1, return_tokens = T)
toc()

## Zero-Shot ----
res <- hgTransformerGetZeroShot(
  sequences = txts,
  candidate_labels = c('не инновационный', 'инновационный'),
  hypothesis_template = template,
  multi_label = FALSE,
  model = model,
  device = 'cpu',
  tokenizer_parallelism = FALSE, # To be checked!
  logging_level = 'error',
  force_return_results = FALSE,
  set_seed = 111L
)

res <- semdiff_zeroshot_map(
  txts,
  model = model,
  items = items,
  template = template,
  prefix = FALSE
)

## Norm Embeddings ----
(items <- scaleset[[1]])

norms <- .items_to_norms(
  items,
  model,
  prefix = TRUE
)
norms2 <- .items_to_norms(
  items,
  model,
  prefix = TRUE,
  group_items = TRUE
)

similarity_norm(bind_rows(norms$texts), norms)
similarity_norm(bind_rows(norms$texts), norms2)
similarity_norm(bind_rows(norms2$texts), norms2)

### CLS Pooling ----
text <- 'classification: Y – инновационный'
embed_cls <- text_embed(
  text,
  model,
  aggregation_from_tokens_to_texts = 'cls'
)
similarity_norm(embed_cls$texts$texts, norms) |> softmax()
similarity_norm(embed_cls$texts$texts, norms2) |> softmax()

### Mean Pooling ----
text <- 'Y – инновационный'
embed_mean <- text_embed(
  text,
  model,
  aggregation_from_tokens_to_texts = 'mean'
)
similarity_norm(embed_mean$texts$texts, norms) |> softmax()
similarity_norm(embed_mean$texts$texts, norms2) |> softmax()

### Token Pooling ----
text <- 'Y – инновационный'
embed_obj <- text_embed(
  text,
  model,
  aggregation_from_tokens_to_texts = 'token',
  select_token = 'Y',
  keep_token_embeddings = TRUE
)
(s <- similarity_norm(embed_obj$texts$texts, norms))
(s <- similarity_norm(embed_obj$texts$texts, norms2))
softmax(s)

## SoftMax ----
softmax <- function(x) exp(x) / sum(exp(x))
softmax_cmp <- compiler::cmpfun(softmax, options = list(optimize=3))

bench_softmax <- microbenchmark::microbenchmark(
  nocmp = softmax(s),
  yacmp = softmax_cmp(s),
  times = 10000L,
  check = 'equal'
)

## Benchmark Similarity ----
texts <- c(
  texts,
  'XCellent тупой и отсталый',
  'XCellent — самая инновационная компания в мире.',
  'Umbrella очень инновационная компания.',
  'Umbrella очень инновационная компания, а XCellent отстает в развитии.'
)

(txts <- .replace_object(texts, 'XCellent', 'Y'))
(template <- .replace_object(template, 'XCellent', 'Y'))

norms2 <- .items_to_norms(
  items,
  model,
  prefix = TRUE,
  group_items = TRUE
)
# norm_embeddings <- norms2
(mask <- rep(c(-1, 1), length(norms2$texts) / 2))

bs1 <- benchmark_similarity(
  norms2,
  model,
  template,
  prefix = TRUE,
  aggregation = 'token',
  select_token = 'Y'
)
bs1

embed_obj <- text_embed(
  paste('classification:', txts),
  model,
  aggregation_from_tokens_to_texts = 'token',
  select_token = 'Y',
  keep_token_embeddings = TRUE
)

(s1 <- similarity_norm(embed_obj$texts$texts, norms2, metric = 'spearman'))

### Fraction ----
(score <- s1 / matrix(rep(bs1, length(txts)), byrow = TRUE, nrow = length(txts)))

(score %*% mask) / (length(norms2$texts) / 2)

### Gaussian ----
pnorm(s1[1,1], mean = bs1[1], sd = .1)
pnorm(s1[1,2], mean = bs1[1], sd = .1)
pnorm(s1[1,1], mean = bs1[2], sd = .1)
pnorm(s1[1,2], mean = bs1[2], sd = .1)

### Hypothesis Similarity ----
(concepts <- names(norms2$texts))

hypotheses <- sapply(
  concepts,
  function(concept) {
    paste(
      'classification:',
      stringr::str_replace(template, fixed('{}'), fixed(concept))
    )
  }
) |>
  as.list() |>
  tibble::as_tibble()
hypotheses
hypotheses_embeddings <- text_embed(
  texts = hypotheses,
  model = model,
  aggregation_from_tokens_to_texts = 'cls'
)
embed_cls <- text_embed(
  paste('classification:', txts),
  model,
  aggregation_from_tokens_to_texts = 'cls'
)
(s1 <- similarity_norm(
  embed_cls$texts$texts, hypotheses_embeddings, metric = 'spearman')
)

tic()
norms3 <- .items_to_norms(
  items,
  model,
  prefix = TRUE,
  group_items = TRUE,
  as_phrases = TRUE,
  template = 'Кажется, что Y {}'
)
toc()

similarity_norm(embed_cls$texts$texts, norms3, metric = 'spearman') |>
  apply(1, softmax)

similarity_norm(embed_obj$texts$texts, norms3, metric = 'spearman') |> softmax()

txts <- examples$sentence |>
  .replace_object('Xcellent', 'Y')

tic()
embed_cls <- text_embed(
  paste('classification:', txts),
  model,
  aggregation_from_tokens_to_texts = 'cls'
)
toc()
save(embed_cls, file = 'data/example-embeddings-cls.RData')

(s <- similarity_norm(embed_cls$texts$texts, norms3, metric = 'spearman'))
examples$similarity_backward <- s[, 1]
examples$similarity_innovative <- s[, 2]

examples |>
  filter(feature == 'Инновационность') |>
  group_by(rating) |>
  summarise(across(starts_with('similarity'), c(mean = mean, sd = sd)))

df <- examples |>
  mutate(score = similarity_innovative - similarity_backward)

df |>
  filter(feature == 'Инновационность') |>
  group_by(rating) |>
  summarise(
    mean = mean(score), sd = sd(score), min = min(score), max = max(score)
  )

x <- df |>
  filter(feature != 'Инновационность' | rating < 1) |>
  pull(score)

hist(x)
shapiro.test(x) # Not Normal

hist(scale(x))
shapiro.test(scale(x))

df <- df |>
  mutate(
    score_scale = (score - mean(x)) / sd(x),
    score_p = pnorm(score_scale, sd = .07) - .5
  )

df |>
  filter(feature == 'Инновационность') |>
  group_by(rating) |>
  summarise(
    mean = mean(score_scale),
    sd = sd(score_scale),
    min = min(score_scale),
    max = max(score_scale)
  )

df |>
  filter(feature == 'Инновационность') |>
  group_by(rating) |>
  summarise(
    mean = mean(score_p),
    sd = sd(score_p),
    min = min(score_p),
    max = max(score_p)
  )

x_stats <- replicate(
  10000,
  {
    y <- sample(x, length(x), replace = TRUE)
    tibble(
      lower = quantile(y, .025),
      mean = mean(y),
      median = median(y),
      upper = quantile(y, .975)
    )
  },
  simplify = FALSE
)
str(x_stats)
x_stats |>
  bind_rows() |>
  summarise(
    across(
      everything(),
      c(mean = mean, median = median)
    )
  )

df <- examples |>
  dplyr::select(rating, starts_with('similarity')) |>
  pivot_longer(
    starts_with('similarity'), names_to = 'item', values_to = 'score'
  ) |>
  mutate(item = str_remove(item, fixed('similarity_')))

library(ggplot2)
df |>
  ggplot(aes(x = score, fill = item)) +
  geom_density() +
  facet_grid(rows = vars(rating)) +
  theme_minimal()

df |>
  ggplot(aes(x = score)) +
  geom_density() +
  facet_grid(rows = vars(rating)) +
  theme_minimal()

x <- df |>
  filter(rating < 1) |>
  pull(score)

hist(x)
shapiro.test(x)

library(fitdistrplus)

fit <- fitdistrplus::fitdist(x, 'norm')
fit
fit$estimate
save(fit, file = 'data/fit-norm-cls-spearman.RData')

scale_score <- function(x) {
  # (x - fit$estimate['mean']) / fit$estimate['sd']
  (x - .6) / 0.04
}

examples$score_backward_norm <- NULL
examples$score_innovative_norm <- NULL
examples$score_backward <- scale_score(examples$similarity_backward)
examples$score_innovative <- scale_score(examples$similarity_innovative)
examples$score <- examples$score_innovative - examples$score_backward
examples$score_p <- pnorm(examples$score) - .5

examples |>
  filter(feature == 'Инновационность') |>
  group_by(rating) |>
  summarise(
    across(c(score, score_p), c(mean = mean, sd = sd, max = max, min = min))
  )

examples |>
  filter(feature == 'Инновационность') |>
  ggplot(aes(x = score_p, fill = factor(rating))) +
  geom_density() +
  theme_minimal()

x <- examples |>
  filter(rating > 0) |>
  pull(similarity_backward)

y <- examples |>
  filter(rating < 1) |>
  pull(similarity_innovative)

hist(x)
shapiro.test(x)
hist(y)
shapiro.test(y)

library(fitdistrplus)

fit_negative <- fitdistrplus::fitdist(x, 'norm')
fit_negative
fit_positive <- fitdistrplus::fitdist(y, 'norm')
fit_positive
save(fit_positive, fit_negative, file = 'data/fit-norm-cls-spearman-all.RData')

## Scale Norms ----
### CLS ----
tic()
norm_embeddings <- .items_to_norms(
  items,
  model,
  as_phrases = prefix,
  template = template,
  prefix = prefix,
  group_items = similarity_group_items,
  aggregation = 'cls'
)
toc()

(txt <- paste(txts[2], txts[3]))
(txt <- 'Y абсолютно отсталая контора. Она совсем не развивается.')
(sents <- sentences(txt))

if (prefix) sents <- paste('classification:', sents)

tic()
text_embeddings <- text_embed(
  sents,
  model,
  aggregation_from_tokens_to_texts = 'cls'
)
toc()

textEmbed(
  sents, model = model, remove_non_ascii = FALSE, layers = -1,
  max_token_to_sentence = 20 
)
textEmbed('Ха-ха', model = model, remove_non_ascii = FALSE, layers = -1)

(n_items <- length(norm_embeddings$texts))
(n_comparisons <- n_items / 2)

(mask <- rep(c(-1, 1), n_comparisons))

(s <- similarity_norm(text_embeddings$texts$texts, norm_embeddings))

(s <- apply(s, 2, max))

((s %*% mask) / n_comparisons) * 6.6

### Token ----
tic()
norm_embeddings <- .items_to_norms(
  items,
  model,
  as_phrases = FALSE,
  template = template,
  prefix = FALSE,
  group_items = similarity_group_items,
  aggregation = 'mean'
)
toc()

(txt <- paste(txts[2], txts[3]))
(txt <- 'Y абсолютно отсталая контора. Она совсем не развивается.')
(sents <- sentences(txt))

if (prefix) sents <- paste('classification:', sents)

tic()
text_embeddings <- text_embed(
  sents,
  model,
  aggregation_from_tokens_to_texts = 'token',
  select_token = select_token
)
toc()

(n_items <- length(norm_embeddings$texts))
(n_comparisons <- n_items / 2)

(mask <- rep(c(-1, 1), n_comparisons))

(s <- similarity_norm(text_embeddings$texts$texts, norm_embeddings))

(s <- apply(s, 2, max))

score <- ((s %*% mask) / n_comparisons)[1]

tibble::tibble(
  items = paste(names(norm_embeddings$texts), collapse = ' – '),
  .score = score
)

tic()
scaleset_norms <- purrr::map2(
  scaleset,
  seq_along(scaleset),
  function(semantic_scale, i) {
    scale_result <- .items_to_norms(
      items = semantic_scale,
      model = model,
      as_phrases = TRUE,
      template = template,
      prefix = prefix,
      group_items = similarity_group_items,
      aggregation = 'cls',
      device = 'cpu'
    )
    return(scale_result)
  }
) |>
  purrr::set_names(names(scaleset))
toc()

tic()
purrr::map2(
  scaleset,
  seq_along(scaleset),
  function(semantic_scale, i) {
      semdiff_similarity(
        sentences = sentences(txt),
        model = model,
        norm_embeddings = scaleset_norms[[i]],
        prefix = prefix,
        aggregation = 'cls',
        select_token = universal_brand_name,
        similarity_metric = 'cosine',
        device = 'cpu'
      )
    }
) |>
  purrr::set_names(names(scaleset))
toc()

## Perplexity ----
txt <- 'Y продолжает задавать тренды, предлагая стильные решения, которые привлекают внимание молодежи.'

labels <- c('Это инновационная компания.', 'Это консервативная компания.')


