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
model <- 'Marwolaeth/rosberta-nli-terra-v0'
# model <- 'ai-forever/ru-en-RoSBERTa'
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
txts <- .replace_object(texts, 'Xcellent', 'Y')
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
  'XCellent — самая инновационная компания в мире.'
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
  aggregation_from_tokens_to_texts = 'cls',
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

## Scale Norms ----
tic()
scaleset_norms <- purrr::map(
  scaleset,
  .items_to_norms,
  model = model,
  prefix = prefix,
  aggregation = if (prefix) 'cls' else 'mean'
)
toc()
str(scaleset_norms, 2)

