source('app/functions.R', encoding = 'UTF-8')

# Data ----
texts <- c(
  'Для истинных ценителей моды XCellent представляет собой идеальный выбор, который не поддается массовым трендам.',
  'Аналитики предполагают, что XCellent сделает шаг вперед, внедрив уникальные решения в свои устройства.'#,
  #   'Каждый раз, когда я ношу вещи от XCellent, получаю комплименты от тех, кто разбирается в моде.',
  #   'Благодаря новым разработкам XCellent, многие компании начинают пересматривать свои стратегии.',
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
prefix <- TRUE
object <- c('XCellent', 'наша компания')
select_token <- 'Y'

# Examples ----
## Object Masking ----
text <- c(
  'XCellent это самый инновационный бренд на сегодняшнем рынке микропроцессоров.',
  'Наша компания постоянно совершенствует свои технологии.',
  'Xcellent работает для вас!',
  'Для нашей компании нет ничего невозможного! xcellent может всё!'
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

bench <- microbenchmark::microbenchmark(
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

tic()
e1 <- text_embed_raw(texts, model)
toc()

tic()
e2 <- textEmbedRawLayers(texts, model = model, layers = -1, return_tokens = T)
toc()