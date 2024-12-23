# devtools::install_github('mlverse/mall', build = TRUE)
if (!require(mall)) {
  install.packages('mall', type = 'source', INSTALL_opts = '--byte-compile')
  library(mall)
}
if (!require(tictoc)) {
  install.packages('tictoc', type = 'source', INSTALL_opts = '--byte-compile')
  library(tictoc)
}
library(text)

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
tic()
df |>
  llm_sentiment(text) |>
  llm_extract(text, 'verb in infinitive form') |>
  llm_extract(text, 'object of sentiment', pred_name = 'object') |>
  llm_classify(text, c('good', 'bad')) |>
  llm_verify(text, 'positive sentiment towards cats', yes_no = c(TRUE,FALSE))
toc()

## Russian ----
### An Example ----
sents <- c(
  'Инженеры Xcellent работают над созданием микропроцессоров, которые могут изменить представление о вычислительных мощностях.',
  'В то время как Xcellent сохраняет свои позиции, другие игроки, такие как BrightFuture, активно внедряют новшества.',
  'Среди последних трендов в технологиях стоит отметить, как Xcellent внедряет инновационные решения в своем производстве.'
)

tic()
llm_vec_verify(
  sents,
  'Xcellent gives the empression of an innovative and technological brand based on the text provided',
  # preview = TRUE
)
toc()

### The Prompt ----
brand <- 'Xcellent'
scale_name <- 'Отсталый – Инновационный'
scale_properties <- c(
  'кажется отсталым, устаревшим и совсем не развивающимся',
  'прозводит впечатление инновационного, передового, современного, задающего технологические тренды'
)

prompt_inno <- glue::glue(
  'Ты – полезная и внимательная машина для контент-анализа текста.\n',
  'Мы с тобой занимаемся анализом восприятия и имиджа брендов ',
  'на основе русскоязычных текстов новостей и социальных медиа.\n',
  'Оцени семантический дифференциал бренда «{brand}» по шкале «{scale_name}». ',
  'На основе приведенного ниже текста поставь оценку от 1 до 5: ',
  '1 если бренд {scale_properties[1]}, ',
  '5 если бренд {scale_properties[2]}; ',
  'или другую оценку, если имидж трудно оценить однозначно или ',
  'в тексте не содержится нужной информации.\n',
  'Никаких объяснений. Никаких букв. Никакой пунктуации. ',
  'Только число от 1 до 5.\n',
  'Поставь оценку исходя из следующего текста:\n',
  .sep = ''
)
prompt_inno

### The Analysis ----
#### Llama ----
tic()
llm_vec_custom(
  sents,
  prompt = prompt_inno
)
toc()

#### BERT ----
model_name <- 'joeddav/xlm-roberta-large-xnli'
model_name <- 'sileod/mdeberta-v3-base-tasksource-nli'
model_name <- 'DeepPavlov/xlm-roberta-large-en-ru-mnli'
template <- glue::glue('{brand} – {{}}')

textZeroShot(
  sents,
  model = model_name,
  c('отсталый', 'инновационный'),
  hypothesis_template = template,
  tokenizer_parallelism = TRUE
)
