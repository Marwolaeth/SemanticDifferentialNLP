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
library(yardstick)

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
ollamar::pull('llama3.2')
llm_use('ollama', 'llama3.2', .cache = '')

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
  'Xcellent seems to be an innovative company',
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
pred_metrics <- metric_set(accuracy, bal_accuracy, precision, recall)
pred_metrics

#### Llama ----
tic()
llm_vec_custom(
  sents,
  prompt = prompt_inno
)
toc()

df <- readxl::read_excel(
  'data/xcellent-sentences.xlsx'
) |>
  dplyr::filter(feature == 'Инновационность')

prompt_inno_binary <- paste(
  'Xcellent seems to be an innovative and pioneering company.',
  'Focus precisely on this aspect, not other like success,',
  'popularity or overall sentiment.'
  # 'that invents something new and surpasses all its rivals in innovations.',
  # 'For example, it develops new products,',
  # 'not merely watching his rivals inventing.',
  # 'Mind the irrelevant mentions, like former employees etc.'
)
# prompt_inno_binary <- paste(
#   'Xcellent создает впечатление передовой и инновационной компании,',
#   'которая не отстает от конкурентов в разработке нового.',
#   'Не считаются случайные упоминания, вроде бывших сотрудников или отдельных',
#   'изделий, перечисления и эпизодические упоминания.'
# )

tic()
df <- df |> 
  llm_verify(
    sentence,
    what = prompt_inno_binary,
    pred_name = 'innovative_hat',
    # additional_prompt = 'Please be restrained.'
  ) |>
  dplyr::mutate(
    innovative = factor(as.numeric(rating > 0))
  )
toc()
summary(df)
dplyr::count(df, rating, innovative_hat)
table(df$rating, df$innovative_hat)
table(df$innovative, df$innovative_hat)

df |>
  # tidyr::replace_na(list(innovative_hat = '0')) |>
  na.omit() |>
  pred_metrics(
    truth = innovative,
    estimate = innovative_hat,
    na_rm = TRUE
  )
  

#### BERT ----
# Llama                                                      # 0.726
model_name <- 'joeddav/xlm-roberta-large-xnli'               # 0.734
model_name <- 'sileod/mdeberta-v3-base-tasksource-nli'       # 0.551
model_name <- 'DeepPavlov/xlm-roberta-large-en-ru-mnli'      # 0.645
model_name <- 'Marwolaeth/rubert-tiny-nli-terra-v0'          # 0.514
model_name <- 'Marwolaeth/rosberta-nli-terra-v0'             # 0.536
model_name <- 'MoritzLaurer/ernie-m-base-mnli-xnli'          # 0.545
model_name <- 'MoritzLaurer/ernie-m-large-mnli-xnli'         # 0.631
model_name <- 'mjwong/mcontriever-msmarco-xnli'              # 0.569
model_name <- 'cointegrated/rubert-base-cased-nli-threeway'  # 0.617
model_name <- 'cointegrated/rubert-base-cased-nli-twoway'    # 0.707
model_name <- 'cointegrated/rubert-tiny-bilingual-nli'       # 0.00

(template <- glue::glue('{brand} – {{}}'))

tic()
bert_results <- textZeroShot(
  df$sentence,
  model = model_name,
  c('отсталый', 'стабильный', 'инновационный'),
  hypothesis_template = template,
  tokenizer_parallelism = TRUE
)
toc()
bert_results

df <- df |>
  dplyr::mutate(
    innovative_hat_bert = factor(
      as.numeric(bert_results$labels_x_1 == 'инновационный')
    )
  )

table(df$innovative, df$innovative_hat_bert)

pred_metrics(
  df,
  truth = innovative,
  estimate = innovative_hat_bert,
  na_rm = TRUE
)
