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

## The Model ----
ollamar::pull('llama3.2')

system_prompt <- ollamar::create_message(
  role = 'system',
  content = paste(
    'You are a skillful content analysis engine model tailored',
    'for brand analytics. Your task is to assess the image of a given brand',
    'based on the provided text.'
  )
)

messages <- ollamar::create_messages(
  system_prompt
)

# llm_use('ollama', 'llama3.2', seed = 111, messages = messages)
backend <- llm_use('ollama', 'llama3.2', seed = 111)

## An Experiment ----
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

(texts <- paste(
  'I', names(verbs_data), 'cats'
))

df <- tibble::tibble(
  text = texts,
  verb = names(verbs_data),
  polarity = verbs_data
)
df

tic()
df |>
  llm_sentiment(text) |>
  llm_extract(text, 'verb in infinitive form') |>
  llm_extract(text, 'object of sentiment', pred_name = 'object') |>
  llm_classify(text, c('good', 'bad')) |>
  llm_verify(text, 'positive sentiment towards cats', yes_no = c(TRUE,FALSE))
toc()

## The Data ----
df <- readxl::read_excel(
  'data/xcellent-sentences.xlsx'
) |>
  dplyr::filter(feature == 'Инновационность')

## The Metrics ----
pred_metrics <- metric_set(accuracy, bal_accuracy, precision, recall)
pred_metrics

## The Analysis ----
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

### Llama ----
#### Binary Classification ----
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

#### Custom Prompt ----
brand <- 'Xcellent'
# scale_name <- 'Отсталый – Инновационный'
# scale_properties <- c(
#   'кажется отсталым, устаревшим и совсем не развивающимся',
#   'прозводит впечатление инновационного, передового, современного, задающего технологические тренды'
# )

prompt_inno <- glue::glue(
  "Brand name: “{brand}”\n",
  "Please rate the brand's image on a semantic differential scale ",
  "from -100 to 100, where:\n",
  "- -100 means extremely stagnant or outdated\n",
  "- 0 means neutral perception or lack of information ",
  "related to innovativeness in the given text\n",
  "- 100 means extremely innovative and cutting-edge\n",
  'Focus precisely on innovativeness, not other aspects like success,',
  'popularity or overall sentiment.\n',
  'Return only the score, no explanation.\n\n',
  'Examples:\n',
  '"Инженеры Xcellent работают над созданием микропроцессоров, которые могут изменить представление о вычислительных мощностях." => 80\n',
  '"Среди последних трендов в технологиях стоит отметить, как Xcellent внедряет инновационные решения в своем производстве." => 92\n',
  '"В то время как Xcellent сохраняет свои позиции, другие игроки, такие как BrightFuture, активно внедряют новшества." => -20\n',
  '"На конференции присутствовали представители Xcellent и других компаний." => 0\n',
  '"На встрече выпускников многие вспоминали о времени, проведенном в Xcellent." => 0\n',
  '"Потерпевший заявил о пропаже брелка с ключами, кредитной карты, ноутбука Xcellent и зонта-трости. Подозреваемого задержали по горячим следам." => 0\n\n',
  '"В правительство области вошел Остап Постебайло, известный по работе в Xcellent и AlphaTech." => 0\n\n',
  "Please analyze the following text and provide a score:",
  .sep = ''
)
prompt_inno

prompts <- ollamar::create_messages(
  system_prompt,
  ollamar::create_message(
    role = 'user',
    content = prompt_inno
  )
)
inherits(prompts, 'list')
ollamar::validate_messages(prompts)

# x <- sents[[1]]
# list(
#   prompts[[1]],
#   purrr::map(prompts[[2]]$content, \(i) purrr::map(i, \(j) glue::glue(j, x = x)))
# )
# 
# backend <- llm_use(.silent = TRUE, .force = FALSE)
# 
# llm_vec_classify(x, c('outdated', 'stable', 'innovative'), preview = TRUE)

m_backend_submit.mall_ollama <- function(backend, x, prompt, preview = FALSE) {
  if (preview) {
    x <- head(x, 1)
    map_here <- purrr::map
  } else {
    map_here <- purrr::map_chr
  }
  map_here(
    x,
    \(x) {
      .args <- c(
        messages = purrr::map(
          prompt,
          function(p) {
            if (p[['role']] == 'user') {
              p[['content']] <- paste(p[['content']], x, sep = '\n')
            }
            p
          }
        ) |> list(),
        output = "text",
        mall:::m_defaults_args(backend)
      )
      res <- NULL
      if (preview) {
        res <- rlang::expr(ollamar::chat(!!!.args))
      }
      if (mall:::m_cache_use() && is.null(res)) {
        hash_args <- rlang::hash(.args)
        res <- mall:::m_cache_check(hash_args)
      }
      if (is.null(res)) {
        require(ollamar)
        res <- rlang::exec("chat", !!!.args)
        mall:::m_cache_record(.args, res, hash_args)
      }
      res
    }
  )
}

preview <- FALSE
tic()
resp <- m_backend_submit(
  backend = backend,
  x = df$sentence,
  prompt = prompts,
  preview = preview
)
toc()

df <- df |>
  dplyr::mutate(
    innovative = factor(as.numeric(rating > 0)),
    innovative_hat = as.numeric(resp)
  )

df |>
  dplyr::summarise(
    dplyr::across(
      innovative_hat,
      c(min = min, max = max, mean = mean, mediian = median),
      .names = '{.fn}'
    ),
    .by = rating
  )

df <- df |> 
  dplyr::slice(13) |>
  llm_custom(
    sentence,
    prompt = prompts,
    pred_name = 'innovative_hat'
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

### BERT ----
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
