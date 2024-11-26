library(text)

## Utils ----
re.match <- function(s, pattern) {
  grepl(pattern, s, ignore.case = TRUE)
}
re.match <- compiler::cmpfun(re.match, options = list(optimize = 3))

match_arch <- function(model_name) {
  
  dplyr::case_when(
    re.match(model_name, 'DistillRoBERTa') ~ 'DistillRoBERTa',
    re.match(model_name, 'DistillBERT') ~ 'DistillBERT',
    re.match(model_name, 'Contriever') ~ 'Contriever',
    re.match(model_name, 'CamemBERT') ~ 'CamemBERT',
    re.match(model_name, 'DeBERTa') ~ 'DeBERTa',
    re.match(model_name, 'RoBERTa') ~ 'RoBERTa',
    re.match(model_name, 'ELECTRA') ~ 'ELECTRA',
    re.match(model_name, 'ALBERT') ~ 'ALBERT',
    re.match(model_name, 'MiniLM') ~ 'MiniLM',
    re.match(model_name, 'ERNIE') ~ 'ERNIE',
    re.match(model_name, 'XLNet') ~ 'XLNet',
    re.match(model_name, 'BERT') ~ 'BERT',
    re.match(model_name, 'BART') ~ 'BART',
    re.match(model_name, 'bge') ~ 'BAAI',
    re.match(model_name, 'nv\\-') ~ 'NV',
    re.match(model_name, 'T5') ~ 'T5',
    NA ~ '[other]',
    .default = '[other]'
  )
}
match_arch <- compiler::cmpfun(match_arch, options = list(optimize = 3))

match_type <- function(model_name) {
  re.match(
    model_name,
    '(cross\\-encoder)|(SBERT)|(sentence)|(rubert\\-tiny)|(MiniLM)|(sts)'
  )
}
match_type <- compiler::cmpfun(match_type, options = list(optimize = 3))

model_download_and_test <- function(
    model_name,
    lang = c('en', 'ru')
) {
  test_lang <- match.arg(lang, c('en', 'ru'), several.ok = FALSE)
  
  txt <- ifelse(lang == 'en', "It's OK", "Всё нормально") |> enc2utf8()
  
  textEmbed(
    tolower(txt),
    model = model_name,
    logging_level = 'info',
    remove_non_ascii = FALSE
  )
}

## Models list ----
models <- list(
  ### English ----
  en = list(
    #### NLI ----
    nli = tibble::tibble(
      model = c(
        'ynie/roberta-large-snli_mnli_fever_anli_R1_R2_R3-nli',
        'joeddav/xlm-roberta-large-xnli',
        'TFLai/Bert-Multilingual-NLI',
        'symanto/xlm-roberta-base-snli-mnli-anli-xnli',
        'ynie/electra-large-discriminator-snli_mnli_fever_anli_R1_R2_R3-nli',
        'Capreolus/electra-base-msmarco',
        'ChrisZeng/electra-large-discriminator-nli-efl-hateval',
        'ddobokki/electra-small-nli-sts',
        # 'manuu01/electra-nli_finetuned',
        'martn-nguyen/multi_nli-electra_small',
        'MoritzLaurer/mDeBERTa-v3-base-xnli-multilingual-nli-2mil7',
        'cointegrated/rubert-tiny-bilingual-nli',
        'MoritzLaurer/mDeBERTa-v3-base-mnli-xnli',
        'MoritzLaurer/ernie-m-base-mnli-xnli',
        'MoritzLaurer/ernie-m-large-mnli-xnli',
        'ynie/albert-xxlarge-v2-snli_mnli_fever_anli_R1_R2_R3-nli',
        'cross-encoder/nli-roberta-base',
        'cross-encoder/nli-deberta-base',
        'cross-encoder/nli-deberta-v3-base',
        'cross-encoder/nli-deberta-v3-large',
        'cross-encoder/qnli-electra-base',
        'cross-encoder/qnli-distilroberta-base',
        'cross-encoder/mmarco-mMiniLMv2-L12-H384-v1',
        'cross-encoder/nli-MiniLM2-L6-H768',
        'cross-encoder/nli-distilroberta-base',
        'cross-encoder/nli-deberta-v3-small',
        'cross-encoder/nli-deberta-v3-xsmall',
        'facebook/bart-large-mnli',
        'mjwong/mcontriever-msmarco-xnli'
      ),
      type = match_arch(model),
      sentence_level = match_type(model),
      task = c('NLI')
    ),
    #### Other ----
    base = tibble::tibble(
      model = c(
        'bert-base-uncased',
        'Akirami/distillbert-uncased-ag-news',
        'google/electra-small-discriminator',
        'google/electra-base-discriminator',
        'google/electra-large-discriminator',
        'FacebookAI/roberta-base',
        'FacebookAI/xlm-roberta-base',
        'FacebookAI/xlm-roberta-large',
        'BAAI/bge-base-en-v1.5',
        'BAAI/bge-small-en-v1.5',
        # 'nvidia/NV-Embed-v2',
        # 'MendelAI/nv-embed-v2-ontada-twab-peft',
        'xlnet/xlnet-base-cased',
        'SamLowe/roberta-base-go_emotions',
        'cardiffnlp/twitter-roberta-base-sentiment-latest',
        'cross-encoder/ms-marco-electra-base',
        'sentence-transformers/distiluse-base-multilingual-cased-v1',
        'sentence-transformers/paraphrase-MiniLM-L6-v2',
        'sentence-transformers/paraphrase-MiniLM-L12-v2',
        'sentence-transformers/all-MiniLM-L6-v2',
        'sentence-transformers/all-MiniLM-L12-v2',
        'sentence-transformers/xlm-r-distilroberta-base-paraphrase-v1',
        'sentence-transformers/msmarco-bert-base-dot-v5',
        'sentence-transformers/msmarco-distilbert-dot-v5',
        'sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2',
        'sentence-transformers/paraphrase-distilroberta-base-v2',
        'sentence-transformers/quora-distilbert-base',
        'sentence-transformers/quora-distilbert-multilingual',
        'sentence-transformers/nli-roberta-base-v2',
        'sentence-transformers/nli-distilroberta-base-v2',
        'sentence-transformers/msmarco-roberta-base-v3',
        'isolation-forest/setfit-absa-polarity'
      ),
      type = match_arch(model),
      sentence_level = match_type(model),
      task = c('LM')
    )
  ),
  ### Russian ----
  ru = list(
    #### NLI ----
    nli = tibble::tibble(
      model = c(
        'cointegrated/rubert-base-cased-nli-threeway',
        'cointegrated/rubert-base-cased-nli-twoway',
        'cointegrated/rubert-tiny-bilingual-nli',
        'taciturno/rubert-base-cased-finetuned-mnli',
        'joeddav/xlm-roberta-large-xnli',
        'symanto/xlm-roberta-base-snli-mnli-anli-xnli',
        'MoritzLaurer/ernie-m-base-mnli-xnli',
        'MoritzLaurer/ernie-m-large-mnli-xnli',
        'sileod/mdeberta-v3-base-tasksource-nli',
        'mjwong/mcontriever-msmarco-xnli'
      ),
      type = match_arch(model),
      sentence_level = match_type(model),
      task = c('NLI')
    ),
    #### Other ----
    base = tibble::tibble(
      model = c(
        'FacebookAI/xlm-roberta-base',
        'FacebookAI/xlm-roberta-large',
        'ai-forever/ruBert-base',
        'ai-forever/ruBert-large',
        'ai-forever/ruElectra-small',
        'ai-forever/ruElectra-medium',
        'ai-forever/ruElectra-large',
        'ai-forever/ruRoberta-large',
        'ai-forever/ru-en-RoSBERTa',
        'DeepPavlov/rubert-base-cased',
        'cointegrated/rubert-tiny2',
        'sergeyzh/rubert-mini-sts',
        'sergeyzh/rubert-tiny-turbo',
        'Pastushoc/rubert-tiny2-finetuned-fintech',
        'isolation-forest/setfit-absa-polarity',
        'IvashinMaxim/128Bert',
        'kartashoffv/vashkontrol-sentiment-rubert',
        'Adammz/rubert-base-cased-1-third',
        'numblilbug/finetuning-rubert-sentiment-model',
        'VivekMalipatel23/mDeBERTa-v3-base-text-emotion-classification',
        'cointegrated/rubert-tiny-sentiment-balanced',
        'fyaronskiy/ruRoberta-large-ru-go-emotions',
        'sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2'
      ),
      type = match_arch(model),
      sentence_level = match_type(model),
      task = c('LM')
    )
  )
)

## Load and Test ----
models_df <- purrr::list_rbind(
  purrr::map(models, purrr::list_rbind),
  names_to = 'lang'
)
res <- models_df |>
  dplyr::select(model, lang) |>
  as.list() |>
  purrr::pmap(purrr::safely(model_download_and_test))

res

models_df <- models_df |>
  dplyr::mutate(
    lang = factor(lang),
    type = factor(type),
    task = factor(task),
    ok = purrr::map_lgl(res, \(m) !length(m[['error']]))
  )

models_df
summary(models_df)
