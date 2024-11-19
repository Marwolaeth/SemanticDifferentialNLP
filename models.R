library(text)

models_nli <- tibble::tibble(
  model = c(
    'ynie/roberta-large-snli_mnli_fever_anli_R1_R2_R3-nli',
    'TFLai/Bert-Multilingual-NLI',
    'ynie/electra-large-discriminator-snli_mnli_fever_anli_R1_R2_R3-nli',
    'Capreolus/electra-base-msmarco',
    'ChrisZeng/electra-large-discriminator-nli-efl-hateval',
    'cointegrated/rubert-base-cased-nli-threeway',
    'cointegrated/rubert-base-cased-nli-twoway',
    'MoritzLaurer/mDeBERTa-v3-base-xnli-multilingual-nli-2mil7',
    'MoritzLaurer/mDeBERTa-v3-base-mnli-xnli',
    'cointegrated/rubert-tiny-bilingual-nli'
  ),
  task = c('NLI')
)