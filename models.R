re.match <- function(s, pattern) {
  grepl(pattern, s, ignore.case = TRUE)
}
re.match <- compiler::cmpfun(re.match, options = list(optimize = 3))

match_arch <- function(model_name) {
  
  model_arch <- dplyr::case_when(
    re.match(model_name, 'ELECTRA') ~ 'ELECTRA',
    re.match(model_name, 'DistillRoBERTa') ~ 'DistillRoBERTa',
    re.match(model_name, 'DistillBERT') ~ 'DistillBERT',
    re.match(model_name, 'CamemBERT') ~ 'CamemBERT',
    re.match(model_name, 'DeBERTa') ~ 'DeBERTa',
    re.match(model_name, 'RoBERTa') ~ 'RoBERTa',
    re.match(model_name, 'ALBERT') ~ 'ALBERT',
    re.match(model_name, 'BERT') ~ 'BERT',
    re.match(model_name, 'bge') ~ 'BAAI',
    re.match(model_name, 'nv-') ~ 'NV',
    NA ~ '[other]',
    .default = '[other]'
  )
}

models <- list(
  english = list(
    nli = tibble::tibble(
      model = c(
        'ynie/roberta-large-snli_mnli_fever_anli_R1_R2_R3-nli',
        'TFLai/Bert-Multilingual-NLI',
        'ynie/electra-large-discriminator-snli_mnli_fever_anli_R1_R2_R3-nli',
        'Capreolus/electra-base-msmarco',
        'ChrisZeng/electra-large-discriminator-nli-efl-hateval',
        # 'cointegrated/rubert-base-cased-nli-threeway',
        # 'cointegrated/rubert-base-cased-nli-twoway',
        'MoritzLaurer/mDeBERTa-v3-base-xnli-multilingual-nli-2mil7',
        # 'cointegrated/rubert-tiny-bilingual-nli',
        'MoritzLaurer/mDeBERTa-v3-base-mnli-xnli',
        'ynie/albert-xxlarge-v2-snli_mnli_fever_anli_R1_R2_R3-nli',
        'cross-encoder/nli-deberta-v3-base',
        'cross-encoder/nli-deberta-v3-large',
        'cross-encoder/qnli-electra-base',
        'cross-encoder/mmarco-mMiniLMv2-L12-H384-v1',
        'cross-encoder/nli-MiniLM2-L6-H768',
        'cross-encoder/nli-distilroberta-base',
        'cross-encoder/nli-deberta-v3-small',
        'cross-encoder/nli-deberta-v3-xsmall'
      ),
      type = match_arch(model),
      task = c('NLI')
    ),
    base = tibble::tibble(
      model = c(
        'bert-base-uncased',
        'distillbert-base-uncased',
        'BAAI/bge-base-en-v1.5',
        'nvidia/NV-Embed-v2',
        'MendelAI/nv-embed-v2-ontada-twab-peft'
      ),
      type = match_arch(model),
      task = c('LM')
    )
  )
)
