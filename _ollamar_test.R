library(ollamar)

list_models()
model_avail('deepseek-r1:7b')
model_avail('deepseek-r1:1.5b')

ollamar::pull('deepseek-r1:1.5b')
ollamar::pull('deepseek-r1:7b')

# text <- sents[[1]]
text <- 'Y тупая и отсталая контора.'
prompts[purrr::map_lgl(prompts, \(x) x$role == 'user')][[1]][['content']] <- paste(
  prompts[purrr::map_lgl(prompts, \(x) x$role == 'user')][[1]][['content']],
  text
)

res <- chat(
  'deepseek-r1:1.5b',
  messages = prompts,
  output = 'jsonlist'
)
