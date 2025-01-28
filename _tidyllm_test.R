devtools::install_github('edubruell/tidyllm', build = TRUE)
library(tidyllm)
library(tictoc)

## Example Texts ----
sents <- c(
  'Инженеры Xcellent работают над созданием микропроцессоров, которые могут изменить представление о вычислительных мощностях.',
  'В то время как Xcellent сохраняет свои позиции, другие игроки, такие как BrightFuture, активно внедряют новшества.',
  'Среди последних трендов в технологиях стоит отметить, как Xcellent внедряет инновационные решения в своем производстве.',
  'Кажется, Xcellent начали забывать. А когда-то он ведь был популярным брендом.',
  'Xcellent получил премию Business Awards как самая инновационная компания.',
  'XCellent тупой и отсталый',
  'XCellent — самая инновационная компания в мире.',
  'Umbrella очень инновационная компания.',
  'Umbrella очень инновационная компания, а XCellent отстает в развитии.'
)
universal_brand_name <- 'Y'
(sents <- .replace_object(sents, 'Xcellent', universal_brand_name))

## The Prompts ----
load('data/prompts/prompts-4-uniqueness-en-2025-01-28.RData')
prompts

### Old Prompt Format ----
user_prompt <- paste(prompts$user, '{text}', sep = '\n')
sent_prompts <- glue::glue(user_prompt, text = sents)

user_prompts <- purrr::map(sent_prompts, llm_message, .system_prompt = prompts$system)
user_prompts

model <- ollama(.model = 'phi4:latest', .seed = 111L, .temperature = 0)
model

chat(llm_message('Say hello'), model)

tic()
res <- chat(user_prompts[[1]], model)
toc()

### Prompts with a Schema ----

semdiff_schema <- tidyllm_schema(
  name = 'semantic_differential',
  scale = 'factor(Инновационность, Популярность, Надежность, Уникальность)[]',
  rating = 'numeric[]',
  comment = 'character[]'
)

system_prompt <- 'You are a smart and skilled content analysis engine tailored for brand analytics. Your task is to assess the image of a brand called "Y" as portrayed in a provided text in Russian, which may include news articles, summaries, blog posts, press releases, or tweets. You should focus on what is said in the text and not attempt to evaluate the brand\'s objective characteristics, as our task is content analysis. Your output should include ratings on various semantic scales (e.g., "Инновационность" (Innovation): инновационный vs. устаревший, "Популярность" (Popularity): модный vs. немодный, "Надежность" (Reliability): надежная vs. ненадежная продукция, "Уникальность" (Uniqueness, Individuality): бренд для настоящих ценителей и символ изысканного вкуса vs. бренд для массового потребителя) on a scale from 5 to -5, where -5 indicates an extremely negative rating on a given scale. If the text does not provide relevant information to assess a given trait, please assign a rating of 0 for that scale and indicate that the text was insufficient. Do not fantasise. Format the output as a JSON string, separating the ratings from the explanations. Example: "text": "В последнее время об Y совсем забыли", "scale": "Популярность", "rating": -4, "comment": "The text suggests that Y is very obscure now, but the word «забыли» suggests that it was somewhat famous formerly."'

system_prompt <- 'You are a smart and skilled content analysis engine tailored for brand analytics. Your task is to assess the image of a brand called "Y" as portrayed in a provided text in Russian, which may include news articles, summaries, blog posts, press releases, or tweets. 

**Important Note**: Focus solely on the subjective characteristics expressed in the text. This includes opinions, perceptions, and interpretations related to the brand. Do not seek objective evidence or details about the brand\'s characteristics. For example, if the text states "В Y работают криворукие недоучки," you should recognize this as a negative subjective assessment without needing to verify the claim.

Your output should include ratings on various semantic scales (e.g., "Инновационность" (Innovation): инновационный vs. устаревший, "Популярность" (Popularity): модный vs. немодный, "Надежность" (Reliability): надежная vs. ненадежная продукция, "Уникальность" (Uniqueness, Individuality): бренд для настоящих ценителей и символ изысканного вкуса vs. бренд для массового потребителя) on a scale from 5 to -5, where -5 indicates an extremely negative rating on a given scale. If the text does not provide relevant information to assess a given trait, please assign a rating of 0 for that scale and indicate that the text was insufficient. Do not fantasise. 

Format the output as a JSON string, separating the ratings from the explanations. Example: 
  "text": "В последнее время об Y совсем забыли", 
"scale": "Популярность", 
"rating": -4, 
"comment": "The text suggests that Y is very obscure now, but the word «забыли» suggests that it was somewhat famous formerly."'

user_prompts <- 'Please assess the following text for the "Y" brand image on four semantic scales, each ranging from -5 to 5. Сосредоточьтесь на следующих аспектах: инновационность, популярность, надежность и уникальность. Please focus on each aspect one at a time. Be sure to always assign 0 if you do not have enough information, and do not fantasise. Текст: "{text}"' |>
  glue::glue(text = sents) |>
  purrr::map(llm_message, .system_prompt = system_prompt)

# backend <- ollama(
#   .model = 'phi4',
#   .seed = 111L,
#   .temperature = 0,
#   .keep_alive = '20m'
# )
# backend

tic()
res <- chat(
  user_prompts[[2]],
  .provider = ollama(),
  .model = 'phi4:latest',
  .temperature = 0,
  .seed = 111,
  .json_schema = semdiff_schema,
  .timeout = 1200
)
toc()

res |> get_reply_data() |> str()
res |>
  get_reply_data() |>
  tibble::as_tibble() |>
  dplyr::group_by(scale) |>
  dplyr::group_split(.keep = F)
