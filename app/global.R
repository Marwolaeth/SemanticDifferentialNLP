## GLOBAL ----
library(shiny)
library(shinyjs)
library(shinyvalidate)
library(readxl)
library(glue)
library(stringr)
library(tictoc)
library(reactable)

source('functions.R', encoding = 'UTF-8')

## Данные для примеров ----
examples <- read_excel('../data/umbrella-sentences.xlsx')
# Роскомнадзор разрабатывает систему для выявления запрещенного контента на основе искусственного интелекта.
# В Роскомнадзоре работают криворукие недоучки.

## Список моделей ----
load('../data/models/models.RData')

## Единый объект для оценки ----
universal_brand_name <- 'Y'

## Шкала тахометров ----
GAUGE_SCALE <- 5

## Prompts ----
### System ----
default_system_prompt_template <- paste0(
  'You are a skillful content analysis engine model tailored ',
  'for brand analytics. Your task is to assess the image of a given brand ',
  'based on any provided text in Russian that mentions the brand name (substituted as "{universal_brand_name}"), which may include ',
  'news articles, summaries, blog posts, press releases, or tweets.\n',
  'Your output should include ratings on various semantic scales (e.g., ',
  '{scaleset_description} ',
  'on a scale from 5 to -5.\n',
  'If the text does not provide relevant information to assess a given trait, ',
  'please assign a rating of 0 for that scale and indicate that the text was insufficient. Do not fantasise. No information means 0.\n',
  'Please double check the sign of your rating. Is it positive or negative?\n',
  'Format the output as a JSON string, separating the rating from the explanation.\n',
  'Example: {scaleset_example}.\n',
  'You are very rigorous. Please double-check your response format and all ',
  'the braces: it must be a valid JSON.'
)


### User ----
default_user_prompt_template <- paste(
  'Please assess the following text for brand image on {n_scales} scales.',
  'Сосредоточьтесь на следующих аспектах: {scale_names}.',
  'Please focus on each aspect one at a time.',
  'Be sure to always assign 0 if you have not enough information,',
  'and do not fantasise.',
  'Пожалуйста, предоставьте результаты в формате JSON, упаковывая оценки по каждому аспекту. Please double-check your response format and all the braces:',
  'it must be a valid JSON.',
  'Текст:'
)

## Отображение Reactable ----
options(reactable.language = reactableLang(
  searchPlaceholder = 'Поиск…',
  noData = 'Нет записей',
  pageInfo = 'Записи {rowStart}–{rowEnd} из {rows}',
  pagePrevious = '\u276e',
  pageNext = '\u276f',
  
  # Accessible labels for assistive technologies such as screen readers.
  # These are already set by default, but don't forget to update them when
  # changing visible text.
  pagePreviousLabel = 'Предыдущая страница',
  pageNextLabel = 'Следующая страница',
  pageSizeOptions = 'Показывать {rows} записей'
))