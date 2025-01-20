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