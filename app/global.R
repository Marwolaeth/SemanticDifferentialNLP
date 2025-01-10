## GLOBAL ----
library(shiny)
library(shinyjs)
library(readxl)
library(glue)
library(stringr)
library(tictoc)

source('functions.R', encoding = 'UTF-8')

### Данные для примеров ----
examples <- read_excel('../data/umbrella-sentences.xlsx')
# Роскомнадзор разрабатывает систему для выявления запрещенного контента на основе искусственного интелекта.
# В Роскомнадзоре работают криворукие недоучки.

### Список моделей ----
load('../data/models/models.RData')

### Единый объект для оценки ----
universal_brand_name <- 'Y'