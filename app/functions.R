library(text)
library(tokenizers)
library(purrr)
library(tibble)
library(tidyr)
library(dplyr)
library(SnowballC)
library(mall)

compiler::enableJIT(3)

## Text Utils ----
reticulate::source_python(
  system.file(
    'python',
    'huggingface_Interface3.py',
    package = 'text',
    mustWork = TRUE
  )
)

#' Concatenate tokens from transformer embeddings into words
#'
#' @param embeddings A data frame containing token embeddings with at least a 'tokens' column and embedding columns named according to dimension names from the `text` package.
#' @param aggregation A function or character string specifying the aggregation method .
#                ('mean', 'min', 'max', 'sum'). Default is mean.
#'
#' @return A data frame with concatenated tokens and aggregated embeddings.
#' @export
#'
#' @examples 
#' library(text)
#' embeddings <- textEmbed('I adore dogs')
#' tokens <- embeddings$tokens$texts[[1]]
#' concatenate_tokens(tokens, 'mean')
.concatenate_tokens <- function(token_embeddings, aggregation = mean) {
  # Check if the embeddings table is empty
  if (nrow(token_embeddings) == 0) stop('The embeddings table is empty')
  
  # Check if the provided aggregation function is valid
  if (is.character(aggregation)) {
    aggregation <- match.arg(aggregation, c('mean', 'min', 'max', 'sum'))
  }
  aggregation <- match.fun(aggregation)
  
  
  is_bert <- token_embeddings$tokens[[1]] == '[CLS]'
  
  special_token_start <- if_else(is_bert, '[', '<')
  word_start <- c('Ġ', '▁')
  subword_indicator <- '#'
  
  result <- token_embeddings |>
    dplyr::mutate(
      first_symbol = substr(tokens, 1, 1),
      special = first_symbol == special_token_start,
      empty = tokens %in% word_start,
      begin = (
        (first_symbol %in% word_start) |
          (!(first_symbol == subword_indicator) & is_bert)
      ) |
        special |
        dplyr::lag(special) |
        empty |
        dplyr::lag(empty),
      group = cumsum(begin),
      tokens = gsub('#+', '', tokens)
    )
  
  if (all(result$begin)) {
    result <- result |>
      dplyr::select(dplyr::all_of(names(token_embeddings)))
  } else {
    result <- result |>
      dplyr::summarise(
        # Concatenate tokens into words
        tokens = paste(tokens, collapse = ''),   
        # Aggregate embeddings
        dplyr::across(
          dplyr::starts_with('Dim'),
          aggregation
        ),
        .by = group
      ) |>
      dplyr::select(-group)
  }
  
  return(result)
}

text_embed_raw <- function(
    texts,
    model,
    device = 'cpu',
    tokenizer_parallelism = FALSE,
    trust_remote_code = TRUE,
    logging_level = 'error',
    max_token_to_sentence = 50
) {
  layers <- get_number_of_hidden_layers(
    model,
    trust_remote_code = trust_remote_code
  )
  
  data_character_variables <- text:::select_character_v_utf8(texts)
  
  x <- data_character_variables
  sorted_layers_ALL_variables <- list()
  sorted_layers_ALL_variables$context_tokens <- list()
  # Loop over all character variables; i_variables = 1
  for (i_variables in seq_len(length(data_character_variables))) {
    # Python file function to HuggingFace
    hg_embeddings <- hgTransformerGetEmbedding(
      text_strings = x[[i_variables]],
      model = model,
      layers = layers,
      return_tokens = TRUE,
      device = reticulate::r_to_py(device),
      tokenizer_parallelism = tokenizer_parallelism,
      model_max_length = NULL,
      max_token_to_sentence = max_token_to_sentence,
      hg_gated = FALSE,
      hg_token = Sys.getenv('HUGGINGFACE_TOKEN', unset = ''),
      trust_remote_code = trust_remote_code,
      logging_level = logging_level
    )
    
    variable_x <- text:::sortingLayers(
      x = hg_embeddings,
      layers = layers,
      return_tokens = TRUE
    )
    
    sorted_layers_ALL_variables$context_tokens[[i_variables]] <- variable_x
    names(
      sorted_layers_ALL_variables$context_tokens
    )[[i_variables]] <- names(x)[[i_variables]]
  }
  
  return(sorted_layers_ALL_variables)
}

.select_token <- function(embeddings, token = 1L, keep_first = FALSE) {
  if (is.numeric(token)) {
    embeddings <- dplyr::filter(embeddings, token_id == token)
  } else if (is.character(token)) {
    # Add an optional first character for RoBERTa tokenizers
    token_regex <- paste0('^.?', tolower(token), '$')
    embeddings <- dplyr::filter(
      embeddings,
      (token_id == 1 & keep_first) |
        (stringr::str_detect(tokens, token_regex))
    )
  } else {
    stop('`select_token` must be either numeric (integer) or character.')
  }
  
  # if (nrow(embeddings) == 0) {
  #   stop('No relevant tokens found.')
  # }
  
  return(embeddings)
}

text_embed_aggregation <- function(
    word_embeddings_layers,
    aggregation_from_tokens_to_texts = c('cls', 'mean', 'min', 'max', 'token'),
    select_token = NULL,
    keep_token_embeddings = FALSE
) {
  aggregation <- match.arg(
    aggregation_from_tokens_to_texts,
    c('cls', 'mean', 'min', 'max', 'token'),
    several.ok = FALSE
  )
  if (aggregation == 'token' & is.null(select_token)) {
    stop(
      paste(
        'Text level aggregation is set to use one token, but the token',
        'is not provided (`select_token = NULL`).'
      )
    )
  }
  
  # Loop over the list of variables; variable_list_i = 1; variable_list_i = 2; remove(variable_list_i)
  selected_layers_aggregated_tibble <- list()
  tokens_list <- list()
  word_embeddings_layers <- word_embeddings_layers$context_tokens
  for (variable_list_i in seq_len(length(word_embeddings_layers))) {
    
    x <- word_embeddings_layers[[variable_list_i]]
    if (tibble::is_tibble(x)) {
      x <- list(x)
    }
    
    # If provided, keep only the selected token to analyse
    if (!is.null(select_token)) {
      x <- purrr::map(x, .select_token, select_token, aggregation == 'cls')
    }
    
    x <- purrr::map(
      x,
      function(text_i) dplyr::select(text_i, -layer_number, -token_id)
    )
    
    if (is.null(aggregation)) {
      # Sort output
      selected_layers_aggregated_tibble[[variable_list_i]] <- x
    }
    
    # Aggregate across tokens
    if (!is.null(aggregation)) {
      selected_layers_aggregated <- purrr::map(
        x,
        function(text_i) dplyr::select(text_i, dplyr::starts_with('Dim'))
      )
      
      if (aggregation %in% c('token', 'cls')) {
        # Than the first row is what we actually need
        selected_layers_tokens_aggregated <- purrr::map(
          selected_layers_aggregated,
          function(embedding) dplyr::slice(embedding, 1)
        )
      } else {
        selected_layers_tokens_aggregated <- lapply(
          selected_layers_aggregated,
          text:::textEmbeddingAggregation,
          aggregation = aggregation
        ) 
      }
      # Sort output
      selected_layers_aggregated_tibble[[variable_list_i]] <- dplyr::bind_rows(
        selected_layers_tokens_aggregated
      )
    }
    tokens_list[[variable_list_i]] <- x
  }
  
  names(selected_layers_aggregated_tibble) <- names(word_embeddings_layers)
  names(tokens_list) <- names(word_embeddings_layers)
  
  if (keep_token_embeddings) {
    result <- list(
      tokens = tokens_list,
      texts = selected_layers_aggregated_tibble
    )
  } else {
    result <- list(
      texts = selected_layers_aggregated_tibble
    )
  }
  
  return(result)
}

text_embed <- function(
    texts,
    model,
    aggregation_from_tokens_to_texts = c('cls', 'mean', 'min', 'max', 'token'),
    select_token = NULL,
    keep_token_embeddings = FALSE,
    ...
) {
  embeddings_raw <- text_embed_raw(texts, model, ...)
  
  text_embed_aggregation(
    embeddings_raw,
    aggregation_from_tokens_to_texts = aggregation_from_tokens_to_texts,
    select_token = select_token,
    keep_token_embeddings = keep_token_embeddings
  )
}

## Utils ----
# Функция для быстрого заключения строки в скобки/кавычки/и т.д.
str_enclose <- function(s, enclosure = c('(', ')')){
  if (enclosure[1] == '(')   enclosure <- c(enclosure, ')')
  if (enclosure[1] == '((')  enclosure <- c(enclosure, '))')
  if (enclosure[1] == '[')   enclosure <- c(enclosure, ']')
  if (enclosure[1] == '[[')  enclosure <- c(enclosure, ']]')
  if (enclosure[1] == '[[[') enclosure <- c(enclosure, ']]]')
  if (enclosure[1] == '{')   enclosure <- c(enclosure, '}')
  if (enclosure[1] == '{{')  enclosure <- c(enclosure, '}}')
  if (enclosure[1] == '<')   enclosure <- c(enclosure, '>')
  if (enclosure[1] == '<<')  enclosure <- c(enclosure, '>>')
  if (enclosure[1] == '>')   enclosure <- c(enclosure, '<')
  if (enclosure[1] == '«')   enclosure <- c(enclosure, '»')
  if (enclosure[1] == '‘')   enclosure <- c(enclosure, '’')
  if (enclosure[1] == '“')   enclosure <- c(enclosure, '”')
  paste0(enclosure[1], s, enclosure[length(enclosure)])
}
str_enclose <- compiler::cmpfun(str_enclose, options = list(optimize=3))

str_parenthesise <- function(s){
  paste0('(', s, ')')
}
str_parenthesise <- compiler::cmpfun(
  str_parenthesise,
  options = list(optimize=3)
)

and <- function(x) {
  if (length(x) <= 1) return(x)
  delims <- c(rep(',', length(x) - 2), ' и', '')
  paste(paste0(x, delims), collapse = ' ')
}
and <- compiler::cmpfun(and, options = list(optimize = 3))

or <- function(x) {
  if (length(x) <= 1) return(x)
  delims <- c(rep(',', length(x) - 2), ' или', '')
  paste(paste0(x, delims), collapse = ' ')
}
or <- compiler::cmpfun(or, options = list(optimize = 3))

# Функция для токенизации текста на параграфы
#' Токенизация текста на параграфы
#'
#' Эта функция принимает текст в виде строки и разбивает его на параграфы,
#' используя символы новой строки для определения границ параграфов.
#'
#' @param text Строка, содержащая текст, который необходимо токенизировать.
#'             Ожидается, что параграфы отделены символом новой строки ('\n').
#'
#' @return Возвращает вектор параграфов, полученных из входного текста.
#'         Если текст пустой, возвращается пустой вектор.
#'
#' @examples
#' # Пример использования функции
#' text <- "Первый параграф.\nВторой параграф.\n\nТретий параграф."
#' paragraphs(text)
#' # Возвращает: c("Первый параграф.", "Второй параграф.", "Третий параграф.")
#'
#' # Пример с пустым текстом
#' paragraphs("")
#' # Возвращает: character(0)
#'
#' @export
paragraphs <- function(text) {
  tokenizers::tokenize_paragraphs(
    text,
    paragraph_break = '\n',
    simplify = TRUE
  )
}
paragraphs <- compiler::cmpfun(paragraphs, options = list(optimize=3))

sentences <- function(text) {
  tokenizers::tokenize_sentences(
    text,
    simplify = TRUE
  )
}
sentences <- compiler::cmpfun(sentences, options = list(optimize=3))

stem <- function(words) {
  SnowballC::wordStem(words = words, language = 'russian')
}
stem <- compiler::cmpfun(stem, options = list(optimize=3))

.stem_each <- function(x) {
  stringr::str_split(x, '\\s') |>
    lapply(stem) |>
    sapply(\(w) paste(w, collapse = ' '))
}
.stem_each <- compiler::cmpfun(.stem_each, options = list(optimize=3))

.object_regex <- function(x) {
  .stem_each(x) |>
    # In case the stemming did a poor job
    # stringr::str_remove_all(
    #   '(?<=\\w\\w)[[ьаеёиоуюя]|(ого)|([иыя]х)|([аиоьыя]м.?)]{1,3}\\b'
    # ) |>
    # Avoid keeping special symbols unascaped
    stringr::str_replace_all(
      '([\\[\\]\\(\\)\\-\\+\\?\\.\\*\\^\\^\\{\\}])',
      '\\\\\\1'
    ) |>
    stringr::str_replace_all(stringr::fixed(' '), stringr::fixed('\\w*\\s')) |>
    paste0('\\w*') |>
    str_parenthesise() |>
    paste(collapse = '|')
}
.object_regex <- compiler::cmpfun(.object_regex, options = list(optimize=3))

.replace_object <- function(text, object, replacement) {
  object_regex <- .object_regex(object)
  
  stringr::str_replace_all(
    text,
    stringr::regex(object_regex, ignore_case = TRUE),
    replacement = replacement
  )
}
.replace_object <- compiler::cmpfun(.replace_object, options = list(optimize=3))

.check_scale <- function(scale) {
  proper_format <- vapply(
    scale,
    \(x) !any(is.null(names(x))) & all(is.numeric(x)),
    logical(1)
  ) |> all()
  
  if (!proper_format) {
    stop('The labels must be named numeric vectors')
  }
}

softmax <- function(x) exp(x) / sum(exp(x))
softmax <- compiler::cmpfun(softmax, options = list(optimize=3))

## Analysis ----
### Backend Functions ----
# Функция для семантического дифференциала с нулевым обучением
#' Семантический дифференциал с нулевым обучением
#'
#' Эта функция выполняет анализ текста с использованием метода семантического
#' дифференциала, применяя модель нулевого обучения для классификации текстов
#' по заданным полярностям. Она позволяет оценить текстовые данные по 
#' различным критериям, таким как инновационность или популярность.
#'
#' @param texts Вектор строк, содержащий тексты для анализа.
#' @param model Строка, указывающая модель, используемую для анализа.
#' @param candidate_labels Вектор строк, содержащий названия полярностей для оценки.
#' @param template Шаблон гипотезы, который будет использоваться для классификации.
#' @param prefix Логическое значение, указывающее, следует ли добавлять префикс
#'                к текстам перед классификацией. По умолчанию FALSE.
#' @param aggregation Метод агрегации для объединения оценок (можно выбрать 'max' или 'mean').
#' @param mask Вектор, определяющий влияние каждой полярности на финальный результат.
#'              По умолчанию c(-1, 1).
#' @param multi_label Логическое значение, указывающее, поддерживает ли модель
#'                    многоклассовую классификацию. По умолчанию FALSE.
#'
#' @return Возвращает числовое значение, представляющее собой итоговый балл
#'         для текста, рассчитанный на основе оценок по полярностям и маске.
#'         Результат можно интерпретировать как обобщенную оценку текста:
#'         - Положительное значение указывает на более высокую оценку по
#'           положительным полярностям.
#'         - Отрицательное значение указывает на более высокую оценку по
#'           отрицательным полярностям.
#'         - Значение близкое к нулю может указывать на сбалансированное
#'           восприятие текста по рассматриваемым полярностям.
#'
#' @examples
#' # Пример использования функции
#' texts <- c("Это новый и интересный продукт.", "Старая модель неэффективна.")
#' model <- "model_name"
#' candidate_labels <- c("инновационный", "устаревший")
#' template <- "Этот продукт является {}."
#'
#' result <- semdiff_zeroshot(
#'   texts = texts,
#'   model = model,
#'   candidate_labels = candidate_labels,
#'   template = template,
#'   mask = c(1, -1),
#'   prefix = TRUE
#' )
#' print(result)
#'
#' @export
semdiff_zeroshot <- function(
    texts,
    model,
    candidate_labels,
    template,
    prefix = FALSE,
    aggregation = c('max', 'mean'),
    mask = c(-1, 1),
    multi_label = FALSE,
    append_neutral = FALSE,
    seed = 111,
    device = 'cpu'
) {
  aggregation <- match.arg(aggregation, c('max', 'mean'))
  aggregation <- match.fun(aggregation)
  
  if (prefix) texts <- paste('classification:', texts)
  
  if (append_neutral) {
    candidate_labels <- c(candidate_labels, 'ничего из перечисленного')
    mask <- c(mask, 0)
  }
  
  res <- hgTransformerGetZeroShot(
    sequences = texts,
    candidate_labels = candidate_labels,
    hypothesis_template = template,
    multi_label = FALSE,
    model = model,
    device = device,
    tokenizer_parallelism = FALSE, # To be checked!
    logging_level = 'error',
    force_return_results = FALSE,
    set_seed = seed
  )
  
  res_wide <- res |>
    dplyr::bind_rows() |>
    tidyr::pivot_wider(
      id_cols = sequence,
      names_from = labels,
      values_from = scores
    ) |>
    dplyr::select(sequence, dplyr::all_of(candidate_labels))
  
  if (length(texts) > 1) {
    res_wide <- res_wide |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(candidate_labels), aggregation)
      )
  }
  
  res_wide |>
    dplyr::mutate(
      score = sum(
        dplyr::c_across(dplyr::all_of(candidate_labels)) * mask
      ) / sum(mask > 0)
    ) |>
    dplyr::pull(score)
}

.extract_concepts <- function(items, ids, group = FALSE, sep = ', ') {
  concepts <- lapply(items, names) |>
    purrr::map(\(item) item[ids])
  if (group) {
    concepts <- concepts |>
      purrr::pmap_chr(paste, sep = sep)
  } else {
    concepts <- unlist(concepts)
  }
  
  return(concepts)
}
.extract_concepts <- compiler::cmpfun(.extract_concepts, options = list(optimize=3))

.items_to_norms <- function(
    items,
    model,
    as_phrases = FALSE,
    template = NULL,
    prefix = FALSE,
    group_items = FALSE,
    aggregation = if (as_phrases) 'cls' else 'mean',
    sep = ', ',
    ...
) {
  if (!is.list(items)) items <- list(items)
  
  .check_scale(items)
  
  concepts <- .extract_concepts(items, c(1, 3), group_items, sep)
  
  concept_names <- concepts
  
  if (as_phrases) {
    if (is.null(template)) template <- 'Y – {}'
    concepts <- sapply(
      concepts,
      function(concept) {
        stringr::str_replace(
          template,
          stringr::fixed('{}'),
          stringr::fixed(concept)
        )
      }
    )
  }
  if (prefix) concepts <- paste('classification:', concepts)
  
  concepts_df <- tibble::as_tibble(
    as.list(concepts) |> setNames(concept_names)
  )
  
  text_embed(
    concepts_df,
    model = model,
    keep_token_embeddings = FALSE,
    aggregation_from_tokens_to_texts = aggregation,
    ...
  )
}

similarity_norm <- function(
    text_embeddings,
    norm_embeddings,
    metric = 'cosine'
) {
  metric <- match.arg(metric, c('cosine', 'spearman', 'pearson', 'kendall'))
  
  if (nrow(text_embeddings) == 0) {
    return(matrix(0, nrow = 1, ncol = length(norm_embeddings$texts)))
  }
  
  texts <- text_embeddings |>
    dplyr::select(dplyr::starts_with('Dim')) |>
    as.matrix()
  concepts <- norm_embeddings$texts |>
    purrr::map(as.matrix) |>
    purrr::reduce(rbind)
  
  if (metric == 'cosine') {
    texts_norms <- sqrt(rowSums(texts^2))
    concept_norms <- sqrt(rowSums(concepts^2))
    
    S <- (texts %*% t(concepts)) / (texts_norms %*% t(concept_norms))
  } else {
    S <- cor(t(texts), t(concepts), method = metric)
  }
  
  colnames(S) <- names(norm_embeddings$texts)
  return(S)
}
similarity_norm <- compiler::cmpfun(similarity_norm, options = list(optimize=3))

benchmark_similarity <- function(
    norm_embeddings,
    model,
    template,
    prefix = FALSE,
    aggregation = if (prefix) 'cls' else 'token',
    metric = 'cosine',
    ...
) {
  concepts <- names(norm_embeddings$texts)
  
  hypotheses <- sapply(
    concepts,
    function(concept) {
      stringr::str_replace(
        template,
        stringr::fixed('{}'),
        stringr::fixed(concept)
      )
    }
  )
  
  if (prefix) hypotheses <- paste('classification:', hypotheses)
  
  hypotheses_embeddings <- text_embed(
    hypotheses,
    model,
    aggregation_from_tokens_to_texts = aggregation,
    ...
  )
  
  score <- diag(
    similarity_norm(
      hypotheses_embeddings$texts$texts,
      norm_embeddings,
      metric = metric
    )
  )
  names(score) <- concepts
  return(score)
}

generate_prompts <- function(
    scaleset,
    system_prompt_template,
    user_prompt_template,
    max_items = NULL
) {
  ## User ----
  n_scales <- length(scaleset)
  scale_names <- and(tolower(names(scaleset)))
  
  ## System ----
  ### Если плейсхолдера нет, то не надо ничего обрабатывать
  scaleset_description_required <- any(
    stringr::str_detect(
      c(system_prompt_template, user_prompt_template),
      fixed('{scaleset_description}')
    )
  )
  cat('scaleset_description_required:', scaleset_description_required)
  
  ### Если есть, построим описание набора шкал
  if (scaleset_description_required) {
    scaleset_description <- purrr::map2(
      scaleset,
      names(scaleset),
      function(scale, name) {
        markers <- map(scale, names)
        items_neg <- map_chr(markers, 1)
        items_pos <- map_chr(markers, 3)
        if (is.null(max_items)) max_items <- length(items_neg)
        if (max_items == 1L) {
          items_neg <- items_neg[1]
          items_pos <- items_pos[1]
        }
        if (length(items_neg) > 1) {
          # Негативны маркеры — дизъюнкция
          items_neg <- items_neg[1:max_items] |> or() |> str_parenthesise()
        }
        if (length(items_pos) > 1) {
          # Позитивные маркеры — конъюнкция
          items_pos <- items_pos[1:max_items] |> and() |> str_parenthesise()
        }
        glue::glue('"{name}": ', paste(items_pos, items_neg, sep = ' vs. '))
      }
    ) |>
      paste(collapse = ', ')
  }
  
  ### Если автоматический пример ответа не требуется, то не надо его составлять
  scaleset_example_required <- any(
    stringr::str_detect(
      c(system_prompt_template, user_prompt_template),
      fixed('{scaleset_example}')
    )
  )
  cat('scaleset_example_required:', scaleset_example_required)
  
  ### Если требуется, то надо составить
  if (scaleset_example_required) {
    scaleset_example <- purrr::map2(
      scaleset,
      names(scaleset),
      function(scale, name) {
        markers <- map(scale, names)
        rating <- sample(
          c(-5L, -4L, 0L, 4L, 5L),
          size = 1,
          prob = c(1, 1, 2, 1, 1)
        )
        items_neg <- map_chr(markers, 1)
        items_pos <- map_chr(markers, 3)
        if (rating  == 0) {
          comment <- 'The text provides no relevant info'
        } else {
          quantifier <- ifelse(abs(rating) == 5, 'очень', 'достаточно')
          if (rating < 0) {
            marker <- sample(items_neg, size = 1)
          } else {
            marker <- sample(items_pos, size = 1)
          }
          comment <- glue::glue(
            'Из текста можно сделать вывод, что бренд {quantifier} {marker}'
          )
        }
        glue::glue(
          '"{name}": {{"rating":{rating},"comment":"{comment}"}}'
        )
      }
    ) |>
      paste(collapse = ',') |>
      str_enclose('{')
  ### Если нет автоматического примера ответа, то, возможно, формат не прописан
  } else {
    #### Проверим, прописан ли формат
    format_example_provided <- stringr::str_detect(
      system_prompt_template,
      '\\"rating\\"\\:'
    ) & stringr::str_detect(
      system_prompt_template,
      '\\"comment\\"\\:'
    )
    cat('format_example_provided:', format_example_provided)
    #### Если не прописан, добавим
    if (!format_example_provided) {
      frmt <- '{"Scale name 1": {"rating":-2, "comment":"Your comment here"}}'
      
      ##### Если придется применять `glue`, то нужно избежать одиноких
      ###### фигурных скобок
      if (scaleset_description_required) frmt <- str_enclose(frmt, '{')
      format_hint <- paste(
        'Please for each scale return adistionary containing two entries:',
        '"raiting" and "comment", like this:',
        frmt
      )
      ##### Добавим описание формата в конец инструкции
      system_prompt_template <- paste(
        system_prompt_template,
        format_hint,
        sep = '\n'
      )
    }
  }
  
  ### Если есть хотя бы один плейсхолдер, используем `glue`
  if (scaleset_example_required | scaleset_description_required) {
    system_prompt_template <- glue::glue(system_prompt_template)
  }
  
  ## Сборка ----
  system_prompt <- ollamar::create_message(
    role = 'system',
    content = system_prompt_template
  )
  
  user_prompt <- ollamar::create_message(
    role = 'user',
    content = glue::glue(user_prompt_template)
  )
  
  prompts <- ollamar::create_messages(
    system_prompt,
    user_prompt
  )
  
  ## Проверка ----
  stopifnot(inherits(prompts, 'list'))
  stopifnot(ollamar::validate_messages(prompts))
  return(prompts)
}

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

.parse_response <- function(response, simplify = FALSE) {
  starts <- stringr::str_locate(response, stringr::fixed('{'))[,1, drop = FALSE]
  ends <- stringr::str_locate_all(response, stringr::fixed('}'))
  ends <- purrr::map_vec(
    ends,
    \(e) end = e[nrow(e), ncol(e), drop = FALSE]
  )
  locs <- cbind(start = starts, end = ends)
  responses <- stringr::str_sub(response, start = locs)
  jsonlite::fromJSON(
    paste0(
      '[',
      paste(responses, collapse = ','),
      ']'
    ),
    simplifyDataFrame = simplify
  )
}

.parse_tibble <- function(resp) {
  fake_tibble <- tibble::tibble(
    .score = 0, comment = 'The model failed to provide valid JSON'
  )
  
  result <- purrr::map(
    resp,
    purrr::safely(.parse_response)
  )
  
  res <- purrr::map(
    result,
    function(x) {
      if (!is.null(x$error)) {
        fake_tibble 
      } else {
        purrr::map(
          x$result[[1]],
          \(y) as_tibble(y) |>
            dplyr::rename(.score = rating) |>
            dplyr::mutate(.score = .score / 5)
        )
      }
    }
  ) |>
    purrr::transpose() |>
    purrr::map(dplyr::bind_rows)
}

.parse_garbage <- function(resp) {
  fake_tibble <- tibble::tibble(
    .score = 0, comment = 'The model failed to provide valid JSON'
  )
  
  scores <- stringr::str_extract_all(resp, '(?<=rating\\"?\\:\\s?)\\-?\\d') |>
    purrr::transpose()
  comments <- resp |>
    stringr::str_squish() |>
    stringr::str_extract_all('(?<=comment\\"?\\:).+?(?=\\})') |>
    purrr::map(
      \(t) stringr::str_squish(t) |> stringr::str_remove_all("[\"\']")
    ) |>
    purrr::transpose()
  
  lapply(
    1:length(scores),
    function(i) {
      tryCatch(
        {
          score <- as.numeric(unlist(scores[[i]]))
          score <- ifelse(is.na(score), 0, score)
          tibble::tibble(
            .score = score / 5,
            comment = unlist(comments[[i]])
          )
        },
        error = function(e) fake_tibble
      )
    }
  )
}

### Wrapper Functions ----
# Функция для обработки нескольких текстов с нулевым обучением
# Функция для семантического дифференциала с нулевым обучением (векторизированная)
#' Семантический дифференциал с нулевым обучением (векторизированный)
#'
#' Эта функция выполняет анализ текста с использованием метода семантического
#' дифференциала, применяя модель нулевого обучения для классификации текстов
#' по заданным полярностям. В отличие от функции `semdiff_zeroshot()`, где
#' аргумент `candidate_labels` используется для указания возможных меток,
#' в этой функции аргумент `items` представляет собой список именованных
#' числовых векторов, где имена служат потенциальными метками классов, а
#' значения используются в качестве маски для расчета результата.
#'
#' @param texts Вектор строк, содержащий тексты для анализа.
#' @param model Строка, указывающая модель, используемую для анализа.
#' @param items Список именованных числовых векторов, содержащих полярности,
#'                   которые будут оцениваться для каждого текста. Имена векторов
#'                   служат метками классов, а значения — маской для расчета результата.
#' @param template Шаблон гипотезы, который будет использоваться для классификации.
#' @param prefix Логическое значение, указывающее, следует ли добавлять префикс
#'                к текстам перед классификацией. По умолчанию FALSE.
#' @param ... Прочие аргументы, передаваемые в `semdiff_zeroshot()`.
#'
#' @return Возвращает дата-фрейм, содержащий для каждого текста
#'         усредненные оценки по всем указанным полярностям. Каждая строка
#'         соответствует тексту, а переменная `.score` – оценке данного текста.
#'         Результат можно интерпретировать
#'         как обобщенные оценки текстов:
#'         - Положительные значения указывают на более высокую оценку по
#'           положительным полярностям.
#'         - Отрицательные значения указывают на более высокую оценку по
#'           отрицательным полярностям.
#'         - Значения близкие к нулю могут указывать на сбалансированное
#'           восприятие текста по рассматриваемым полярностям.
#'
#' @examples
#' # Пример использования функции
#' texts <- c("Это новый и интересный продукт.", "Старая модель неэффективна.")
#' model <- "model_name"
#' items <- list(
#'   c('устаревший' = -1, 'сдержанный' = 0, 'инновационный'   = 1),
#'   c('отсталый'   = -1, 'стабильный' = 0, 'изобретательный' = 1)
#' )
#' template <- "Этот продукт является {}."
#'
#' result <- semdiff_zeroshot_map(
#'   texts = texts,
#'   model = model,
#'   items = items,
#'   template = template,
#'   prefix = TRUE
#' )
#' print(result)
#'
#' @export
semdiff_zeroshot_map <- function(
    texts,
    model,
    items,
    template,
    prefix = FALSE,
    ...
) {
  
  # Можно добавить несколько наборов меток классов
  ## Тогда их придется обрабатывать последовательно
  if (!is.list(items)) items <- list(items)
  
  .check_scale(items)
  
  # Текстам нужен свой ID
  ids <- tibble::tibble(
    texts = texts,
    text_id = 1:length(texts)
  )
  
  # Генерация всех комбинаций текстов и полярностей
  tidyr::expand_grid(
    texts, items
  ) |>
    dplyr::mutate(
      mask = items,
      items = lapply(items, names),
      .score = purrr::pmap_dbl(
        list(texts, items, mask),
        function(texts, items, mask) {
          semdiff_zeroshot(
            texts = paragraphs(texts),
            model = model,
            candidate_labels = items,
            template = template,
            mask = mask,
            multi_label = FALSE,
            prefix = prefix,
            ...
          )
        }
      )
    )
  
  # Объединение результатов с идентификаторами текстов
  # res <- analysis_grid |>
  #   dplyr::left_join(ids, by = 'texts') |>
  #   dplyr::group_by(text_id) |>
  #   dplyr::summarise(across(.score, mean)) |>
  #   dplyr::left_join(ids, by = 'text_id') |>
  #   dplyr::relocate(texts, .after = 1)
}

semdiff_similarity <- function(
    text_embeddings,
    norm_embeddings,
    similarity_metric = 'cosine',
    use_softmax = FALSE,
    temperature = 10,
    ...
) {
  
  # Number of differential items un the scale
  n_items <- length(norm_embeddings$texts)
  
  # Number of item pairs
  n_comparisons <- n_items / 2
  
  similarity_matrix <- similarity_norm(
    text_embeddings$texts$texts,
    norm_embeddings,
    metric = similarity_metric
  )
  
  max_bipolar_scores <- apply(similarity_matrix, 2, max, na.rm = TRUE)
  
  if (use_softmax) {
    max_bipolar_scores <- softmax(max_bipolar_scores * temperature)
    # Summation pattern
    mask <- rep(c(0, 1), n_comparisons)
  } else {
    mask <- rep(c(-1, 1), n_comparisons)
  }
  
  score <- ((max_bipolar_scores %*% mask) / n_comparisons)[1]
  
  tibble::tibble(
    items = paste(names(norm_embeddings$texts), collapse = ' – '),
    .score = score
  )
}

semdiff_chat <- function(
    texts,
    backend,
    prompts,
    scale_names,
    trust_model_output = TRUE
) {
  resp <- m_backend_submit(
    backend = backend,
    x = texts,
    prompt = prompts,
    preview = FALSE
  )
  
  # Модели часто ошибаются в JSON
  if (trust_model_output) {
    output <- .parse_tibble(resp)
  } else {
    output <- .parse_garbage(resp)
  }
  
  purrr::set_names(output, scale_names)
}

## Results Processing ----
show_scales_result <- function(result) {
  if (tibble::is_tibble(result)) {
    result |>
      dplyr::rowwise() |>
      dplyr::mutate(
        items = paste(items, collapse = ' – ')
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-mask, -texts)
  } else if (is.list(result) | !is.data.frame(result)) {
    purrr::map(result, show_scales_result)
  }
}

## UI ----
with_red_spinner <- function(
    ui_element,
    size = 1.8,
    caption = 'Pending Evaluation'
) {
  shinycssloaders::withSpinner(
    ui_element,
    type = 2,
    color = '#d73925',
    color.background = '#ECF0F5',
    hide.ui = FALSE,
    size = size,
    caption = caption
  )
}

with_helper <- function(ui_element, content) {
  helper(
    ui_element,
    colour = '#d73925',
    type = 'markdown',
    content = content,
    buttonLabel = 'Понятно',
    easyClose = TRUE,
    fade = TRUE
  )
}
