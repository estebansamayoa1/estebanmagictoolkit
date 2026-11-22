#' @export
extract_topics <- function(text, method = c("keywords", "lda"), n = 10, k = 4,
                           sentences_per_chunk = 5, seed = 123) {
  method <- match.arg(method)
  chunks <- chunk_text(text, sentences_per_chunk)
  stops  <- tidytext::stop_words$word

  if (method == "keywords") {
    words <- chunks |>
      tidytext::unnest_tokens(term, text) |>
      dplyr::filter(!term %in% stops, !grepl("[0-9]", term), nchar(term) > 2) |>
      dplyr::mutate(type = "word")

    phrases <- chunks |>
      tidytext::unnest_tokens(term, text, token = "ngrams", n = 2) |>
      dplyr::filter(!is.na(term)) |>
      tidyr::separate(term, c("w1", "w2"), sep = " ", remove = FALSE) |>
      dplyr::filter(!w1 %in% stops, !w2 %in% stops, !grepl("[0-9]", term)) |>
      dplyr::select(chunk, term) |>
      dplyr::mutate(type = "phrase")

    return(
      dplyr::bind_rows(words, phrases) |>
        dplyr::count(type, term, name = "frequency") |>
        dplyr::group_by(type) |>
        dplyr::slice_max(frequency, n = n, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::arrange(type, dplyr::desc(frequency))
    )
  }

  if (nrow(chunks) < k * 2) {
    stop("document too short ", k, " for LDA topics. ")
  }

  dtm <- chunks |>
    tidytext::unnest_tokens(word, text) |>
    dplyr::filter(!word %in% stops, !grepl("[0-9]", word), nchar(word) > 2) |>
    dplyr::count(chunk, word) |>
    tidytext::cast_dtm(chunk, word, n)

  model <- topicmodels::LDA(dtm, k = k, control = list(seed = seed))

  tidytext::tidy(model, matrix = "beta") |>
    dplyr::group_by(topic) |>
    dplyr::slice_max(beta, n = n, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::arrange(topic, dplyr::desc(beta))
}
