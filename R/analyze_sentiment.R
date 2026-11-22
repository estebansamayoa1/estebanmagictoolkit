#' @export
analyze_sentiment <- function(text, neutral_band = 0.05) {
  sents <- sentimentr::get_sentences(text)

  sentences <- tibble::as_tibble(sentimentr::sentiment(sents)) |>
    dplyr::mutate(position = dplyr::row_number(),
                  sentence = unlist(sents)) |>
    dplyr::select(position, sentence, word_count, sentiment)

  avg <- mean(sentences$sentiment, na.rm = TRUE)
  overall <- tibble::tibble(
    average_sentiment = avg,
    sd_sentiment      = stats::sd(sentences$sentiment, na.rm = TRUE),
    n_sentences       = nrow(sentences),
    label = dplyr::case_when(
      avg >  neutral_band ~ "positive",
      avg < -neutral_band ~ "negative",
      TRUE                ~ "neutral"
    )
  )

  words <- tibble::tibble(text = text) |>
    tidytext::unnest_tokens(word, text) |>
    dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") |>
    dplyr::count(word, sentiment, sort = TRUE)

  list(overall = overall, sentences = sentences, words = words)
}
