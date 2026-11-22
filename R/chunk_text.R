#' @export
chunk_text <- function(text, sentences_per_chunk = 5) {
  sents <- unlist(sentimentr::get_sentences(text))
  ids <- ceiling(seq_along(sents) / sentences_per_chunk)
  tibble::tibble(
    chunk = sort(unique(ids)),
    text  = vapply(split(sents, ids), paste, character(1), collapse = " ")
  )
}
