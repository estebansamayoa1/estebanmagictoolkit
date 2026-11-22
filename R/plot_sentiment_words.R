#' @export
plot_sentiment_words <- function(sentiment_result, n = 10) {
  sentiment_result$words |>
    dplyr::group_by(sentiment) |>
    dplyr::slice_max(n, n = n, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(word = tidytext::reorder_within(word, n, sentiment)) |>
    ggplot2::ggplot(ggplot2::aes(n, word, fill = sentiment)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::facet_wrap(~ sentiment, scales = "free_y") +
    tidytext::scale_y_reordered() +
    ggplot2::scale_fill_manual(values = c(negative = "#c0392b", positive = "#27ae60")) +
    ggplot2::labs(x = "Count", y = NULL) +
    ggplot2::theme_minimal()
}
