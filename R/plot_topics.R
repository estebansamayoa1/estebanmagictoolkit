#' @export
plot_topics <- function(topics) {
  if ("topic" %in% names(topics)) {
    topics |>
      dplyr::mutate(term = tidytext::reorder_within(term, beta, topic)) |>
      ggplot2::ggplot(ggplot2::aes(beta, term)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::facet_wrap(~ paste("Topic", topic), scales = "free_y") +
      tidytext::scale_y_reordered() +
      ggplot2::labs(x = "Importance in topic (beta)", y = NULL) +
      ggplot2::theme_minimal()
  } else {
    topics |>
      dplyr::mutate(term = tidytext::reorder_within(term, frequency, type)) |>
      ggplot2::ggplot(ggplot2::aes(frequency, term)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::facet_wrap(~ type, scales = "free") +
      tidytext::scale_y_reordered() +
      ggplot2::labs(x = "Frequency", y = NULL) +
      ggplot2::theme_minimal()
  }
}
