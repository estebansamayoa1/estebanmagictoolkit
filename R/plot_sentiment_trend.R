#' @export
plot_sentiment_trend <- function(sentiment_result) {
  ggplot2::ggplot(sentiment_result$sentences,
                  ggplot2::aes(position, sentiment)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_line(colour = "grey70") +
    ggplot2::geom_smooth(method = "loess", se = FALSE, colour = "darkred",
                         formula = y ~ x) +
    ggplot2::labs(x = "Sentence position in document", y = "Sentiment score") +
    ggplot2::theme_minimal()
}
