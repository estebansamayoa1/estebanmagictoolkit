#' @export
clean_text <- function(text) {
  text <- gsub("\f", "\n", text)
  text <- gsub("(\\w)-[ \t]*\n[ \t]*(\\w)", "\\1\\2", text, perl = TRUE)
  text <- gsub("\n[ \t]*\\d{1,4}[ \t]*\n", "\n", text)
  text <- gsub("(?<!\n)\n(?!\n)", " ", text, perl = TRUE)
  text <- gsub("[ \t]+", " ", text)
  text <- gsub("\n{2,}", "\n\n", text)
  trimws(text)
}
