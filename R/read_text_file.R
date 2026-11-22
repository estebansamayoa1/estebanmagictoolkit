#' @export
read_text_file <- function(path, ext = NULL) {
  if (is.null(ext)) ext <- tools::file_ext(path)
  ext <- tolower(ext)

  text <- switch(
    ext,
    pdf  = paste(pdftools::pdf_text(path), collapse = "\n"),
    docx = {
      s <- officer::docx_summary(officer::read_docx(path))
      paste(s$text[s$content_type == "paragraph"], collapse = "\n\n")
    },
    txt  = paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    stop("unsupported file type: .", ext, ". only pdf, or txt files.")
  )

  if (!nzchar(trimws(text))) {
    stop("no text found")
  }
  text
}
