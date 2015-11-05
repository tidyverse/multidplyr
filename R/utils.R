#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

wrap <- function(..., indent = 0) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 2,
    width = getOption("width"))

  paste0(wrapped, collapse = "\n")
}
