#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}

cat_line <- function(...) {
  cat(paste(..., "\n", collapse = "", sep = ""))
}
