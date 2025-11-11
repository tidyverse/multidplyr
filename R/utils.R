#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

table_name <- local({
  i <- 0
  function() {
    i <<- i + 1
    paste0("_DF", i)
  }
})

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}

cat_line <- function(...) {
  cat(paste(..., "\n", collapse = "", sep = ""))
}
