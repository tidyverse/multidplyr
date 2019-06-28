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

wrap <- function(..., indent = 0) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 2,
    width = getOption("width"))

  paste0(wrapped, collapse = "\n")
}

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}

cat_line <- function(...) {
  cat(paste(..., "\n", collapse = "", sep = ""))
}

drop_last <- function(x) {
  if (length(x) == 0) {
    x
  } else {
    x[-length(x)]
  }
}
