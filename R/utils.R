#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}
