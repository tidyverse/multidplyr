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

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}


grouping_part <- function(cluster_id, groups_n) {

  if (length(cluster_id) >= length(groups_n)) {
    return(cluster_id[seq_along(groups_n)])
  }

  if(length(cluster_id) == 1L) return(rep(cluster_id, times = length(groups_n)))

  sorted_group_n <- sort(groups_n, decreasing = TRUE, index.return = TRUE)

  c(cluster_id,
    grouping_part(
      rev(cluster_id),
      sorted_group_n$x[-seq_along(cluster_id)]
    )
  )[sorted_group_n$ix]
}
