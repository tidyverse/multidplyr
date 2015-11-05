#' Attach a library on each node.
#'
#' @inheritParams objman
#' @param packages A character vector of package names
#' @export
#' @examples
#' cl <- create_cluster(2)
#' cl %>% cluster_eval(search())
#'
#' cl %>%
#'   cluster_library("magrittr") %>%
#'   cluster_eval(search())
cluster_library <- function(cluster, packages) {
  cluster_call(.cl = cluster, library, package = packages, character.only = TRUE)
  invisible(cluster)
}
