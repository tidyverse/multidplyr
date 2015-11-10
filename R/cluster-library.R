#' Attach a library on each node.
#'
#' Also attached the library in the current session.
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
  library(packages, character.only = TRUE)
  cluster_call(.cl = cluster, library, package = packages, character.only = TRUE)
  invisible(cluster)
}
