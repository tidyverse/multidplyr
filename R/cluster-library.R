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
#'
#' cl %>%
#'   cluster_library(c("magrittr", "stats")) %>%
#'   cluster_eval(search())
cluster_library <- function(cluster, packages) {
  lapply(packages, library, character.only = TRUE)
  cluster_call(.cl = cluster, lapply, FUN = library, X = packages,
               character.only = TRUE)
  invisible(cluster)
}
