#' Cluster management.
#'
#' If no cluster is currently active, \code{get_default_cluster()} will create
#' one.
#'
#' @name default_cluster
#' @examples
#' get_default_cluster()
NULL

cluster_env <- new.env(parent = emptyenv())

#' @export
#' @rdname default_cluster
#' @param x New cluster to use as default.
set_default_cluster <- function(x) {
  if (!inherits(x, "cluster")) {
    stop("x is not a cluster", call. = FALSE)
  }

  cluster_env$cluster <- x
  invisible(x)
}


#' @export
#' @rdname default_cluster
get_default_cluster <- function() {
  if (!cluster_exists()) {
    x <- create_cluster()
    set_default_cluster(x)
  }

  cluster_env$cluster
}

cluster_exists <- function() {
  exists("cluster", where = cluster_env)
}
