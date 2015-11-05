#' Cluster management.
#'
#' For parallel operations in \code{\link{do}}, dplyr maintains a light-weight
#' local cluster. Cluster creation is relatively expensive (around a second)
#'
#' @name default_cluster
#' @examples
#' get_default_cluster()
NULL

cluster_env <- new.env(parent = emptyenv())

#' @export
#' @rdname default_cluster
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
