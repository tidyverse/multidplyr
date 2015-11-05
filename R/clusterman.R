cluster_env <- new.env(parent = emptyenv())
reg.finalizer(cluster_env, function(e) {
  stopCluster(e$cluster)
})

#' Cluster management.
#'
#' For parallel operations in \code{\link{do}}, dplyr maintains a light-weight
#' local cluster. Cluster creation is relatively expensive (around a second)
#' so the cluster is created once and cached for subsequent use.
#'
#' On windows, this is a PSOCK cluster, and on linux/mac it's a fork cluster.
#' If not supplied, the number of cores will be two less than the number of
#' cores provided by \code{\link[parallel]{detectCores}}.
#'
#' \code{get_cluster}, \code{set_cluster} and \code{has_cluster} are low
#' level accessor functions to control the cluster cache. You shouldn't
#' need to use these unless you want to create your own cluster using another
#' mechanism. \code{init_cluster} creates and caches a new cluster, and
#' \code{stop_cluster} shuts it down.
#'
#' @keywords internal
#' @name dplyr-cluster
NULL

#' @export
#' @rdname dplyr-cluster
cluster_set <- function(x) {
  if (!inherits(x, "cluster")) {
    stop("x is not a cluster", call. = FALSE)
  }

  cluster_stop(cluster_env$cluster)
  cluster_env$cluster <- x

  invisible(x)
}

#' @export
#' @rdname dplyr-cluster
#' @importFrom parallel stopCluster
cluster_stop <- function(cl) {
  if (cluster_exists()) {
    stopCluster(cluster_env$cluster)
    rm("cluster", envir = cluster_env)
  }

  invisible(TRUE)
}

#' @export
#' @rdname dplyr-cluster
cluster_get <- function() {
  if (!cluster_exists()) {
    cluster_create()
  }

  cluster_env$cluster
}

#' @export
#' @rdname dplyr-cluster
cluster_exists <- function() {
  exists("cluster", where = cluster_env)
}

#' @importFrom parallel detectCores makePSOCKcluster makeForkCluster
#' @export
#' @rdname dplyr-cluster
cluster_create <- function(cores = NA, quiet = FALSE) {
  if (is.na(cores)) {
    max <- parallel::detectCores()
    if (max == 1L) {
      stop("Single core computer. Parallelisation not available", call. = FALSE)
    }
    # Always use at least two cores, but leave two free for other work
    cores <- pmax(2L, max - 2L)
  }

  if (!quiet) message("Initialising ", cores, " core cluster.")
  cluster <- parallel::makePSOCKcluster(cores)

  cluster_set(cluster)
  cluster
}
