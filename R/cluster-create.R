#' Create a new cluster with sensible defaults.
#'
#' Clusters created with this function will automatically clean up after
#' themselves. will automatically automatically clean up after itself.
#'
#' @importFrom parallel detectCores makePSOCKcluster
#' @param cores Number of cores to use. If \code{NA} will uses at least two
#'   cores, but if yo have extra it leaves one free for other stuff.
#' @param quiet If \code{TRUE}, don't display initialisation message.
#' @export
create_cluster <- function(cores = NA, quiet = FALSE) {
  if (is.na(cores)) {
    cores <- guess_cores()
  }

  if (!quiet) message("Initialising ", cores, " core cluster.")
  cluster <- parallel::makePSOCKcluster(cores)
  attr(cluster, "finaliser") <- cluster_stop(cluster)

  cluster
}

guess_cores <- function() {
  if (in_check()) {
    return(2L)
  }

  max <- parallel::detectCores()
  if (max == 1L) 1L else pmax(2L, max - 1L)
}


cluster_stop <- function(x) {
  reg.finalizer(environment(), function(...) {
    parallel::stopCluster(x)
  })
  environment()
}

in_check <- function() {
  grepl("Rcheck$", getwd())
}
