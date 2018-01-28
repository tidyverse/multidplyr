#' Create a new cluster with sensible defaults.
#'
#' Clusters created with this function will automatically clean up after
#' themselves.
#'
#' @importFrom parallel detectCores makePSOCKcluster makeCluster
#' @param cores Number of cores to use. If \code{NA} will uses at least two
#'   cores, but if you have extra it leaves one free for other stuff.
#'   For clusters other than \code{'PSOCK'} this parameter can be used to
#'   set up cluster description
#' @param quiet If \code{TRUE}, don't display initialisation message.
#' @param type Type of the cluster;  by default \code{'PSOCK'},
#'   which calls \code{makPSOCKcluster}, but other types supported by
#'   parallel package are possible, including \code{'MPI'} for MPI-based
#'   environments.
#' @export
create_cluster <- function(cores = NA, quiet = FALSE, type = 'PSOCK') {
  if (is.na(cores)) {
    cores <- guess_cores()
  }

  if (!quiet) message("Initialising ", cores, " core cluster.")
  if (type == 'PSOCK') {
      cluster <- parallel::makePSOCKcluster(cores)
  } else {
      cluster <- parallel::makeCluster(cores, type=type)
  }
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
  paths <- strsplit(getwd(), "/", fixed = TRUE)[[1]]
  any(grepl("Rcheck$", paths))
}
