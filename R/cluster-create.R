#' Create a new cluster with sensible defaults.
#'
#' Clusters created with this function will automatically clean up after
#' themselves. will automatically automatically clean up after itself.
#'
#' @importFrom parallel detectCores makePSOCKcluster
#' @param quiet If \code{TRUE}, don't display initialisation message.
#' @export
#' @examples
#' c <- create_cluster()
create_cluster <- function(cores = NA, quiet = FALSE) {
  if (is.na(cores)) {
    max <- parallel::detectCores()
    if (max == 1L) {
      cores <- 1L
    } else {
      # Always use at least two cores, but leave two free for other work
      cores <- pmax(2L, max - 2L)
    }
  }

  if (!quiet) message("Initialising ", cores, " core cluster.")
  cluster <- parallel::makePSOCKcluster(cores)
  attr(cluster, "finaliser") <- cluster_stop(cluster)

  cluster
}


cluster_stop <- function(x) {
  reg.finalizer(environment(), function(...) {
    parallel::stopCluster(x)
  })
  environment()
}
