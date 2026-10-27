#' Create a new cluster with sensible defaults.
#'
#' A cluster is a pool of worker R processes managed by
#' [mizu](https://github.com/shikokuchuo/mizu). Work is submitted to the pool
#' as a whole and stolen by idle workers, so there is no way (or need) to
#' address individual workers. Workers are torn down when the cluster is
#' garbage collected.
#'
#' @param n Number of workers to create. Avoid setting this higher than the
#'   number of cores in your computer as it will degrade performance.
#' @export
#' @returns A `multidplyr_cluster` object.
#' @examples
#' cluster <- new_cluster(2)
#' cluster
new_cluster <- function(n) {
  env <- new.env(parent = emptyenv())
  env$bindings <- list()
  env$packages <- character()

  structure(
    list(
      pool = mizu::mizu_pool(
        n_workers = n,
        launcher = mizu::mizu_launcher(stdout = FALSE, stderr = FALSE),
        # Larger result slots give worker errors room to carry the shard
        # index alongside the message (512 drops the index, 2048 the index
        # again once the call fits)
        slot_size = 1024
      ),
      n = n,
      env = env
    ),
    class = "multidplyr_cluster"
  )
}

#' @export
length.multidplyr_cluster <- function(x) {
  x$n
}

#' @export
print.multidplyr_cluster <- function(x, ...) {
  status <- mizu::mizu_pool_status(x$pool)
  state_abbr <- ifelse(status$workers == "live", cli::col_green("."), "F")

  cat_line(x$n, " session cluster [", paste(state_abbr, collapse = ""), "]")
}

is_cluster <- function(x) inherits(x, "multidplyr_cluster")

cluster_bindings <- function(cluster) cluster$env$bindings

cluster_packages <- function(cluster) cluster$env$packages

#' Default cluster
#'
#' Setting up a cluster is relatively expensive, so it's best to use a single
#' cluster throughout a session. This function lazily creates a 2-worker
#' cluster for use in examples and test.
#'
#' @param n Number of workers to use; defaults to 2 because this is the maximum
#'   allowed by CRAN.
#' @keywords internal
#' @returns A cached cluster of workers.
#' @export
#' @examples
#' default_cluster()
default_cluster <- function(n = 2) {
  if (!env_has(cluster_env, "cluster")) {
    message("Initialising default cluster of size ", n)
    env_bind(cluster_env, cluster = new_cluster(n))
  } else {
    if (!missing(n)) {
      abort("Can not supply `n` when cluster has already been initiated")
    }
  }

  env_get(cluster_env, "cluster")
}

cluster_env <- env()
