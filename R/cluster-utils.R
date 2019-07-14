#' Cluster utitility functions
#'
#' These functions provide useful helpers for performaning common operations.
#'
#' @param cluster Cluster to work on
#' @param ... Name-value pairs
#' @param packages Character vector of packages to load
#' @name cluster_utils
#' @return All functions that modify the worker environment invisibly return
#'   the cluster so they can chain them together. The other functions return
#'   lists with with one element for each worker.
#' @examples
#' cl <- default_cluster()
#' cluster_assign(cl, a = runif(1))
#' cluster_call(cl, a)
#'
#' # Assign different values on each cluster
#' cluster_assign_each(cl, b = c(1, 10))
#' cluster_call(cl, b)
#'
#' # If you want different to compute different values on each
#' # worker, use `cluster_call()` directly:
#' cluster_call(cl, c <- runif(1))
#' cluster_call(cl, c)
#'
#' # cluster_copy() is a useful shortcut
#' d <- 10
#' cluster_copy(cl, "d")
#'
#' cluster_ls(cl)
#' cluster_rm(cl, letters[1:4])
#' cluster_ls(cl)
#'
#' # Use cluster_library() to load packages
#' cluster_call(cl, search())
#' cluster_library(cl, "magrittr")
#' cluster_call(cl, search())
NULL

#' @rdname cluster_utils
#' @export
cluster_assign <- function(cluster, ...) {
  stopifnot(is_cluster(cluster))
  values <- list2(...)
  stopifnot(is_named(values))

  path <- tempfile()
  on.exit(unlink(path))

  qs::qsave(values, path, preset = "fast", check_hash = FALSE, nthreads = 2)
  cluster_walk(cluster, list2env(qs::qread(!!path), globalenv()))

  invisible(cluster)
}

#' @rdname cluster_utils
#' @export
cluster_assign_each <- function(cluster, ...) {
  stopifnot(is_cluster(cluster))
  values <- tibble(..., .rows = length(cluster))

  for (i in seq_len(nrow(values))) {
    cluster_assign(cluster[i], !!!lapply(values, "[[", i))
  }

  invisible(cluster)
}

#' @rdname cluster_utils
#' @param names Name of variables to copy.
#' @param env Environment in which to look for varibles to copy.
#' @export
cluster_copy <- function(cluster, names, env = caller_env()) {
  stopifnot(is_cluster(cluster))
  stopifnot(is.character(names))

  cluster_assign(cluster, !!!env_get_list(env, names, inherit = TRUE))
}

# Always suceeds: only gives warning if object not found, and warnings
# are not syndicated back to master
#' @rdname cluster_utils
#' @export
cluster_rm <- function(cluster, names) {
  stopifnot(is_cluster(cluster))
  stopifnot(is.character(names))

  cluster_call(cluster, rm(list = !!names, envir = globalenv()))
  invisible(cluster)
}

#' @rdname cluster_utils
#' @export
cluster_ls <- function(cluster) {
  cluster_call(cluster, ls(envir = globalenv()))
}

#' @rdname cluster_utils
#' @export
cluster_library <- function(cluster, packages) {
  lapply(packages, library, character.only = TRUE)

  cluster_call(cluster, lapply(!!packages, library, character.only = TRUE))
  invisible(cluster)
}
