#' Cluster utitility functions
#'
#' These functions provide useful helpers for performaning common operations.
#' `cluster_assign()` assigns the same value on each worker;
#' `cluster_assign_each()` assigns different values on each worker;
#' `cluster_assign_partition()` partitions vectors so that each worker gets
#' (approximately) the same number of pieces.
#'
#' @param cluster,.cluster Cluster to work on
#' @param ... Name-value pairs
#' @param packages Character vector of packages to load
#' @name cluster_utils
#' @return Functions that modify the worker environment invisibly return
#'   `cluster` so calls can be piped together. The other functions return
#'   lists with one element for each worker.
#' @examples
#' cl <- default_cluster()
#' cluster_assign(cl, a = runif(1))
#' cluster_call(cl, a)
#'
#' # Assign different values on each cluster
#' cluster_assign_each(cl, b = c(1, 10))
#' cluster_call(cl, b)
#'
#' # Partition a vector so that each worker gets approximately the
#' # same amount of it
#' cluster_assign_partition(cl, c = 1:11)
#' cluster_call(cl, c)
#'
#' # If you want different to compute different values on each
#' # worker, use `cluster_call()` directly:
#' cluster_call(cl, d <- runif(1))
#' cluster_call(cl, d)
#'
#' # cluster_copy() is a useful shortcut
#' e <- 10
#' cluster_copy(cl, "e")
#'
#' cluster_call(cl, ls())
#' cluster_rm(cl, letters[1:5])
#' cluster_call(cl, ls())
#'
#' # Use cluster_library() to load packages
#' cluster_call(cl, search())
#' cluster_library(cl, "magrittr")
#' cluster_call(cl, search())
NULL

#' @rdname cluster_utils
#' @export
cluster_assign <- function(.cluster, ...) {
  stopifnot(is_cluster(.cluster))
  values <- list2(...)
  stopifnot(is_named(values))

  path <- tempfile()
  on.exit(unlink(path))

  qs::qsave(values, path, preset = "fast", check_hash = FALSE, nthreads = 2)
  cluster_send(.cluster, list2env(qs::qread(!!path), globalenv()))

  invisible(.cluster)
}

#' @rdname cluster_utils
#' @export
cluster_assign_each <- function(.cluster, ...) {
  stopifnot(is_cluster(.cluster))
  values <- tibble(..., .rows = length(.cluster))

  for (i in seq_len(nrow(values))) {
    cluster_assign(.cluster[i], !!!lapply(values, "[[", i))
  }

  invisible(.cluster)
}

#' @export
#' @rdname cluster_utils
cluster_assign_partition <- function(.cluster, ...) {
  stopifnot(is_cluster(.cluster))
  values <- list(...)

  m <- length(.cluster)
  values_split <- lapply(values, function(x) {
    vctrs::vec_split(x, cut(vctrs::vec_seq_along(x), m, labels = FALSE))$val
  })

  cluster_assign_each(.cluster, !!!values_split)
  invisible(.cluster)
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

  cluster_send(cluster, rm(list = !!names, envir = globalenv()))
  invisible(cluster)
}

#' @rdname cluster_utils
#' @export
cluster_library <- function(cluster, packages) {
  lapply(packages, library, character.only = TRUE)

  cluster_send(cluster, lapply(!!packages, library, character.only = TRUE))
  invisible(cluster)
}
