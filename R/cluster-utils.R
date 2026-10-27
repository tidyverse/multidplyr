#' Cluster utility functions
#'
#' These functions maintain the cluster's *registry*: the set of values and
#' packages made visible inside every verb evaluation and [cluster_map()]
#' call. Use them to give workers access to objects (e.g. helper functions or
#' lookup tables) and packages from your session.
#'
#' @param cluster,.cluster Cluster to work on.
#' @param ... Name-value pairs.
#' @param packages Character vector of packages to load.
#' @param names Name of variables to copy or remove.
#' @param env Environment in which to look for variables to copy.
#' @name cluster_utils
#' @return All functions return `cluster` invisibly, so calls can be piped
#'   together.
#' @examples
#' cl <- default_cluster()
#' # Make package functions available inside verbs
#' cluster_library(cl, "dplyr")
#'
#' # Make a value visible inside verbs
#' cluster_assign(cl, a = 10)
#' partition(mtcars, cl) %>% mutate(x = mpg + a)
#'
#' # cluster_copy() is a useful shortcut for existing objects
#' b <- 20
#' cluster_copy(cl, "b")
#'
#' # Values can be removed again
#' cluster_rm(cl, c("a", "b"))
NULL

#' @rdname cluster_utils
#' @export
cluster_assign <- function(.cluster, ...) {
  stopifnot(is_cluster(.cluster))
  values <- list2(...)
  stopifnot(is_named(values))

  .cluster$env$bindings[names(values)] <- lapply(values, mori::share)
  invisible(.cluster)
}

#' @rdname cluster_utils
#' @export
cluster_copy <- function(cluster, names, env = caller_env()) {
  stopifnot(is_cluster(cluster))
  stopifnot(is.character(names))

  cluster_assign(cluster, !!!env_get_list(env, names, inherit = TRUE))
}

#' @rdname cluster_utils
#' @export
cluster_rm <- function(cluster, names) {
  stopifnot(is_cluster(cluster))
  stopifnot(is.character(names))

  cluster$env$bindings <- cluster$env$bindings[setdiff(
    names(cluster$env$bindings),
    names
  )]
  invisible(cluster)
}

#' @rdname cluster_utils
#' @export
cluster_library <- function(cluster, packages) {
  lapply(packages, library, character.only = TRUE)

  cluster$env$packages <- union(cluster$env$packages, packages)
  invisible(cluster)
}
