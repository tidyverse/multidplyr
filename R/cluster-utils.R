#' Cluster utitility functions
#'
#' These functions provide useful helpers for performaning common operations.
#'
#' @param cluster Cluster to work on
#' @param name,names Name of variable or variables, as strings.
#' @param expr An expression to evaluate.
#' @param values A list of values, one for each worker.
#' @param packages Character vector of packages to load
#' @name cluster_utils
#' @return All functions that modify the worker environment invisibly return
#'   the cluster so they can chain them together. The other functions return
#'   lists with with one element for each worker.
#' @examples
#' cl <- default_cluster(2)
#' cl %>%
#'   cluster_assign("x", runif(1)) %>%
#'   cluster_get("x")
#'
#' # Inline the expression using !! if you want to compute it locally
#' cl %>%
#'   cluster_assign("y", !!runif(1)) %>%
#'   cluster_get("x")
#'
#' cl %>%
#'   cluster_assign_each("z", list(3, 4)) %>%
#'   cluster_get("z")
#'
#' cl %>% cluster_ls()
#'
#' w <- 10
#' cluster_copy(cl, "w")
#'
#' cl %>% cluster_rm(c("w", "x", "y", "z"))
#' cl %>% cluster_ls()
#'
#' cl %>% cluster_call(search())
#'
#' cl %>%
#'   cluster_library("magrittr") %>%
#'   cluster_call(search())
NULL

#' @rdname cluster_utils
#' @export
cluster_assign <- function(cluster, name, expr) {
  stopifnot(is_cluster(cluster))
  stopifnot(is_string(name))
  expr <- enexpr(expr)

  assign_call <- call2("<-", sym(name), expr)
  cluster_call(cluster, !!call2("{", assign_call, NULL))
  invisible(cluster)
}

#' @rdname cluster_utils
#' @export
cluster_assign_each <- function(cluster, name, values) {
  stopifnot(is_cluster(cluster))
  stopifnot(is_string(name))
  stopifnot(is.list(values), length(values) == length(cluster))

  path <- tempfile()
  on.exit(unlink(path), add = TRUE)

  for (i in seq_along(values)) {
    qs::qsave(values[[i]], path, preset = "fast", check_hash = FALSE, nthreads = 2)
    assign_call <- call2("<-", sym(name), expr(qs::qread(!!path, nthreads = 2)))

    cluster_call(cluster[i], !!call2("{", assign_call, NULL))
  }

  invisible(cluster)
}

#' @rdname cluster_utils
#' @param env Environment in which to look for varibles to copy.
#' @export
cluster_copy <- function(cluster, names, env = caller_env()) {
  stopifnot(is_cluster(cluster))
  stopifnot(is.character(names))

  path <- tempfile()
  on.exit(unlink(path), add = TRUE)

  values <- lapply(names, env_get, env = env, inherit = TRUE)
  names(values) <- names
  qs::qsave(values, path, preset = "fast", check_hash = FALSE, nthreads = 2)

  assign_call <- expr(list2env(qs::qread(!!path), globalenv()))
  cluster_call(cluster, !!call2("{", assign_call, NULL))

  invisible(cluster)
}

#' @rdname cluster_utils
#' @export
cluster_get <- function(cluster, name) {
  stopifnot(is_cluster(cluster))
  stopifnot(is_string(name))
  cluster_call(cluster, !!sym(name))
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
