#' Create a new cluster with sensible defaults.
#'
#' Clusters created with this function will automatically clean up after
#' themselves.
#'
#' @param n Number of workers to create. If `NULL`, it will use at least
#'   two, but no more than the number of cores on your computer - 2.
#' @export
#' @examples
#' cluster <- new_cluster(2)
#' cluster
new_cluster <- function(n = NULL) {
  if (is.null(n)) {
    n <- guess_cores()
    message("Initialising ", n, " core cluster.")
  }

  sessions <- replicate(n, callr::r_session$new())
  structure(sessions, class = "multidplyr_cluster")
}

#' @export
print.multidplyr_cluster <- function(x, ...) {
  n <- length(x)

  state <- vapply(x, function(x) x$get_state(), character(1))
  state_abbr <- c(
    "starting" = "S",
    "idle" = crayon::green("."),
    "busy" = crayon::red("*"),
    "finished" = "F"
  )[state]

  cat_line(n, " session cluster [", paste(state_abbr, collapse = ""), "]")
}

is_cluster <- function(x) inherits(x, "multidplyr_cluster")

#' @export
`[.multidplyr_cluster` <- function(x, i, ...) {
  structure(NextMethod(), class = "multidplyr_cluster")
}

guess_cores <- function() {
  if (in_check()) {
    return(2L)
  }

  pmax(2L, ps::ps_cpu_count() - 2L)
}

in_check <- function() {
  paths <- strsplit(getwd(), "/", fixed = TRUE)[[1]]
  any(grepl("Rcheck$", paths))
}

#' Cluster management.
#'
#' This manages a default cluster for the session, creating one the first
#' time it is called.
#'
#' @name default_cluster
#' @keywords internal
#' @examples
#' get_default_cluster()
NULL

cluster_env <- env()

#' @export
#' @rdname default_cluster
#' @param x New cluster to use as default.
set_default_cluster <- function(x) {
  stopifnot(is_cluster(x))

  cluster_env$cluster <- x
  invisible(x)
}

#' @export
#' @rdname default_cluster
get_default_cluster <- function(...) {
  if (!env_has(cluster_env, "cluster")) {
    set_default_cluster(new_cluster(...))
  }

  cluster_env$cluster
}
