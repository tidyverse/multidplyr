#' Create a new cluster with sensible defaults.
#'
#' Clusters created with this function will automatically clean up after
#' themselves.
#'
#' @param n Number of workers to create. Avoid setting this higher than the
#'   number of cores in your computer as it will degrade performance.
#' @export
#' @examples
#' cluster <- new_cluster(2)
#' cluster
new_cluster <- function(n) {
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

#' Cluster management.
#'
#' Setting up a cluster is relatively expensive, so it's best to use a single
#' cluster throughout a session. These functions maintain a default for the
#' session and are used in examples and tests.
#'
#' @name default_cluster
#' @param n Number of workers to use; defaults to 2 because this is the maximum
#'   allowed by CRAN.
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
get_default_cluster <- function(n = 2) {
  if (!env_has(cluster_env, "cluster")) {
    message("Initiating default cluster of size ", n)
    set_default_cluster(new_cluster(n))
  } else {
    if (!missing(n)) {
      abort("`n` ignored; cluster has already been initiated")
    }
  }

  cluster_env$cluster
}
