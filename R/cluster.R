#' Create a new cluster with sensible defaults.
#'
#' Clusters created with this function will automatically clean up after
#' themselves.
#'
#' @param n Number of workers to create. Avoid setting this higher than the
#'   number of cores in your computer as it will degrade performance.
#' @export
#' @returns A `multidplyr_cluster` object.
#' @examples
#' cluster <- new_cluster(2)
#' cluster
new_cluster <- function(n) {
  sessions <- replicate(n, callr::r_session$new(wait_timeout = 15 * 1000))
  structure(
    sessions,
    cleaner = Cleaner$new(),
    class = "multidplyr_cluster"
  )
}

#' @importFrom R6 R6Class
Cleaner <- R6Class(
  "Cleaner",
  list(
    names = character(),
    add = function(x) {
      self$names <- union(self$names, x)
      invisible(self)
    },
    reset = function(x) {
      old <- self$names
      self$names <- character()
      old
    }
  )
)

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
  structure(
    NextMethod(),
    cleaner = attr(x, "cleaner"),
    class = "multidplyr_cluster"
  )
}

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
