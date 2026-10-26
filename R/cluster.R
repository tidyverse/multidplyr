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
  # Each worker gets its own single-daemon compute profile so that we can
  # target individual workers and their global environments persist.
  ids <- paste0("multidplyr_", cluster_id(), "_", seq_len(n))
  # Set up all profiles before launching so workers start in parallel
  for (id in ids) {
    mirai::daemons(url = mirai::local_url(), dispatcher = FALSE, .compute = id)
  }
  for (id in ids) {
    mirai::launch_local(1, cleanup = FALSE, .compute = id)
  }

  cleaner <- Cleaner$new()
  reg.finalizer(cleaner, function(e) stop_workers(ids), onexit = TRUE)

  structure(
    as.list(ids),
    cleaner = cleaner,
    class = "multidplyr_cluster"
  )
}

cluster_id <- function() {
  cluster_env$n <- (cluster_env$n %||% 0L) + 1L
  paste0(Sys.getpid(), "_", cluster_env$n)
}

stop_workers <- function(ids) {
  for (id in ids) {
    try(mirai::daemons(0, .compute = id), silent = TRUE)
  }
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

  # Calls are synchronous, so a connected worker is always idle here
  connected <- vapply(x, worker_connected, logical(1))
  state_abbr <- ifelse(connected, crayon::green("."), "F")

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

worker_connected <- function(id) {
  info <- tryCatch(mirai::info(.compute = id), error = function(e) NULL)
  !is.null(info) && isTRUE(info[["connections"]] > 0)
}

cluster_env <- env()
