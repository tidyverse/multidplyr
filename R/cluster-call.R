#' Call a function on each node of a cluster
#'
#' `cluster_call()` executes the code on each worker and returns the results;
#' `cluster_send()` executes the code ignoring the result. Jobs are submitted
#' to workers in parallel, and then we wait until they're complete.
#'
#' @param cluster A cluster.
#' @param code An expression to execute on each worker.
#' @param ptype Determines the output type. The default returns a list,
#'   which will always succeed. Set to a narrower type to simplify the output.
#' @export
#' @examples
#' cl <- default_cluster()
#'
#' # Run code on each cluster and retrieve results
#' cluster_call(cl, Sys.getpid())
#' cluster_call(cl, runif(1))
#'
#' # use ptype to simplify
#' cluster_call(cl, runif(1), ptype = double())
#'
#' # use cluster_send() to ignore results
#' cluster_send(cl, x <- runif(1))
#' cluster_call(cl, x, ptype = double())
cluster_call <- function(cluster, code, ptype = list()) {
  stopifnot(is_cluster(cluster))
  code <- enexpr(code)
  to_rm <- attr(cluster, "cleaner")$reset()

  # nocov start
  f <- function(code, to_rm) {
    rm(list = to_rm, envir = globalenv())
    eval(code, globalenv())
  }
  # nocov end
  lapply(cluster, function(x) x$call(f, list(code = code, to_rm = to_rm)))
  lapply(cluster, function(x) x$poll_process(-1))

  results <- lapply(cluster, function(x) x$read())

  errs <- lapply(results, "[[", "error")
  failed <- !vapply(errs, is.null, logical(1))
  if (any(failed)) {
    err <- errs[failed][[1]]$parent
    msg <- paste0("Remote computation failed:\n", conditionMessage(err))
    abort(msg) # parent = err$error - only shows trace from worker
  }

  out <- lapply(results, "[[", "result")
  if (!identical(ptype, list())) {
    out <- vctrs::vec_list_cast(out, ptype)
  }
  out
}

#' @rdname cluster_call
#' @export
cluster_send <- function(cluster, code) {
  stopifnot(is_cluster(cluster))
  code <- call2("{", enexpr(code), NULL)
  cluster_call(cluster, !!code)

  invisible(cluster)
}
