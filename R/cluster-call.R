#' Call a function on each node of a cluster
#'
#' `cluster_call()` executes the code on each worker and returns the results;
#' `cluster_send()` executes the code ignoring the result. Jobs are submitted
#' to workers in parallel, and then we wait until they're complete.
#'
#' @param cluster A cluster.
#' @param code An expression to execute on each worker.
#' @param simplify Should the results be simplified from a list?
#'   * `TRUE`: simplify or die trying.
#'   * `NA`: simplify if possible.
#'   * `FALSE`: never try to simplify, always leaving as a list.
#'
#'   `code` must return a vector of length one in order for simplification
#'   to succeed.
#' @param ptype If `simplify` is `TRUE`, use `ptype` to enforce the desired
#'   output type.
#' @export
#' @return A list of results with one element for each worker in `cluster`.
#' @examples
#' cl <- default_cluster()
#'
#' # Run code on each cluster and retrieve results
#' cluster_call(cl, Sys.getpid())
#' cluster_call(cl, runif(1))
#'
#' # use ptype to simplify
#' cluster_call(cl, runif(1), simplify = TRUE)
#'
#' # use cluster_send() to ignore results
#' cluster_send(cl, x <- runif(1))
#' cluster_call(cl, x, simplify = TRUE)
cluster_call <- function(cluster, code, simplify = FALSE, ptype = NULL) {
  stopifnot(is_cluster(cluster))
  code <- enexpr(code)
  to_rm <- attr(cluster, "cleaner")$reset()

  if (length(simplify) > 1 || !is.logical(simplify)) {
    cli::cli_abort("{.arg simplify} must be `TRUE`, `FALSE`, or `NA`.")
  }
  if (!isTRUE(simplify) && !is.null(ptype)) {
    # 0.1.2
    warn("Must now set `simplify = TRUE` when supplying ptype")
    simplify <- TRUE
  }

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
    worker <- which(failed)[[1]]
    err <- errs[[worker]]$parent
    message <- paste0("Remote computation failed in worker ", worker)
    abort(message, parent = err)
  }

  out <- lapply(results, "[[", "result")
  if (!isFALSE(simplify)) {
    out <- simplify_impl(out, strict = !is.na(simplify), ptype = ptype)
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

# TODO: replace with purrr::list_simplify() when purrr 1.0.0 is out
simplify_impl <- function(x,
                          strict = TRUE,
                          ptype = NULL,
                          error_arg = caller_arg(x),
                          error_call = caller_env()) {
  vctrs::vec_check_list(x, arg = error_arg, call = error_call)

  # Handle the cases where we definitely can't simplify
  if (strict) {
    vctrs::list_check_all_vectors(x, arg = error_arg, call = error_call)
    size_one <- vctrs::list_sizes(x) == 1L
    can_simplify <- all(size_one)

    if (!can_simplify) {
      bad <- which(!size_one)[[1]]
      cli::cli_abort(
        c(
          "All elements must be size 1.",
          i = "`{error_arg}[[{bad}]]` is size {vec_size(x[[bad]])}."
        ),
        call = error_call
      )
    }
  } else {
    can_simplify <- vctrs::list_all_vectors(x) && all(vctrs::list_sizes(x) == 1L)

    if (!can_simplify) {
      return(x)
    }
  }

  names <- vctrs::vec_names(x)
  x <- vctrs::vec_set_names(x, NULL)

  out <- tryCatch(
    vctrs::vec_c(!!!x, ptype = ptype),
    vctrs_error_incompatible_type = function(err) {
      if (strict || !is.null(ptype)) {
        cnd_signal(err)
      } else {
        x
      }
    }
  )
  vctrs::vec_set_names(out, names)
}
