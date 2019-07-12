#' Call a function on each node of a cluster
#'
#' `cluster_map()` passes a function and arguments; `cluster_call()` quotes
#' the input and then re-calls. Jobs are submitted to workers in parallel, and
#' then we wait until they're complete.
#'
#' @param x,.x A cluster
#' @param .f Function to call. Must be a function, string, or formula.
#'    * If a **function**, it will be copied to each worke. Must be
#'      self-contained because its environment will be set to the global
#'      environment prior to being distributed to the workers.
#'
#'    * If a **string**, the function will not be copied, and a function with
#'      that name will be called on each worker. Can use `::`.
#'
#'    * If a **formula**, e.g. `~ .x + 2`, it is converted to a function.
#' @param ... Arguments to .f. Eagerly evaluated before distribution.
#' @param code An expression to execute on each worker.
#' @return A list, with one element for each worker.
#' @export
#' @examples
#' cl <- new_cluster(2)
#'
#' # This function will be copied to each node
#' f <- function() 1 + 1
#' cl %>% cluster_map(f)
#'
#' # The function will be called on each node
#' cl %>% cluster_map("Sys.getpid")
#'
#' # cluster_call() provides a slightly simpler sytanx where you just
#' # provide an expression to be executed on each worke
#' cl %>% cluster_call(1 + 1)
#' cl %>% cluster_call(Sys.getpid())
#'
#' invisible(cl %>% cluster_call(x <- runif(1)))
#' cl %>% cluster_call(x)
cluster_map <- function(.x, .f, ...) {
  stopifnot(is_cluster(.x))
  .f <- as_function(.f)
  args <- list(...)

  lapply(.x, function(x) x$call(.f, args))
  lapply(.x, function(x) x$poll_process(-1))

  results <- lapply(.x, function(x) x$read())

  errs <- lapply(results, "[[", "error")
  failed <- !vapply(errs, is.null, logical(1))
  if (any(failed)) {
    err <- errs[failed][[1]]
    abort("Computation failed", parent = err)
  }

  lapply(results, "[[", "result")
}

#' @rdname cluster_map
#' @export
cluster_call <- function(x, code) {
  code <- enexpr(code)
  cluster_map(x, function(x) eval(x, globalenv()), code)
}

# helpers -----------------------------------------------------------------

as_function <- function(f, env = caller_env()) {
  if (is_string(f)) {
    new_function(exprs(... = ), call2(parse_expr(f), quote(...)))
  } else {
    rlang::as_function(f, env = env)
  }
}
