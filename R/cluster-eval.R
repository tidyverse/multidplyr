#' Evaluate arbitrary code on each node
#'
#' Note that you can only evaluate in the global environment: it doesn't
#' seem to make much sense to evaluate elsewhere on remote nodes.
#'
#' @inheritParams objman
#' @param expr Expression to evaluate. \code{cluster_eval} uses non-standard
#'   evaluation to automatically quote. For \code{cluster_eval_}, you must
#'   quote it yourself.
#' @export
#' @examples
#' cl <- get_default_cluster()
#' cl %>% cluster_eval(1 + 1)
#' cl %>% cluster_eval_(quote(1 + 1))
cluster_eval <- function(cluster, expr) {
  cluster_eval_(cluster, substitute(expr))
}

#' @export
#' @rdname cluster_eval
cluster_eval_ <- function(cluster, expr) {
  stopifnot(is.atomic(expr) || is.name(expr) || is.call(expr))
  cluster_call(cluster, eval, expr)
}
