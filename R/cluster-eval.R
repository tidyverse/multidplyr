#' Evaluate arbitrary code on each node
#'
#' @inheritParams objman
#' @param expr Expression to evaluate. \code{cluster_eval} uses non-standard
#'   evaluation to automatically quote. For \code{cluster_eval_}, you must
#'   quote it yourself.
#' @export
#' @examples
#' cl <- create_cluster(2)
#' cl %>% cluster_eval(1 + 1)
#' cl %>% cluster_eval_(quote(1 + 1))
cluster_eval <- function(cluster, expr, env = globalenv()) {
  cluster_call(cl, eval, substitute(expr), env = env)
}

#' @export
#' @rdname cluster_eval
cluster_eval_ <- function(cluster, expr, env = globalenv()) {
  stopifnot(is.atomic(cluster) || is.name(expr) || is.call(expr))
  cluster_call(cl, eval, expr, env = env)
}
