#' A cluster.
#'
#' @param cluster Cluster to use as backend.
#' @export
#' @examples
#' src_cluster()
src_cluster <- function(cluster = get_default_cluster()) {
  stopifnot(inherits(cluster, "cluster"))

  structure(list(cluster = cluster), class = c("src_cluster", "src"))
}

#' @export
#' @method src_tbls src_cluster
#' @importFrom dplyr src_tbls
src_tbls.src_cluster <- function(x, ...) {
  vars <- cluster_ls(x)
  Reduce(intersect, vars)
}

#' @export
#' @method format src_cluster
format.src_cluster <- function(x, ...) {
  paste0("src:  ", class(x)[1], " with ", length(x$cluster), " nodes\n",
    wrap("tbls: ", paste0(sort(src_tbls(x)), collapse = ", ")))
}
