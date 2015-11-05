#' Call a function on each node of a cluster
#'
#' @section S3:
#'
#' This is an S3 generic, and is the only method that \pkg{dparty} needs
#' in order to use a cluster.
#'
#' @param .cl A cluster
#' @param .fun Function to call. Either a string or function. If a function,
#'   will be copied to remote nodes.
#' @param ... Arguments to fun
#' @return A list, with one element for each node.
#' @export
#' @examples
#' c <- get_default_cluster()
#' cluster_call(c, "ls", envir = globalenv())
cluster_call <- function(.cl, .fun, ...) {
  UseMethod("cluster_call")
}

#' @export
cluster_call.cluster <- function(.cl, .fun, ...) {
  parallel::clusterCall(cl = .cl, fun = .fun, ...)
}

#' @export
cluster_call.party_df <- function(.cl, .fun, ...) {
  cluster_call(.cl = .cl$cluster, .fun = .fun, ...)
}
