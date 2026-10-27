#' Map a function over inputs on the cluster
#'
#' `cluster_map()` evaluates `.f` on each element of `.x` on the cluster's
#' workers, balancing the work dynamically. One use is building the shards of
#' a [party_df()] in parallel, e.g. reading one file per shard.
#'
#' @param cluster A cluster.
#' @param .x A vector or list of inputs; `.f` is called once per element.
#' @param .f A function applied as `.f(.x[[i]], ...)`. It is serialised to
#'   the workers with its enclosing environment, so keep that small. Objects
#'   it needs from your session can be registered with [cluster_assign()] or
#'   [cluster_copy()], and packages with [cluster_library()].
#' @param ... Additional arguments passed to `.f`.
#' @return A list of results in input order.
#' @export
#' @examples
#' cl <- default_cluster()
#' cluster_map(cl, 1:4, function(x) x^2)
#'
#' # Read one file per shard, then bind into a party_df
#' \dontrun{
#' shards <- cluster_map(cl, files, vroom::vroom)
#' df <- party_df(cl, shards)
#' }
cluster_map <- function(cluster, .x, .f, ...) {
  stopifnot(is_cluster(cluster))
  map <- mizu_map_muffled(
    cluster$pool,
    .x = .x,
    .f = cluster_map_eval,
    f = .f,
    args = list(...),
    bindings = cluster_bindings(cluster),
    packages = cluster_packages(cluster)
  )
  collect_map(map)
}
