#' A cluster.
#'
#' @param cluster Cluster to use as backend.
#' @export
#' @examples
#' library(dplyr)
#' cl <- src_cluster()
#'
#' # Copying a data frame to a src_cluster copies it once to each node.
#' # This isn't usually useful unless you plan on joining it to a data
#' # frame created with partition()
#' copy_to(cl, mtcars)
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

#' @method copy_to src_cluster
#' @importFrom dplyr copy_to
#' @export
copy_to.src_cluster <- function(dest, df, name = deparse(substitute(df)), ...) {
  cluster_assign_value(dest, name, df)
  tbl(dest, name)
}

#' @method tbl src_cluster
#' @importFrom dplyr tbl
#' @export
tbl.src_cluster <- function(src, name, ...) {
  party_df(name, src$cluster)
}
