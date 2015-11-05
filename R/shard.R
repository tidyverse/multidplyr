#' Partition data across a cluster.
#'
#' @param .data Dataset to partition
#' @param cluster Cluster to use.
#' @export
#' @examples
#' library(dplyr)
#' s <- partition(mtcars)
#' s %>% mutate(cyl2 = 2 * cyl)
#' s %>% filter(vs == 1)
#' s %>% summarise(n())
#' s %>% select(-cyl)
partition <- function(.data, cluster = get_default_cluster()) {

  idx <- parallel::splitIndices(nrow(.data), length(cluster))
  shards <- lapply(idx, function(i) .data[i, , drop = FALSE])

  name <- random_table_name()
  cluster_assign_each(cluster, name, shards)

  party_df(name, cluster)
}

party_df <- function(name, cluster) {
  structure(
    list(
      cluster = cluster,
      name = name,
      deleter = shard_deleter(name, cluster)
    ),
    class = "party_df"
  )
}

shard_deleter <- function(name, cluster) {
  reg.finalizer(environment(), function(...) {
    cluster_rm(cluster, name)
  })
  environment()
}


shard_rows <- function(x) {
  call <- substitute(nrow(x), list(x = as.name(x$name)))
  nrows <- cluster_eval_(x, call)

  unlist(nrows)
}

shard_cols <- function(x) {
  call <- substitute(ncol(x), list(x = as.name(x$name)))
  cluster_eval_(x$cluster[1], call)[[1]]
}

#' @export
dim.party_df <- function(x) {
  c(sum(shard_rows(x)), shard_cols(x))
}

#' @importFrom utils head
#' @export
head.party_df <- function(x, n = 6L, ...) {
  pieces <- vector("list", length(x$cluster))
  left <- n

  # Work cluster by cluster until we have enough rows
  for (i in seq_along(x$cluster)) {
    call <- substitute(head(x, n), list(x = as.name(x$name), n = left))
    head_i <- cluster_eval_(x$cluster[i], call)[[1]]

    pieces[[i]] <- head_i
    left <- left - nrow(head_i)
    if (left == 0)
      break
  }

  dplyr::bind_rows(pieces)
}

#' @export
print.party_df <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: sharded data frame ", dplyr::dim_desc(x), "\n", sep = "")

  shards <- shard_rows(x)
  cat("Shards: ", length(shards), " [", min(shards), "--", max(shards),
    " rows]\n", sep = "")
  cat("\n")
  print(dplyr::trunc_mat(x, n = n, width = width))

  invisible(x)
}

#' @export
as.data.frame.party_df <- function(x, row.names, optional, ...) {
  dplyr::bind_rows(cluster_get(x, x$name))
}

#' @importFrom dplyr collect
#' @method collect party_df
#' @export
collect.party_df <- function(.data, ...) {
  as.data.frame(.data)
}

# Methods passed on to shards ---------------------------------------------

#' @importFrom dplyr mutate_
#' @method mutate_ party_df
#' @export
mutate_.party_df <- function(.data, ..., .dots = list()) {
  shard_call(.data, quote(dplyr::mutate), ..., .dots = .dots)
}

#' @importFrom dplyr filter_
#' @method filter_ party_df
#' @export
filter_.party_df <- function(.data, ..., .dots = list()) {
  shard_call(.data, quote(dplyr::filter), ..., .dots = .dots)
}

#' @importFrom dplyr summarise_
#' @method summarise_ party_df
#' @export
summarise_.party_df <- function(.data, ..., .dots = list()) {
  shard_call(.data, quote(dplyr::summarise), ..., .dots = .dots)
}

#' @importFrom dplyr select_
#' @method select_ party_df
#' @export
select_.party_df <- function(.data, ..., .dots = list()) {
  shard_call(.data, quote(dplyr::select), ..., .dots = .dots)
}

#' @importFrom dplyr group_by_
#' @method group_by_ party_df
#' @export
group_by_.party_df <- function(.data, ..., .dots = list()) {
  shard_call(.data, quote(dplyr::group_by), ..., .dots = .dots)
}

shard_call <- function(df, fun, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  call <- lazyeval::make_call(fun, c(list(df$name), dots))

  new_name <- random_table_name()
  cluster_assign_expr(df, new_name, call$expr)
  party_df(new_name, df$cluster)
}
