#' Shard data across a cluster.
#'
#' @examples
#' s <- shard(mtcars)
#' s %>% mutate(cyl2 = 2 * cyl)
#' s %>% filter(vs == 1)
#' s %>% summarise(n())
#' s %>% select(-cyl)
shard <- function(.data, cluster = cluster_get()) {

  idx <- parallel::splitIndices(nrow(.data), length(cluster))
  shards <- lapply(idx, function(i) .data[i, , drop = FALSE])

  name <- random_table_name()
  cluster_assign_each(name, shards, .cl = cluster)

  sharded_df(name, cluster)
}

sharded_df <- function(name, cluster) {
  structure(
    list(
      cluster = cluster,
      name = name,
      deleter = shard_deleter(name, cluster)
    ),
    class = "sharded_df"
  )
}

shard_deleter <- function(name, cluster) {
  reg.finalizer(environment(), function(...) {
    cluster_rm(name, .cl = cluster)
  })
  environment()
}


shard_rows <- function(x) {
  call <- substitute(nrow(x), list(x = as.name(x$name)))
  nrows <- cluster_eval(call, .cl = x$cluster)

  unlist(nrows)
}

shard_cols <- function(x) {
  call <- substitute(ncol(x), list(x = as.name(x$name)))
  cluster_eval(call, .cl = x$cluster[1])[[1]]
}

#' @export
dim.sharded_df <- function(x) {
  c(sum(shard_rows(x)), shard_cols(x))
}

#' @export
head.sharded_df <- function(x, n = 6L, ...) {
  pieces <- vector("list", length(x$cluster))
  left <- n

  # Work cluster by cluster until we have enough rows
  for (i in seq_along(x$cluster)) {
    call <- substitute(head(x, n), list(x = as.name(x$name), n = left))
    head_i <- cluster_eval(call, .cl = x$cluster[i])[[1]]

    pieces[[i]] <- head_i
    left <- left - nrow(head_i)
    if (left == 0)
      break
  }

  bind_rows(pieces)
}

#' @export
print.sharded_df <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: sharded data frame ", dim_desc(x), "\n", sep = "")

  shards <- shard_rows(x)
  cat("Shards: ", length(shards), " [", min(shards), "--", max(shards),
    " rows]\n", sep = "")
  cat("\n")
  print(trunc_mat(x, n = n, width = width))

  invisible(x)
}

#' @export
as.data.frame.sharded_df <- function(.data, ...) {
  bind_rows(cluster_retrieve(.data$name, .cl = .data$cluster))
}

#' @export
collect.sharded_df <- function(.data, ...) {
  as.data.frame(.data)
}


# Methods passed on to shards ---------------------------------------------

#' @export
mutate_.sharded_df <- function(.data, ..., .dots = list()) {
  shard_call(.data, quote(dplyr::mutate), ..., .dots = .dots)
}

#' @export
filter_.sharded_df <- function(.data, ..., .dots = list()) {
  shard_call(.data, quote(dplyr::filter), ..., .dots = .dots)
}

#' @export
summarise_.sharded_df <- function(.data, ..., .dots = list()) {
  shard_call(.data, quote(dplyr::summarise), ..., .dots = .dots)
}

#' @export
select_.sharded_df <- function(.data, ..., .dots = list()) {
  shard_call(.data, quote(dplyr::select), ..., .dots = .dots)
}

#' @export
group_by_.sharded_df <- function(.data, ..., .dots = list()) {
  shard_call(.data, quote(dplyr::group_by), ..., .dots = .dots)
}

shard_call <- function(df, fun, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  call <- lazyeval::make_call(fun, c(list(df$name), dots))

  new_name <- random_table_name()
  cluster_assign_expr(new_name, call$expr, .cl = df$cluster)
  sharded_df(new_name, df$cluster)
}
