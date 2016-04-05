#' Partition data across a cluster.
#'
#' @param .data Dataset to partition
#' @param ... Variables to partition by. Will generally work best when you
#'   have many more groups than nodes. If omitted, will randomly partition
#'   rows across nodes.
#' @param cluster Cluster to use. If NULL, will choose the number of nodes to be
#'   the minimum of the number of groups and the number of cores - 1.
#' @export
#' @examples
#' library(dplyr)
#' s <- partition(mtcars)
#' s %>% mutate(cyl2 = 2 * cyl)
#' s %>% filter(vs == 1)
#' s %>% summarise(n())
#' s %>% select(-cyl)
#'
#' if (require("nycflights13")) {
#' planes <- partition(flights, tailnum)
#' summarise(planes, n())
#'
#' month <- partition(flights, month)
#' month %>% group_by(day) %>% summarise(n())
#' }
partition <- function(.data, ..., cluster = NULL) {
  dots <- lazyeval::lazy_dots(...)
  partition_(.data, dots, cluster)
}

partition_ <- function(data, groups, cluster = NULL) {
  n <- nrow(data)

  if (length(groups) == 0) {
    if (is.null(cluster)) {
      cluster <- get_default_cluster()
    }
    m <- length(cluster)
    part_id <- sample(floor(m * (seq_len(n) - 1) / n + 1))
    n_groups <- m

    data$PARTITION_ID <- part_id
    data <- dplyr::group_by_(data, ~PARTITION_ID)
    group_vars <- list(quote(PARTITION_ID))
  } else {
    group_vars <- grouping_vars(groups)

    data <- dplyr::group_by_(data, .dots = groups)
    group_id <- dplyr::group_indices_(data)
    n_groups <- dplyr::n_groups(data)

    groups <- scramble_rows(dplyr::data_frame(
      id = seq_len(n_groups),
      n = tabulate(group_id, n_groups)
    ))

    if (is.null(cluster)) {
      m <- min(guess_cores(), n_groups)
      cluster <- create_cluster(m)
    } else {
      m <- length(cluster)
    }

    groups <- dplyr::arrange_(groups, ~n)
    groups$part_id <- floor(m * (cumsum(groups$n) - 1) / sum(groups$n) + 1)
    part_id <- groups$part_id[match(group_id, groups$id)]
  }

  idx <- split(seq_len(n), part_id)
  shards <- lapply(idx, function(i) data[i, , drop = FALSE])

  name <- random_table_name()
  cluster_assign_each(cluster, name, shards)

  party_df(name, cluster, group_vars)
}

grouping_vars <- function(vars) {

  is_name <- vapply(vars, function(x) is.name(x$expr), logical(1))
  if (any(!is_name)) {
    stop("All partition vars must already exist", call. = FALSE)
  }

  lapply(vars, `[[`, "expr")
}

party_df <- function(name, cluster, partition = list(), groups = partition) {
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(is.list(groups))

  structure(
    list(
      cluster = cluster,
      name = name,
      partitions = partition,
      groups = groups,
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
  cat("Source: party_df ", dplyr::dim_desc(x), "\n", sep = "")

  if (length(x$groups) > 0) {
    groups <- vapply(x$groups, as.character, character(1))
    cat("Groups: ", paste0(groups, collapse = ", "), "\n", sep = "")
  }

  shards <- shard_rows(x)
  cat("Shards: ", length(shards),
    " [", big_mark(min(shards)), "--", big_mark(max(shards)), " rows]\n",
    sep = "")
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
  group_by_(as.data.frame(.data), .dots = c(.data$partitions, .data$groups))
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
  shard_call(.data, quote(dplyr::summarise), ..., .dots = .dots,
    groups = .data$groups[-length(.data$groups)])
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
group_by_.party_df <- function(.data, ..., .dots = list(), add = FALSE) {
  dots <- lazyeval::all_dots(.dots, ...)
  groups <- c(.data$partition, grouping_vars(dots))

  shard_call(.data, quote(dplyr::group_by), .dots = groups, groups = groups)
}

#' @importFrom dplyr do_
#' @method do_ party_df
#' @export
do_.party_df <- function(.data, ..., .dots = list()) {
  shard_call(.data, quote(dplyr::do), ..., .dots = .dots)
}

#' @importFrom dplyr rename_
#' @method rename_ party_df
#' @export
rename_.party_df <- function(.data, ..., .dots = list()) {
  shard_call(.data, quote(dplyr::rename), ..., .dots = .dots)
}

shard_call <- function(df, fun, ..., .dots, groups = df$partition) {
  dots <- lazyeval::all_dots(.dots, ...)
  call <- lazyeval::make_call(fun, c(list(df$name), dots))

  new_name <- random_table_name()
  cluster_assign_expr(df, new_name, call$expr)
  party_df(new_name, df$cluster, df$partition, groups)
}

scramble_rows <- function(df) {
  df[sample(nrow(df)), , drop = FALSE]
}
