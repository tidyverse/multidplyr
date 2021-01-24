#' Partition data across a cluster.
#'
#' Partitioning ensures that all observations in a group end up on the same
#' worker. To try and keep the observations on each worker balanced,
#' `partition()` uses a greedy algorithm iteratively assigning each group to
#' the worker with the fewest observations.
#'
#' @param data Dataset to partition, typically grouped. When grouped, all
#'   observations in a group will be assigned to the same cluster.
#' @param cluster Cluster to use.
#' @export
#' @examples
#' library(dplyr)
#' cl <- default_cluster()
#'
#' mtcars2 <- partition(mtcars, cl)
#' mtcars2 %>% mutate(cyl2 = 2 * cyl)
#' mtcars2 %>% filter(vs == 1)
#' mtcars2 %>% group_by(cyl) %>% summarise(n())
#' mtcars2 %>% select(-cyl)
partition <- function(data, cluster) {
  worker_id <- worker_id(data, cluster)
  worker_rows <- split(seq_along(worker_id), worker_id)

  if (length(worker_rows) < length(cluster)) {
    message("Using partial cluster of size ", length(worker_rows))
    cluster <- cluster[seq_along(worker_rows)]
  }
  shards <- lapply(worker_rows, function(i) data[i, , drop = FALSE])

  name <- table_name()
  cluster_assign_each(cluster, !!name := shards)
  new_party_df(cluster, name, auto_rm = TRUE)
}

worker_id <- function(data, cluster) {
  n <- nrow(data)
  m <- length(cluster)

  if (!dplyr::is_grouped_df(data)) {
    # Assign sequentially
    (seq_len(n) - 1) %% m + 1
  } else {
    # Assign each new group to the session with fewest rows
    group_id <- dplyr::group_indices(data)
    counts <- tabulate(group_id)

    rows <- integer(m)
    group_worker_id <- integer(length(counts))

    for (i in seq_along(counts)) {
      j <- which.min(rows)
      group_worker_id[[i]] <- j
      rows[[j]] <- rows[[j]] + counts[[i]]
    }

    group_worker_id[group_id]
  }
}

# Constructor -------------------------------------------------------------

#' Create party_df "by hand"
#'
#' Use this function to create a partitioned data frame from existing
#' data frames spread across a cluster
#'
#' @export
#' @param cluster A cluster
#' @param name Name of data frame variable. Must exist on every worker,
#'   be a data frame, and have the same names.
#' @param auto_rm If `TRUE`, will automatically `rm()` the data frame on
#'   the workers when this object is created.
#' @export
#' @examples
#' # If a real example, you might spread file names across the clusters
#' # and read in using data.table::fread()/vroom::vroom()/qs::qread().
#' cl <- default_cluster()
#' cluster_send(cl[1], n <- 10)
#' cluster_send(cl[2], n <- 15)
#' cluster_send(cl, df <- data.frame(x = runif(n)))
#'
#' df <- party_df(cl, "df")
#' df
party_df <- function(cluster, name, auto_rm = FALSE) {
  stopifnot(is_cluster(cluster))
  stopifnot(is_string(name))

  # Check that variable exists, is data frame, and has same names
  exists <- unlist(cluster_call(cluster, exists(!!name)))
  if (!all(exists)) {
    abort(paste0("`", name, "` does not exist on all workers"))
  }

  is_df <- unlist(cluster_call(cluster, is.data.frame(!!sym(name))))
  if (!all(is_df)) {
    abort(paste0("`", name, "` is not a data frame on all workers"))
  }

  names <- cluster_call(cluster, names(!!sym(name)))
  if (length(unique(names)) != 1) {
    abort(paste0("`", name, "` does not have the same names on all workers"))
  }

  new_party_df(cluster, name, auto_rm = auto_rm)
}

new_party_df <- function(cluster, name, auto_rm) {
  stopifnot(is_cluster(cluster))
  stopifnot(is_string(name))

  structure(
    list(
      cluster = cluster,
      name = sym(name),
      .auto_clean = shard_deleter(auto_rm, name, cluster)
    ),
    class = "multidplyr_party_df"
  )
}

is_party_df <- function(x) inherits(x, "multidplyr_party_df")

shard_deleter <- function(auto_rm, name, cluster) {
  if (!auto_rm) {
    return(NULL)
  }

  env <- new_environment()
  reg.finalizer(env, function(...) attr(cluster, "cleaner")$add(name))
  env
}

shard_rows <- function(x) {
  nrows <- cluster_call(x$cluster, nrow(!!x$name))
  unlist(nrows)
}

shard_cols <- function(x) {
  cluster_call(x$cluster[1], ncol(!!x$name))[[1]]
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.multidplyr_party_df <- function(x) {
  character()
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.multidplyr_party_df <- function(x) {
  cluster_call(x$cluster[1], dplyr::tbl_vars(!!x$name))[[1]]
}

#' @importFrom dplyr group_vars
#' @export
group_vars.multidplyr_party_df <- function(x) {
  cluster_call(x$cluster[1], dplyr::group_vars(!!x$name))[[1]]
}

#' @export
dim.multidplyr_party_df <- function(x) {
  c(sum(shard_rows(x)), shard_cols(x))
}

#' @importFrom utils head
#' @export
head.multidplyr_party_df <- function(x, n = 6L, ...) {
  pieces <- vector("list", length(x$cluster))
  left <- n

  # Work cluster by cluster until we have enough rows
  for (i in seq_along(x$cluster)) {
    head_i <- cluster_call(x$cluster[i], head(!!x$name, n = !!left))[[1]]

    pieces[[i]] <- head_i
    left <- left - nrow(head_i)
    if (left == 0)
      break
  }

  dplyr::bind_rows(pieces)
}

#' @export
print.multidplyr_party_df <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: party_df ", dplyr::dim_desc(x), "\n", sep = "")

  groups <- group_vars(x)
  if (length(groups) > 0) {
    cat("Groups: ", paste0(groups, collapse = ", "), "\n", sep = "")
  }

  shards <- shard_rows(x)
  cat("Shards: ", length(shards),
    " [", big_mark(min(shards)), "--", big_mark(max(shards)), " rows]\n",
    sep = "")
  cat("\n")
  print(tibble::trunc_mat(x, n = 6, width = width))

  invisible(x)
}

#' @export
as.data.frame.multidplyr_party_df <- function(x, row.names, optional, ...) {
  dplyr::bind_rows(cluster_call(x$cluster, !!x$name))
}

#' @importFrom dplyr collect
#' @export
collect.multidplyr_party_df <- function(.data, ...) {
  out <- as.data.frame(.data)
  dplyr::grouped_df(out, group_vars(.data))
}

#' @importFrom dplyr pull
#' @export
pull.multidplyr_party_df <- function(.data, var = -1) {
  expr <- enquo(var)
  var <- dplyr:::find_var(expr, tbl_vars(.data))

  .data <- ungroup(.data)
  .data <- select(.data, !!sym(var))
  .data <- collect(.data)
  .data[[1]]
}
