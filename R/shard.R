#' Partition data across a cluster.
#'
#' @param .data,data Dataset to partition
#' @param ... Variables to partition by. Will generally work best when you
#'   have many more groups than workers. If omitted, will randomly partition
#'   rows across workers.
#' @param .cluster Cluster to use.
#' @export
#' @examples
#' library(dplyr)
#' cl <- new_cluster(2)
#'
#' mtcars2 <- partition(mtcars, .cluster = cl)
#' mtcars2 %>% mutate(cyl2 = 2 * cyl)
#' mtcars2 %>% filter(vs == 1)
#' mtcars2 %>% group_by(cyl) %>% summarise(n())
#' mtcars2 %>% select(-cyl)
partition <- function(.data, ..., .cluster = get_default_cluster()) {
  worker_id <- worker_id(.data, .cluster, ...)
  worker_rows <- split(seq_along(worker_id), worker_id)

  if (length(worker_rows) < length(.cluster)) {
    message("Using partial cluster of size ", length(worker_rows))
    .cluster <- .cluster[seq_along(worker_rows)]
  }
  shards <- lapply(worker_rows, function(i) .data[i, ])

  name <- table_name()
  cluster_assign_each(.cluster, name, shards)
  party_df(name, .cluster)
}

worker_id <- function(.data, .cluster, ...) {
  n <- nrow(.data)
  m <- length(.cluster)

  if (missing(...) && !dplyr::is_grouped_df(.data)) {
    # Randomly assign
    sample(seq_len(n) %% m) + 1
  } else {
    # Assign each new group to the session with fewest rows
    group_id <- dplyr::group_indices(.data, ...)
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

party_df <- function(name, cluster, partition_vars = "PARTITION_ID") {
  stopifnot(is.character(name), length(name) == 1)

  structure(
    list(
      cluster = cluster,
      name = sym(name),
      partition_vars = partition_vars,
      .auto_clean = shard_deleter(name, cluster)
    ),
    class = "party_df"
  )
}

shard_deleter <- function(name, cluster) {
  env <- env()
  reg.finalizer(env, function(...) {
    cluster_rm(cluster, name)
  })
  env
}


shard_rows <- function(x) {
  nrows <- cluster_call(x$cluster, nrow(!!x$name))
  unlist(nrows)
}

shard_cols <- function(x) {
  cluster_call(x$cluster[1], ncol(!!x$name))[[1]]
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.party_df <- function(x) {
  cluster_call(x$cluster[1], tbl_vars(!!x$name))[[1]]
}

#' @importFrom dplyr groups
#' @export
groups.party_df <- function(x) {
  cluster_call(x$cluster[1], dplyr::groups(!!x$name))[[1]]
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
    head_i <- cluster_call(x$cluster[i], head(!!x$name, n = !!left))[[1]]

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

  groups <- groups(x)
  if (length(groups) > 0) {
    groups <- vapply(groups, as.character, character(1))
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
as.data.frame.party_df <- function(x, row.names, optional, ...) {
  dplyr::bind_rows(cluster_get(x$cluster, as.character(x$name)))
}

#' @importFrom dplyr collect
#' @export
collect.party_df <- function(.data, ...) {
  out <- as.data.frame(.data)
  group_by(out, !!!groups(.data))
}

# Methods passed on to shards ---------------------------------------------

#' @importFrom dplyr arrange
#' @export
arrange.party_df <- function(.data, ..., .by_group = FALSE) {
  shard_call(.data, "arrange", enquos(...), .by_group = .by_group)
}

#' @importFrom dplyr filter
# exported on load
filter.party_df <- function(.data, ...) {
  shard_call(.data, "filter", enquos(...))
}

#' @importFrom dplyr group_by
#' @export
group_by.party_df <- function(.data, ..., add = FALSE) {
  shard_call(.data, "group_by", enquos(...), add = add)
}

#' @importFrom dplyr group_by
#' @export
ungroup.party_df <- function(.data, ..., add = FALSE) {
  shard_call(.data, "ungroup", enquos(...), add = add)
}

#' @importFrom dplyr mutate
#' @export
mutate.party_df <- function(.data, ...) {
  shard_call(.data, "mutate", enquos(...))
}

#' @importFrom dplyr select
#' @export
select.party_df <- function(.data, ...) {
  shard_call(.data, "select", enquos(...))
}

#' @importFrom dplyr slice
#' @export
slice.party_df <- function(.data, ...) {
  shard_call(.data, "slice", enquos(...))
}

#' @importFrom dplyr summarise
#' @export
summarise.party_df <- function(.data, ...) {
  shard_call(.data, "summarise", enquos(...))
}

#' @importFrom dplyr do
#' @export
do.party_df <- function(.data, ...) {
  shard_call(.data, "do", enquos(...))
}

shard_call <- function(.data, .verb, dots, ...) {
  call <- call2(.verb, .data$name, !!!dots, ..., .ns = "dplyr")

  new_name <- table_name()
  cluster_assign(.data$cluster, new_name, !!call)
  party_df(new_name, .data$cluster)
}
