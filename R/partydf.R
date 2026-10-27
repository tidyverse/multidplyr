#' Partition data across workers in a cluster
#'
#' Partitioning ensures that all observations in a group end up in the same
#' shard. To keep the shards balanced, `partition()` uses a greedy algorithm
#' that iteratively assigns each group to the shard that currently has the
#' fewest rows.
#'
#' The data is shared with the workers once (zero copy, via
#' [mori::share()]) and sliced into shards in parallel on the workers, so
#' `partition()` performs no data copies in the host session.
#'
#' @param data Dataset to partition, typically grouped. When grouped, all
#'   observations in a group will be assigned to the same shard.
#' @param cluster Cluster to use.
#' @param n_shards Number of shards to create. The default creates up to 4
#'   shards per worker so that the pool can balance load dynamically.
#' @returns A [party_df].
#' @export
#' @examples
#' library(dplyr)
#' cl <- default_cluster()
#' cluster_library(cl, "dplyr")
#'
#' mtcars2 <- partition(mtcars, cl)
#' mtcars2 %>% mutate(cyl2 = 2 * cyl)
#' mtcars2 %>% filter(vs == 1)
#' mtcars2 %>% group_by(cyl) %>% summarise(n())
#' mtcars2 %>% select(-cyl)
partition <- function(data, cluster, n_shards = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  stopifnot(is_cluster(cluster))
  if (is.null(n_shards)) {
    n_shards <- max(1L, min(nrow(data), 4L * length(cluster)))
  } else {
    if (length(n_shards) != 1L || !is.numeric(n_shards) || n_shards < 1) {
      cli::cli_abort("{.arg n_shards} must be a positive number.")
    }
    n_shards <- as.integer(n_shards)
  }

  rows <- shard_bins(data, n_shards)
  groups <- dplyr::group_vars(data)

  # Share once, then slice in parallel on the workers. `shared` must stay
  # alive in this frame until the workers have mapped it.
  shared <- mori::share(dplyr::ungroup(data))
  map <- mizu_map_muffled(
    cluster$pool,
    .x = seq_along(rows),
    .f = shard_slice,
    data = shared,
    rows = rows
  )
  shards <- collect_map(map)

  new_party_df(cluster, shards, groups)
}

# Row-index assignment only: a list of integer vectors, one per shard
shard_bins <- function(data, n_shards) {
  n <- nrow(data)
  if (n == 0L) {
    return(list(integer()))
  }

  if (!dplyr::is_grouped_df(data)) {
    # Contiguous ranges preserve row order on collect()
    n_bins <- min(n, n_shards)
    if (n_bins == 1L) {
      return(list(seq_len(n)))
    }
    bins <- cut(seq_len(n), breaks = n_bins, labels = FALSE)
    return(unname(split(seq_len(n), bins)))
  }

  # Assign each group to the shard with fewest rows so far
  group_id <- dplyr::group_indices(data)
  counts <- tabulate(group_id)
  n_bins <- min(length(counts), n_shards)

  bin_rows <- numeric(n_bins)
  group_bin <- integer(length(counts))
  for (i in seq_along(counts)) {
    j <- which.min(bin_rows)
    group_bin[[i]] <- j
    bin_rows[[j]] <- bin_rows[[j]] + counts[[i]]
  }

  unname(split(seq_along(group_id), group_bin[group_id]))
}

# Constructor -------------------------------------------------------------

#' A `party_df` partitioned data frame
#'
#' This S3 class represents a data frame partitioned across a cluster. You
#' can use this constructor if you have already computed the shards yourself
#' (e.g. by reading one file per shard with [cluster_map()]). If not, start
#' with [partition()] instead.
#'
#' @param cluster A cluster.
#' @param shards A list of data frames, one per shard. All shards must have
#'   the same column names. Grouped data frames are ungrouped and their
#'   group variables must agree.
#' @param groups A character vector of grouping variables, applied to every
#'   shard when a verb is evaluated. Defaults to the shards' own grouping.
#' @return An S3 object with class `multidplyr_party_df`.
#' @export
#' @examples
#' # Read one file per shard on the workers, then bind them into a party_df
#' \dontrun{
#' shards <- cluster_map(cl, files, vroom::vroom)
#' df <- party_df(cl, shards)
#' }
party_df <- function(cluster, shards, groups = character()) {
  stopifnot(is_cluster(cluster))
  if (!is.list(shards) || length(shards) == 0L) {
    cli::cli_abort("{.arg shards} must be a non-empty list of data frames.")
  }
  is_df <- vapply(shards, is.data.frame, logical(1))
  if (!all(is_df)) {
    cli::cli_abort("All elements of {.arg shards} must be data frames.")
  }

  shard_names <- lapply(shards, names)
  if (length(unique(shard_names)) != 1L) {
    cli::cli_abort(
      "All elements of {.arg shards} must have the same column names."
    )
  }

  shard_groups <- unique(lapply(shards, dplyr::group_vars))
  if (length(shard_groups) > 1L) {
    cli::cli_abort(
      "All elements of {.arg shards} must have the same group variables."
    )
  }
  shard_groups <- if (length(shard_groups) == 1L) {
    shard_groups[[1]]
  } else {
    character()
  }
  if (length(groups) == 0L) {
    groups <- shard_groups
  } else if (length(shard_groups) > 0L && !identical(shard_groups, groups)) {
    cli::cli_abort(
      "{.arg groups} does not match the group variables of {.arg shards}."
    )
  }
  if (!all(groups %in% shard_names[[1]])) {
    cli::cli_abort("All {.arg groups} must be columns of {.arg shards}.")
  }

  shards <- lapply(shards, dplyr::ungroup)
  new_party_df(cluster, shards, groups)
}

new_party_df <- function(cluster, shards, groups = character()) {
  stopifnot(is_cluster(cluster))
  structure(
    list(cluster = cluster, shards = shards, groups = groups),
    class = "multidplyr_party_df"
  )
}

is_party_df <- function(x) inherits(x, "multidplyr_party_df")

shard_rows <- function(x) {
  vapply(x$shards, nrow, integer(1))
}

# Local metadata ----------------------------------------------------------

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.multidplyr_party_df <- function(x) {
  character()
}

#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.multidplyr_party_df <- function(x) {
  names(x$shards[[1]])
}

#' @importFrom dplyr group_vars
#' @export
group_vars.multidplyr_party_df <- function(x) {
  x$groups
}

#' @export
dim.multidplyr_party_df <- function(x) {
  c(sum(shard_rows(x)), ncol(x$shards[[1]]))
}

#' @importFrom utils head
#' @export
head.multidplyr_party_df <- function(x, n = 6L, ...) {
  pieces <- vector("list", length(x$shards))
  left <- n

  # Slice before binding so a view-backed shard is not materialised whole
  for (i in seq_along(x$shards)) {
    take <- min(left, nrow(x$shards[[i]]))
    if (take > 0) {
      pieces[[i]] <- vctrs::vec_slice(x$shards[[i]], seq_len(take))
      left <- left - take
    }
    if (left == 0) {
      break
    }
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
  cat(
    "Shards: ",
    length(shards),
    " [",
    big_mark(min(shards)),
    "--",
    big_mark(max(shards)),
    " rows]\n",
    sep = ""
  )
  cat("\n")
  print(tibble::trunc_mat(head(x, 6), n = 6, width = width))

  invisible(x)
}

#' @export
as.data.frame.multidplyr_party_df <- function(x, row.names, optional, ...) {
  dplyr::bind_rows(x$shards)
}

#' @importFrom dplyr collect
#' @export
collect.multidplyr_party_df <- function(x, ...) {
  out <- dplyr::bind_rows(x$shards)
  if (length(x$groups)) {
    dplyr::grouped_df(out, x$groups)
  } else {
    out
  }
}

#' @importFrom dplyr pull
#' @export
pull.multidplyr_party_df <- function(.data, var = -1, ...) {
  var <- tidyselect::vars_pull(tbl_vars(.data), {{ var }})
  vctrs::vec_c(!!!lapply(.data$shards, `[[`, var))
}

#' @importFrom dplyr ungroup
#' @export
ungroup.multidplyr_party_df <- function(x, ...) {
  vars <- enquos(...)
  if (length(vars) == 0L) {
    groups <- character()
  } else {
    drop <- tidyselect::vars_select(x$groups, !!!vars)
    groups <- setdiff(x$groups, drop)
  }
  new_party_df(x$cluster, x$shards, groups)
}
