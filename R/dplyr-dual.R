#' @importFrom dplyr same_src
#' @export
same_src.multidplyr_party_df <- function(x, y) {
  is_party_df(y) && identical(x$cluster, y$cluster)
}

#' @importFrom dplyr auto_copy
#' @export
auto_copy.multidplyr_party_df <- function(x, y, copy = FALSE, ...) {
  name <- table_name()
  cluster_assign(x$cluster, !!name := y)
  party_df(x$cluster, name)
}

# joins -------------------------------------------------------------------

#' @importFrom dplyr left_join
#' @export
left_join.multidplyr_party_df <- function(
  x,
  y,
  ...,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y")
) {
  y <- auto_copy(x, y, copy = copy)
  shard_call_dual("left_join", x, y, ..., by = by, suffix = suffix)
}

#' @importFrom dplyr right_join
#' @export
right_join.multidplyr_party_df <- function(
  x,
  y,
  ...,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y")
) {
  y <- auto_copy(x, y, copy = copy)
  shard_call_dual("right_join", x, y, ..., by = by, suffix = suffix)
}

#' @importFrom dplyr inner_join
#' @export
inner_join.multidplyr_party_df <- function(
  x,
  y,
  ...,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y")
) {
  y <- auto_copy(x, y, copy = copy)
  shard_call_dual("inner_join", x, y, ..., by = by, suffix = suffix)
}

#' @importFrom dplyr full_join
#' @export
full_join.multidplyr_party_df <- function(
  x,
  y,
  ...,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y")
) {
  y <- auto_copy(x, y, copy = copy)
  shard_call_dual("full_join", x, y, ..., by = by, suffix = suffix)
}

#' @importFrom dplyr anti_join
#' @export
anti_join.multidplyr_party_df <- function(x, y, ..., by = NULL, copy = FALSE) {
  y <- auto_copy(x, y, copy = copy)
  shard_call_dual("anti_join", x, y, ..., by = by)
}

#' @importFrom dplyr semi_join
#' @export
semi_join.multidplyr_party_df <- function(x, y, ..., by = NULL, copy = FALSE) {
  y <- auto_copy(x, y, copy = copy)
  shard_call_dual("semi_join", x, y, ..., by = by)
}

# setops ------------------------------------------------------------------

#' @importFrom dplyr intersect
# Exported onload
intersect.multidplyr_party_df <- function(x, y, ..., copy = FALSE) {
  y <- auto_copy(x, y, copy = copy)
  shard_call_dual("intersect", x, y, ...)
}

#' @importFrom dplyr union
# Exported onload
union.multidplyr_party_df <- function(x, y, ..., copy = FALSE) {
  y <- auto_copy(x, y, copy = copy)
  shard_call_dual("union", x, y, ...)
}

#' @importFrom dplyr union_all
#' @export
union_all.multidplyr_party_df <- function(x, y, ..., copy = FALSE) {
  y <- auto_copy(x, y, copy = copy)
  shard_call_dual("union_all", x, y, ...)
}

#' @importFrom dplyr setdiff
# Exported onload
setdiff.multidplyr_party_df <- function(x, y, ..., copy = FALSE) {
  y <- auto_copy(x, y, copy = copy)
  shard_call_dual("setdiff", x, y, ...)
}


# helpers -----------------------------------------------------------------

shard_call_dual <- function(.verb, .x, .y, ...) {
  new_name <- table_name()
  call <- call2(.verb, .x$name, .y$name, ..., .ns = "dplyr")

  cluster_send(.x$cluster, !!call2("<-", new_name, call))
  new_party_df(.x$cluster, new_name, auto_rm = TRUE)
}
