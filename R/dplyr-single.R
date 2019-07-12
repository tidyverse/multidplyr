
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

#' @importFrom dplyr ungroup
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
