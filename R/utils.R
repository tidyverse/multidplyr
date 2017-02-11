#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

wrap <- function(..., indent = 0) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 2,
    width = getOption("width"))

  paste0(wrapped, collapse = "\n")
}

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}


grouping_part <- function(cluster_id, groups_n) {

  if (length(cluster_id) >= length(groups_n)) {
    return(cluster_id[seq_along(groups_n)])
  }

  if(length(cluster_id) == 1L) return(rep(cluster_id, times = length(groups_n)))

  m <- length(cluster_id)
  sorted_group_n <- sort(groups_n, decreasing = TRUE, index.return = TRUE)
  sorted_group_n <- setNames(sorted_group_n$x, sorted_group_n$ix)

  mean_load <- sum(sorted_group_n) / m
  nodes <- setNames(vector('list', m), cluster_id)

  nodes[] <- lapply(seq_len(m), function(.x) sorted_group_n[.x])
  nodes[[1]] <- c(nodes[[1]], sorted_group_n[-seq_along(nodes)])

  for (i in seq_along(nodes)) {
    k <- length(nodes[[i]])
    while ((sum(nodes[[i]]) > mean_load) &&
           (length(nodes[[i]]) > 1) &&
           (k > 0)
    ) {
      test <- TRUE
      j    <- i + 1
      while (test && j < (m + 1)) {
        if (sum(nodes[[j]] + min(nodes[[i]])) <= mean_load) {
          id_min <- which.min(nodes[[i]])
          nodes[[j]] <- c(nodes[[j]], nodes[[i]][id_min])
          nodes[[i]] <- nodes[[i]][-id_min]
          test <-  FALSE
        }
        j <- j + 1
      }
      k <- k - 1
    }
  }

  nodes[] <- lapply(names(nodes),
                    function(.x) {
                      setNames(rep(.x, length(nodes[[.x]])), names(nodes[[.x]]))
                    }
  )
  nodes <- unlist(nodes)
  idx <- names(nodes) %>%
          vapply(function(.x) strsplit(.x,'\\.')[[1]][[2]],
                 FUN.VALUE = 'character'
          ) %>%
          as.integer
  nodes[idx] %>% as.integer
}
