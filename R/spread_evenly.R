#' @title Allocate different sized groups into partitions
#' @param groups A tibble with columns id, n, part_id.
#' @param m Number of partitions to allocate into.
#' @description Implements a first fit pass plus a best fit pass heuristic of the bin
#' packing problem. Inspired by the algorithm here:
#' \url{http://stackoverflow.com/questions/16588669/spread-objects-evenly-over-multiple-collections}
#' If the number of groups is large, the bin packing algorithm can be expensive.
#' So, if the ratio of the number of groups to partitions is greater than 300, I simply do the first pass.
#' If the ratio is greater than 1500, I randomize the groups.
#' @return A tibble with columns id, n, part_id, spread (approximately) evenly.
#' @export
spread_evenly <- function(groups, m){
  orig_names <- names(groups)
  groups <- groups %>%
    dplyr::arrange(dplyr::desc(n))
  # Get an initial spread solution
  # Put the m largest groups in different clusters to guarantee all buckets have at least one element
  groups$part_id[1:m] <- 1:m

  # Then allocate the rest randomly
  is_NA <- is.na(groups$part_id)
  groups$part_id[is_NA] <- sample(1:m, size = sum(is_NA), replace = TRUE)

  if(NROW(groups) / m < 1500){
    groups <- groups %>%
      dplyr::group_by(part_id) %>%
      dplyr::mutate(n_bucket = sum(n))

    average <- sum(groups$n) / m

    # Identify the groups that are above average and have more than one member
    groups <- groups %>%
      dplyr::mutate(large = n_bucket[1] > average) %>%
      dplyr::mutate(eligible = length(n_bucket) > 1 & large)

    # Average should be calculated with all small buckets and large eligible buckets
    get_average <- function(groups, m){
      tmp <- groups %>%
        dplyr::filter(eligible | !large)
      tmp %>%
        .$n %>%
        sum() / length(unique(tmp$part_id))
    }

    average <- get_average(groups, m)
    # Recompute large/small with new average (do just one pass of this)
    groups <- groups %>%
      dplyr::mutate(large = n_bucket[1] > average) %>%
      dplyr::mutate(eligible = length(n_bucket) > 1 & large)

    groups <- groups %>%
      # There can be more than one max element - choose the first one
      dplyr::mutate(max_elem_lgl = dplyr::if_else(large & eligible, n == max(n), NA),
                    max_elem = n[max_elem_lgl][1])
    # Pass 1
    i <- 1
    while(any(groups$large & groups$eligible)){
      if (i == 1000){
        warning(paste0("spread_evenly is experimental and currently limited to 1000 iterations. ",
                       "You may have found a bug causing an infinite loop - please file an issue ",
                       "at https://github.com/hadley/multidplyr/issues with a reproducible example."))
        break
      }
      i <- i + 1

      # Use the $ to get the whole vector in the min/max (recall this is a grouped_df)
      groups <- groups %>%
        dplyr::mutate(this_small = dplyr::if_else(!large, n_bucket == min(groups$n_bucket), FALSE),
                      this_large = dplyr::if_else(large & eligible,
                                                  max_elem == max(groups$max_elem[groups$large & groups$eligible],
                                                                  na.rm = TRUE), FALSE))
      # This covers groups that switch from large to small through the algorithm
      groups$eligible[groups$this_small] <- FALSE

      this_large_id <- groups$part_id[groups$this_large][1]
      this_small_id <- groups$part_id[groups$this_small][1]
      # These aren't necessarily one part_id
      groups$this_large[groups$this_large & groups$part_id != this_large_id] <- FALSE
      groups$this_small[groups$this_small & groups$part_id != this_small_id] <- FALSE

      if(groups$n_bucket[groups$this_large][1] - groups$max_elem[groups$this_large][1] < average){
        # Set all of this_large to ineligible
        groups$eligible[groups$this_large] <- FALSE
        groups$large[groups$this_large] <- FALSE
        groups$max_elem_lgl[groups$this_large] <- NA
        groups$max_elem[groups$this_large] <- NA
        groups$this_large[groups$this_large] <- FALSE
      } else {
        to_move_lgl <- groups$this_large & groups$max_elem_lgl
        groups$part_id[to_move_lgl] <- groups$part_id[groups$this_small][1]
        groups$this_small[to_move_lgl] <- TRUE
        groups$this_large[to_move_lgl] <- FALSE
        groups$large[to_move_lgl] <- FALSE
        groups$eligible[to_move_lgl] <- FALSE
        groups$max_elem_lgl[to_move_lgl] <- NA
        groups$max_elem[to_move_lgl] <- NA

        groups$n_bucket[groups$this_small] <- sum(groups$n[groups$this_small])
        groups$n_bucket[groups$this_large] <- sum(groups$n[groups$this_large])

        groups$max_elem_lgl[groups$this_large] <- groups$n[groups$this_large] == max(groups$n[groups$this_large])
        groups$max_elem[groups$this_large] <- groups$n[groups$max_elem_lgl & groups$this_large][1]
        groups <- groups %>% dplyr::group_by(part_id)
      }
      average <- get_average(groups, m)
      # Recompute large/small and max_elem with new average
      groups <- groups %>%
        dplyr::mutate(large = n_bucket[1] > average) %>%
        # There can be more than one max element - choose the first one
        dplyr::mutate(max_elem_lgl = dplyr::if_else(large & eligible, n == max(n), NA),
                      max_elem = n[max_elem_lgl][1])

    }
    groups <- groups %>%
      dplyr::mutate(eligible = length(n_bucket) > 1)
    average <- get_average(groups, m)
    groups <- groups %>%
      dplyr::mutate(large = n_bucket[1] > average)
    if(NROW(groups) / m < 300){
      # Pass 2 uses the best fitting elements from largest to this_small - There shouldn't be too many options
      # as Pass 1 should have handled the most obvious moves.
      for(.part_id in unique(groups$part_id[!groups$large])){
        i <- 1
        while(any(groups$large & groups$eligible)){
          if (i == 10000){
            warning(paste0("spread_evenly is experimental and currently limited to 10000 iterations. ",
                           "You may have found a bug causing an infinite loop - please file an issue ",
                           "at https://github.com/hadley/multidplyr/issues with a reproducible example."))
            break
          }
          i <- i + 1
          average <- get_average(groups, m)
          groups <- groups %>%
            dplyr::mutate(this_small = part_id == .part_id,
                          this_large = dplyr::if_else(large & eligible,
                                                      n_bucket == max(groups$n_bucket[groups$large &
                                                                                        groups$eligible]), FALSE))
          this_large_id <- groups$part_id[groups$this_large][1]
          this_small_id <- groups$part_id[groups$this_small][1]
          # These aren't necessarily one part_id
          groups$this_large[groups$this_large & groups$part_id != this_large_id] <- FALSE
          groups$this_small[groups$this_small & groups$part_id != this_small_id] <- FALSE

          old_dist <- abs(groups$n_bucket[groups$this_small][1] - average) + abs(groups$n_bucket[groups$this_large][1] - average)

          # Best fit based on making the small one as close to average as possible.
          id_to_move <- groups %>%
            dplyr::filter(this_large) %>%
            dplyr::filter(n_bucket - n > average) %>%
            dplyr::mutate(dist_from_av = n + groups$n_bucket[groups$this_small][1] - average) %>%
            # Keep only those that don't push small over average
            dplyr::filter(dist_from_av < 0) %>%
            dplyr::slice(which.min(abs(dist_from_av))) %>%
            .$id
          # This doesn't check if this is actually an improving move. Is it guaranteed to be?

          if(length(id_to_move) > 0){
            to_move_lgl <- groups$id == id_to_move
            groups$part_id[to_move_lgl] <- groups$part_id[groups$this_small][1]
            groups$this_small[to_move_lgl] <- TRUE
            groups$this_large[to_move_lgl] <- FALSE
            groups$large[to_move_lgl] <- FALSE

            groups$n_bucket[groups$this_small] <- sum(groups$n[groups$this_small])
            groups$n_bucket[groups$this_large] <- sum(groups$n[groups$this_large])
            new_dist <- abs(groups$n_bucket[groups$this_small][1] - average) + abs(groups$n_bucket[groups$this_large][1] - average)
            groups <- groups %>% dplyr::group_by(part_id)
            if(new_dist > old_dist){
              browser()
            }
          } else {
            groups$eligible[groups$this_large] <- FALSE
            groups$this_large[groups$this_large] <- FALSE
          }

        }
        groups <- groups %>%
          dplyr::mutate(eligible = length(n_bucket) > 1)
        average <- get_average(groups, m)
        groups <- groups %>%
          dplyr::mutate(large = n_bucket[1] > average)
      }
    }
  }
  groups <- groups %>%
    dplyr::select(dplyr::one_of(orig_names))
}
