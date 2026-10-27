# an error in one shard aborts the verb

    Code
      pf %>% mutate(y = stop("boom"))
    Condition
      Error in `collect_map()`:
      ! Remote computation failed in shard 1.
      Caused by error in `dplyr::mutate()`:
      ! In argument: `y = stop("boom")`.
      Caused by error:
      ! boom

