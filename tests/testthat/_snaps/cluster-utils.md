# cluster_rm removes values again

    Code
      pf %>% mutate(y = x + a)
    Condition
      Error in `collect_map()`:
      ! Remote computation failed in shard 1.
      Caused by error in `dplyr::mutate()`:
      ! In argument: `y = x + a`.
      Caused by error:
      ! object 'a' not found

