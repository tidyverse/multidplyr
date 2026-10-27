# errors carry the element index

    Code
      cluster_map(cl, 1:3, function(x) if (x == 2) stop("boom") else x)
    Condition
      Error in `collect_map()`:
      ! Remote computation failed in shard 2.
      Caused by error:
      ! boom

