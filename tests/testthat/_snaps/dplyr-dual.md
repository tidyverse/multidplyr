# joining data frame requires explicit copy

    Code
      left_join(pf, df)
    Condition
      Error in `auto_copy()`:
      ! `x` and `y` must share the same src.
      i `x` is a <multidplyr_party_df> object.
      i `y` is a <data.frame> object.
      i Set `copy = TRUE` if `y` can be copied to the same source as `x` (may be slow).

# joining party_dfs needs matching shard counts

    Code
      inner_join(pf1, pf2, by = "x")
    Condition
      Error in `shard_call_dual()`:
      ! `x` and `y` must have the same number of shards.
      i `x` has 2 shards and `y` has 3.
      i Partition both data frames with the same `n_shards` and grouping.

