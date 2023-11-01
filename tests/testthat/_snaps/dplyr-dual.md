# joining data frame requires explicit copy

    Code
      left_join(pf, df)
    Condition
      Error in `auto_copy()`:
      ! `x` and `y` must share the same src.
      i set `copy` = TRUE (may be slow).

