# joining data frame requires explicit copy

    Code
      left_join(pf, df)
    Condition
      Error in `auto_copy()`:
      ! `x` and `y` must share the same src.
      i `x` is a <multidplyr_party_df> object.
      i `y` is a <data.frame> object.
      i Set `copy = TRUE` if `y` can be copied to the same source as `x` (may be slow).

