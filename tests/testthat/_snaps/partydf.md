# n_shards controls the shard count

    Code
      partition(df, cl, n_shards = 0)
    Condition
      Error in `partition()`:
      ! `n_shards` must be a positive number.

# party_df validates its shards

    Code
      party_df(cl, list(1))
    Condition
      Error in `party_df()`:
      ! All elements of `shards` must be data frames.
    Code
      party_df(cl, list(tibble(x = 1), tibble(y = 1)))
    Condition
      Error in `party_df()`:
      ! All elements of `shards` must have the same column names.
    Code
      party_df(cl, list(group_by(tibble(g = 1), g), tibble(g = 1)))
    Condition
      Error in `party_df()`:
      ! All elements of `shards` must have the same group variables.
    Code
      party_df(cl, list(group_by(tibble(g = 1), g)), groups = "x")
    Condition
      Error in `party_df()`:
      ! `groups` does not match the group variables of `shards`.
    Code
      party_df(cl, list(tibble(x = 1)), groups = "g")
    Condition
      Error in `party_df()`:
      ! All `groups` must be columns of `shards`.

# can print partydf

    Code
      pf
    Output
      Source: party_df [20 x 1]
      Shards: 8 [2--3 rows]
      
    Condition
      Warning:
      `trunc_mat()` was deprecated in tibble 3.1.0.
      i Printing has moved to the pillar package.
    Output
      # A tibble: 6 x 1
            y
        <int>
      1     1
      2     2
      3     3
      4     4
      5     5
      6     6

---

    Code
      pf %>% group_by(y)
    Output
      Source: party_df [20 x 1]
      Groups: y
      Shards: 8 [2--3 rows]
      
    Condition
      Warning:
      `trunc_mat()` was deprecated in tibble 3.1.0.
      i Printing has moved to the pillar package.
    Output
      # A tibble: 6 x 1
            y
        <int>
      1     1
      2     2
      3     3
      4     4
      5     5
      6     6

