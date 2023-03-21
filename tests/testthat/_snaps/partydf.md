# can construct and print partydf

    Code
      df
    Output
      Source: party_df [20 x 1]
      Shards: 2 [10--10 rows]
      
      # A data frame: 20 x 1
            y
        <int>
      1     1
      2     2
      3     3
      4     4
      5     5
      6     6
      # ... with 14 more rows

---

    Code
      group_by(df, y)
    Output
      Source: party_df [20 x 1]
      Groups: y
      Shards: 2 [10--10 rows]
      
      # A data frame: 20 x 1
            y
        <int>
      1     1
      2     2
      3     3
      4     4
      5     5
      6     6
      # ... with 14 more rows

