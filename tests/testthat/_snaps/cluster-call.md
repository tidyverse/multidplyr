# errors are propagated

    Code
      cluster_call(cl, stop("!!"))
    Condition
      Error in `cluster_call()`:
      ! Remote computation failed:
      !!

