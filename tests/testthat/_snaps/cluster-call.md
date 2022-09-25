# old ptype interface works with warning

    Code
      out <- cluster_call(cl, 1, ptype = double())
    Condition
      Warning:
      Must now set `simplify = TRUE` when supplying ptype

# errors are propagated

    Code
      cluster_call(cl, stop("!!"))
    Condition
      Error in `cluster_call()`:
      ! Remote computation failed in worker 1
      Caused by error:
      ! !!

# errors capture worker id

    Code
      cluster_call(cl, f(x))
    Condition
      Error in `cluster_call()`:
      ! Remote computation failed in worker 2
      Caused by error:
      ! Computation failed

