# old ptype interface works with warning

    Code
      out <- cluster_call(cl, 1, ptype = double())
    Warning <rlang_warning>
      Must now set `simplify = TRUE` when supplying ptype

# errors are propagated

    Code
      cluster_call(cl, stop("!!"))
    Error <rlang_error>
      Remote computation failed:
      !!

