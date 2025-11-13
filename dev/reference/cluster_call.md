# Call a function on each node of a cluster

\`cluster_call()\` executes the code on each worker and returns the
results; \`cluster_send()\` executes the code ignoring the result. Jobs
are submitted to workers in parallel, and then we wait until they're
complete.

## Usage

``` r
cluster_call(cluster, code, simplify = FALSE, ptype = NULL)

cluster_send(cluster, code)
```

## Arguments

- cluster:

  A cluster.

- code:

  An expression to execute on each worker.

- simplify:

  Should the results be simplified from a list? \* \`TRUE\`: simplify or
  die trying. \* \`NA\`: simplify if possible. \* \`FALSE\`: never try
  to simplify, always leaving as a list.

  \`code\` must return a vector of length one in order for
  simplification to succeed.

- ptype:

  If \`simplify\` is \`TRUE\`, use \`ptype\` to enforce the desired
  output type.

## Value

A list of results with one element for each worker in \`cluster\`.

## Examples

``` r
cl <- default_cluster()
#> Initialising default cluster of size 2

# Run code on each cluster and retrieve results
cluster_call(cl, Sys.getpid())
#> [[1]]
#> [1] 7643
#> 
#> [[2]]
#> [1] 7652
#> 
cluster_call(cl, runif(1))
#> [[1]]
#> [1] 0.6863466
#> 
#> [[2]]
#> [1] 0.9655598
#> 

# use ptype to simplify
cluster_call(cl, runif(1), simplify = TRUE)
#> [1] 0.3494536 0.9017765

# use cluster_send() to ignore results
cluster_send(cl, x <- runif(1))
cluster_call(cl, x, simplify = TRUE)
#> [1] 0.4209335 0.3051460
```
