# Default cluster

Setting up a cluster is relatively expensive, so it's best to use a
single cluster throughout a session. This function lazily creates a
2-worker cluster for use in examples and test.

## Usage

``` r
default_cluster(n = 2)
```

## Arguments

- n:

  Number of workers to use; defaults to 2 because this is the maximum
  allowed by CRAN.

## Value

A cached cluster of workers.

## Examples

``` r
default_cluster()
#> 2 session cluster [..]
```
