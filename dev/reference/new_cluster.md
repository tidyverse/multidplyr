# Create a new cluster with sensible defaults.

Clusters created with this function will automatically clean up after
themselves.

## Usage

``` r
new_cluster(n)
```

## Arguments

- n:

  Number of workers to create. Avoid setting this higher than the number
  of cores in your computer as it will degrade performance.

## Value

A \`multidplyr_cluster\` object.

## Examples

``` r
cluster <- new_cluster(2)
cluster
#> 2 session cluster [..]
```
