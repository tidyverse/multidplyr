# A \`party_df\` partitioned data frame

This S3 class represents a data frame partitioned across workers in a
cluster. You can use this constructor if you have already spread data
frames spread across a cluster. If not, start with \[partition()\]
instead.

## Usage

``` r
party_df(cluster, name, auto_rm = FALSE)
```

## Arguments

- cluster:

  A cluster

- name:

  Name of data frame variable. Must exist on every worker, be a data
  frame, and have the same names.

- auto_rm:

  If \`TRUE\`, will automatically \`rm()\` the data frame on the workers
  when this object is created.

## Value

An S3 object with class \`multidplyr_party_df\`.

## Examples

``` r
# If a real example, you might spread file names across the clusters
# and read in using data.table::fread()/vroom::vroom()/qs2::qs_read().
cl <- default_cluster()
cluster_send(cl[1], n <- 10)
cluster_send(cl[2], n <- 15)
cluster_send(cl, df <- data.frame(x = runif(n)))

df <- party_df(cl, "df")
df
#> Source: party_df [25 x 1]
#> Shards: 2 [10--15 rows]
#> 
#> # A data frame: 25 × 1
#>        x
#>    <dbl>
#> 1 0.304 
#> 2 0.922 
#> 3 0.484 
#> 4 0.811 
#> 5 0.913 
#> 6 0.0890
#> # ℹ 19 more rows
```
