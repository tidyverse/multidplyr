# Partition data across workers in a cluster

Partitioning ensures that all observations in a group end up on the same
worker. To try and keep the observations on each worker balanced,
\`partition()\` uses a greedy algorithm that iteratively assigns each
group to the worker that currently has the fewest rows.

## Usage

``` r
partition(data, cluster)
```

## Arguments

- data:

  Dataset to partition, typically grouped. When grouped, all
  observations in a group will be assigned to the same cluster.

- cluster:

  Cluster to use.

## Value

A \[party_df\].

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
cl <- default_cluster()
cluster_library(cl, "dplyr")

mtcars2 <- partition(mtcars, cl)
mtcars2 %>% mutate(cyl2 = 2 * cyl)
#> Source: party_df [32 x 12]
#> Shards: 2 [16--16 rows]
#> 
#> # A data frame: 32 × 12
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#> 2  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#> 3  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#> 4  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#> 5  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 6  17.8     6  168.   123  3.92  3.44  18.9     1     0     4     4
#> # ℹ 26 more rows
#> # ℹ 1 more variable: cyl2 <dbl>
mtcars2 %>% filter(vs == 1)
#> Source: party_df [14 x 11]
#> Shards: 2 [5--9 rows]
#> 
#> # A data frame: 14 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  22.8     4 108      93  3.85  2.32  18.6     1     1     4     1
#> 2  22.8     4 141.     95  3.92  3.15  22.9     1     0     4     2
#> 3  17.8     6 168.    123  3.92  3.44  18.9     1     0     4     4
#> 4  30.4     4  75.7    52  4.93  1.62  18.5     1     1     4     2
#> 5  21.5     4 120.     97  3.7   2.46  20.0     1     0     3     1
#> 6  21.4     6 258     110  3.08  3.22  19.4     1     0     3     1
#> # ℹ 8 more rows
mtcars2 %>% group_by(cyl) %>% summarise(n())
#> Source: party_df [6 x 2]
#> Shards: 2 [3--3 rows]
#> 
#> # A data frame: 6 × 2
#>     cyl `n()`
#>   <dbl> <int>
#> 1     4     5
#> 2     6     2
#> 3     8     9
#> 4     4     6
#> 5     6     5
#> 6     8     5
mtcars2 %>% select(-cyl)
#> Source: party_df [32 x 10]
#> Shards: 2 [16--16 rows]
#> 
#> # A data frame: 32 × 10
#>     mpg  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21    160    110  3.9   2.62  16.5     0     1     4     4
#> 2  22.8  108     93  3.85  2.32  18.6     1     1     4     1
#> 3  18.7  360    175  3.15  3.44  17.0     0     0     3     2
#> 4  14.3  360    245  3.21  3.57  15.8     0     0     3     4
#> 5  22.8  141.    95  3.92  3.15  22.9     1     0     4     2
#> 6  17.8  168.   123  3.92  3.44  18.9     1     0     4     4
#> # ℹ 26 more rows
```
