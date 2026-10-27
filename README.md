
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multidplyr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/tidyverse/multidplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverse/multidplyr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidyverse/multidplyr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidyverse/multidplyr?branch=main)
[![Codecov test
coverage](https://codecov.io/gh/tidyverse/multidplyr/graph/badge.svg)](https://app.codecov.io/gh/tidyverse/multidplyr)
<!-- badges: end -->

## Overview

multidplyr is a backend for dplyr that partitions a data frame across
multiple cores. You tell multidplyr how to split the data up with
`partition()` and then the data stays on each node until you explicitly
retrieve it with `collect()`. This minimises the amount of time spent
moving data around, and maximises parallel performance. This idea is
inspired by [partools](https://github.com/matloff/partools) by Norm
Matloff and [distributedR](https://github.com/vertica/DistributedR) by
the Vertica Analytics team.

This branch of multidplyr is built on
[mizu](https://github.com/shikokuchuo/mizu) and
[mori](https://cran.r-project.org/package=mori): the workers form a
single work-stealing process pool and the data lives in shared memory.
`partition()` performs no data copies in the host session — the data is
shared once and sliced in parallel on the workers — verb results return
as zero-copy views for ungrouped data with plain columns, and cross with
a single serialisation (no deep copy) otherwise. Load is balanced
dynamically, so a slow shard no longer holds back the others.

Due to the overhead associated with communicating between the nodes, you
won’t see much performance improvement with simple operations on less
than ~10 million observations, and you may want to instead try
[dtplyr](https://dtplyr.tidyverse.org/), which uses
[data.table](https://R-datatable.com/). multidplyr’s strength is found
parallelising calls to slower and more complex functions.

## Installation

You can install the released version of multidplyr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("multidplyr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tidyverse/multidplyr")
```

The mizu-based branch additionally needs mizu, which is not yet on CRAN:

``` r
pak::pak("shikokuchuo/mizu")
```

multidplyr runs on 64-bit Linux (kernel \>= 5.3), macOS, and Windows.
The data lives in shared memory, so on Linux the partitions are bounded
by `/dev/shm` (typically half of RAM).

## Usage

To use multidplyr, you first create a cluster of the desired number of
workers. Each one of these workers is a separate R process, and the
operating system will spread their execution across multiple cores:

``` r
library(multidplyr)

cluster <- new_cluster(4)
cluster_library(cluster, "dplyr")
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

There are two primary ways to use multidplyr. The first, and most
efficient, way is to read different files on each worker:

``` r
# Read one file per shard, in parallel on the workers
shards <- cluster_map(cluster, c("a.csv", "b.csv", "c.csv", "d.csv"), vroom::vroom)

# Then bind the shards into a party_df
my_data <- party_df(cluster, shards)
```

Alternatively, if you already have the data loaded in the main session,
you can use `partition()` to automatically spread it across the workers.
Before calling `partition()`, it’s a good idea to call `group_by()` to
ensure that all of the observations belonging to a group end up in the
same shard.

``` r
library(nycflights13)

flight_dest <- flights %>% group_by(dest) %>% partition(cluster)
flight_dest
#> Source: party_df [336,776 x 19]
#> Groups: dest
#> Shards: 16 [17,756--28,839 rows]
#> 
#> # A tibble: 6 × 19
#>    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
#>   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
#> 1  2013     1     1      558            600        -2      924            917
#> 2  2013     1     1      628            630        -2     1016            947
#> 3  2013     1     1      658            700        -2     1027           1025
#> 4  2013     1     1      702            700         2     1058           1014
#> 5  2013     1     1      743            730        13     1107           1100
#> 6  2013     1     1      822            830        -8     1014           1016
#> # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
#> #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
#> #   hour <dbl>, minute <dbl>, time_hour <dttm>
```

Now you can work with it like a regular data frame, but the computations
will be spread across multiple cores. Once you’ve finished computation,
use `collect()` to bring the data back to the host session:

``` r
flight_dest %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE), n = n()) %>% 
  collect()
#> # A tibble: 105 × 3
#>    dest  delay     n
#>    <chr> <dbl> <int>
#>  1 ABQ   13.7    254
#>  2 BZN   11.5     36
#>  3 CHO   21.4     52
#>  4 CLE   13.4   4573
#>  5 LAX    9.40 16174
#>  6 ACK    6.46   265
#>  7 CAE   35.6    116
#>  8 CRW   17      138
#>  9 DCA   10.3   9705
#> 10 OKC   30.6    346
#> # ℹ 95 more rows
```

Note that there is some overhead associated with copying data from the
worker nodes back to the host node (and vice versa), so you’re best off
using multidplyr with more complex operations. See
`vignette("multidplyr")` for more details.
