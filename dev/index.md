# multidplyr

## Overview

multidplyr is a backend for dplyr that partitions a data frame across
multiple cores. You tell multidplyr how to split the data up with
[`partition()`](https://multidplyr.tidyverse.org/dev/reference/partition.md)
and then the data stays on each node until you explicitly retrieve it
with `collect()`. This minimises the amount of time spent moving data
around, and maximises parallel performance. This idea is inspired by
[partools](https://github.com/matloff/partools) by Norm Matloff and
[distributedR](https://github.com/vertica/DistributedR) by the Vertica
Analytics team.

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
# Create a filename vector containing different values on each worker
cluster_assign_each(cluster, filename = c("a.csv", "b.csv", "c.csv", "d.csv"))

# Use vroom to quickly load the csvs
cluster_send(cluster, my_data <- vroom::vroom(filename))

# Create a party_df using the my_data variable on each worker
my_data <- party_df(cluster, "my_data")
```

Alternatively, if you already have the data loaded in the main session,
you can use
[`partition()`](https://multidplyr.tidyverse.org/dev/reference/partition.md)
to automatically spread it across the workers. Before calling
[`partition()`](https://multidplyr.tidyverse.org/dev/reference/partition.md),
it’s a good idea to call `group_by()` to ensure that all of the
observations belonging to a group end up on the same worker.

``` r
library(nycflights13)

flight_dest <- flights %>% group_by(dest) %>% partition(cluster)
flight_dest
#> Source: party_df [336,776 x 19]
#> Groups: dest
#> Shards: 4 [81,594--86,548 rows]
#> 
#> # A data frame: 336,776 × 19
#>    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
#>   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
#> 1  2013     1     1      544            545        -1     1004           1022
#> 2  2013     1     1      558            600        -2      923            937
#> 3  2013     1     1      559            600        -1      854            902
#> 4  2013     1     1      602            610        -8      812            820
#> 5  2013     1     1      602            605        -3      821            805
#> 6  2013     1     1      611            600        11      945            931
#> # ℹ 336,770 more rows
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
#>  1 ABQ    13.7   254
#>  2 AUS    13.0  2439
#>  3 BQN    12.4   896
#>  4 BTV    13.6  2589
#>  5 BUF    13.4  4681
#>  6 CLE    13.4  4573
#>  7 CMH    12.2  3524
#>  8 DEN    15.2  7266
#>  9 DSM    26.2   569
#> 10 DTW    11.8  9384
#> # ℹ 95 more rows
```

Note that there is some overhead associated with copying data from the
worker nodes back to the host node (and vice versa), so you’re best off
using multidplyr with more complex operations. See
[`vignette("multidplyr")`](https://multidplyr.tidyverse.org/dev/articles/multidplyr.md)
for more details.
