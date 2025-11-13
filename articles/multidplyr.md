# An introduction to multidplyr

multidplyr is a backend for dplyr that spreads work across multiple
processes. Like all dplyr backends, it allows you to use the dplyr verbs
that you’re already familiar with, but alters the underlying
computational model to transparently support multi-process parallelism.

This vignette will show you the basics of multidplyr using the
[nycflights13](https://CRAN.R-project.org/package=nycflights13) dataset.

``` r
library(multidplyr)
library(dplyr, warn.conflicts = FALSE)
library(nycflights13)
```

## Creating a cluster

To start using multidplyr you must create a cluster. Here I used two
cores because it’s the maximum permitted by CRAN, but I suggest that you
use more. For best performance, I recommend using 1 or 2 fewer than the
total number of cores on your computer, which you can detect with
[`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
(leaving at least 1 core free means that you should still be able to use
your computer for other tasks while your computation is running).

``` r
cluster <- new_cluster(2)
cluster
#> 2 session cluster [..]
```

(In the examples, you’ll also see the use of
[`default_cluster()`](https://multidplyr.tidyverse.org/reference/default_cluster.md);
this is designed specifically for the constraints of R CMD check, so I
don’t recommend using it in your own code.)

A cluster consists of multiple R processes created by
[callr](https://callr.r-lib.org/). When multiple processes are running
at the same time, your operating system will take care of spreading the
work across multiple cores.

## Add data

There are two ways to get data to the workers in cluster:

- [`partition()`](https://multidplyr.tidyverse.org/reference/partition.md)
  a data frame that already loaded *in the interactive process*.
- Load a different subset of the data *in each worker*.

### `partition()`

[`partition()`](https://multidplyr.tidyverse.org/reference/partition.md)
is useful if you have a single in-memory data frame. For example, take
[`nycflights13::flights`](https://rdrr.io/pkg/nycflights13/man/flights.html).
This dataset contains information for about ~300,000 flights departing
New York City in 2013. We group it by destination, then partition it:

``` r
flights1 <- flights %>% group_by(dest) %>% partition(cluster)
flights1
#> Source: party_df [336,776 x 19]
#> Groups: dest
#> Shards: 2 [166,251--170,525 rows]
#> 
#> # A data frame: 336,776 × 19
#>    year month   day dep_time sched_dep_time dep_delay arr_time
#>   <int> <int> <int>    <int>          <int>     <dbl>    <int>
#> 1  2013     1     1      557            600        -3      709
#> 2  2013     1     1      557            600        -3      838
#> 3  2013     1     1      558            600        -2      849
#> 4  2013     1     1      558            600        -2      853
#> 5  2013     1     1      559            559         0      702
#> 6  2013     1     1      559            600        -1      854
#> # ℹ 336,770 more rows
#> # ℹ 12 more variables: sched_arr_time <int>, arr_delay <dbl>,
#> #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>,
#> #   dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
#> #   minute <dbl>, time_hour <dttm>
```

[`partition()`](https://multidplyr.tidyverse.org/reference/partition.md)
splits `flights1` into roughly equal subsets on each worker, ensuring
that all rows in a group are transfered to the same worker. The result
is a `party_df`, or partitioned data frame.

### Direct loading

[`partition()`](https://multidplyr.tidyverse.org/reference/partition.md)
is simple to call, but it’s relatively expensive because it copies a lot
of data between processes. An alternative strategy is for each worker to
load the data (from files) it needs directly.

To show how that might work, I’ll first split flights up by month and
save as csv files:

``` r
path <- tempfile()
dir.create(path)

flights %>% 
  group_by(month) %>% 
  group_walk(~ vroom::vroom_write(.x, sprintf("%s/month-%02i.csv", path, .y$month)))
```

Now we find all the files in the directory, and divide them up so that
each worker gets (approximately) the same number of pieces:

``` r
files <- dir(path, full.names = TRUE)
cluster_assign_partition(cluster, files = files)
```

Then we read in the files on each worker and use
[`party_df()`](https://multidplyr.tidyverse.org/reference/party_df.md)
to create a partitioned dataframe:

``` r
cluster_send(cluster, flights2 <- vroom::vroom(files))

flights2 <- party_df(cluster, "flights2")
flights2
#> Source: party_df [336,776 x 18]
#> Shards: 2 [166,158--170,618 rows]
#> 
#> # A data frame: 336,776 × 18
#>    year   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
#>   <dbl> <dbl>    <dbl>          <dbl>     <dbl>    <dbl>          <dbl>
#> 1  2013     1      517            515         2      830            819
#> 2  2013     1      533            529         4      850            830
#> 3  2013     1      542            540         2      923            850
#> 4  2013     1      544            545        -1     1004           1022
#> 5  2013     1      554            600        -6      812            837
#> 6  2013     1      554            558        -4      740            728
#> # ℹ 336,770 more rows
#> # ℹ 11 more variables: arr_delay <dbl>, carrier <chr>, flight <dbl>,
#> #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>,
#> #   distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>
```

## dplyr verbs

Once you have a partitioned data frame, you can operate on it with the
usual dplyr verbs. To bring the data back to the interactive process,
use [`collect()`](https://dplyr.tidyverse.org/reference/compute.html):

``` r
flights1 %>% 
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  collect()
#> # A tibble: 105 × 2
#>    dest  dep_delay
#>    <chr>     <dbl>
#>  1 ABQ       13.7 
#>  2 ALB       23.6 
#>  3 AUS       13.0 
#>  4 AVL        8.19
#>  5 BDL       17.7 
#>  6 BGR       19.5 
#>  7 BHM       29.7 
#>  8 BNA       16.0 
#>  9 BOS        8.73
#> 10 BZN       11.5 
#> # ℹ 95 more rows
```

For this size of data and a simple transformation, using a local cluster
actually makes performance much worse!

``` r
by_dest <- flights %>% group_by(dest)

# Local computation
system.time(by_dest %>% summarise(mean(dep_delay, na.rm = TRUE)))
#>    user  system elapsed 
#>   0.011   0.000   0.011

# Remote: partitioning
system.time(flights2 <- flights %>% partition(cluster))
#>    user  system elapsed 
#>   0.388   0.005   0.568
# Remote: computation
system.time(flights3 <- flights2 %>% summarise(mean(dep_delay, na.rm = TRUE)))
#>    user  system elapsed 
#>   0.007   0.000   0.037
# Remote: retrieve results
system.time(flights3 %>% collect())
#>    user  system elapsed 
#>   0.005   0.002   0.048
```

That’s because of the overhead associated with sending the data to each
worker and retrieving the results at the end. For basic dplyr verbs,
multidplyr is unlikely to give you significant speed ups unless you have
10s or 100s of millions of data points (and in that scenario you should
first try [dtplyr](https://dtplyr.tidyverse.org/), which uses
[data.table](https://R-datatable.com/)).

multipldyr might help, however, if you’re doing more complex things.
Let’s see how that plays out when fitting a moderately complex model.
We’ll start by selecting a subset of flights that have at least 50
occurrences, and we’ll compute the day of the year from the date:

``` r
daily_flights <- flights %>%
  count(dest) %>%
  filter(n >= 365)

common_dest <- flights %>% 
  semi_join(daily_flights, by = "dest") %>% 
  mutate(yday = lubridate::yday(ISOdate(year, month, day))) %>% 
  group_by(dest)

nrow(common_dest)
#> [1] 332942
```

That leaves us with ~332,000 observations. Let’s partition this smaller
dataset:

``` r
by_dest <- common_dest %>% partition(cluster)
by_dest
#> Source: party_df [332,942 x 20]
#> Groups: dest
#> Shards: 2 [164,539--168,403 rows]
#> 
#> # A data frame: 332,942 × 20
#>    year month   day dep_time sched_dep_time dep_delay arr_time
#>   <int> <int> <int>    <int>          <int>     <dbl>    <int>
#> 1  2013     1     1      517            515         2      830
#> 2  2013     1     1      533            529         4      850
#> 3  2013     1     1      542            540         2      923
#> 4  2013     1     1      554            558        -4      740
#> 5  2013     1     1      555            600        -5      913
#> 6  2013     1     1      558            600        -2      753
#> # ℹ 332,936 more rows
#> # ℹ 13 more variables: sched_arr_time <int>, arr_delay <dbl>,
#> #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>,
#> #   dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
#> #   minute <dbl>, time_hour <dttm>, yday <dbl>
```

Let’s fit a smoothed generalised additive model to each destination,
estimating how delays vary over the course of the year and within a day.
Note that we need to use
[`cluster_library()`](https://multidplyr.tidyverse.org/reference/cluster_utils.md)
to load the mgcv package on every node. That takes around 3s:

``` r
cluster_library(cluster, "mgcv")
system.time({
  models <- by_dest %>% 
    do(mod = gam(dep_delay ~ s(yday) + s(dep_time), data = .))
})
#>    user  system elapsed 
#>   0.007   0.002   3.447
```

Compared with around 5s doing it locally:

``` r
system.time({
  models <- common_dest %>% 
    group_by(dest) %>% 
    do(mod = gam(dep_delay ~ s(yday) + s(dep_time), data = .))
})
#>    user  system elapsed 
#>   4.447   7.358   3.067
```

The cost of transmitting messages to the nodes is roughly constant, so
the longer the task you’re parallelising, the closer you’ll get to a
linear speed up.
