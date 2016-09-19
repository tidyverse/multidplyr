# An introduction to multidplyr
Hadley Wickham  



multidplyr is a new backend for dplyr. You continue to use the dplyr verbs that you're familiar with, but instead of the computation happening on one core it's spread across multiple cores. You effectively create a local cluster on your computer, and then multidplyr takes care of telling each node what to do.

## Basics

multiplyr is built on the principle that moving data around is expensive so you want to do it as little as possible. The basic sequence of operations is:

1. Call `partition()` to split dataset your dataset across multiple cores.
   This makes a partitioned data frame, or a party df for short.
   
1. Each dplyr verb applied to a party df performs the operation independently
   on each core. It leaves each result on each core, and returns another
   party df.
   
1. When you're done with the expensive operations that need to be 
   done on each core, you call `collect()` to retrieve the data and 
   bring it back to you local computer.
  
Let's see a simple example of that using the `nycflights13::flights`. This dataset contains information for about ~300,000 flights departing New York City in 2013.

We start by loading the packages we need:


```r
library(dplyr)
library(multidplyr)
library(nycflights13)
```
  
Next, we partition the flights data by flight number, compute the average delay per flight, and then collect the results:


```r
flights1 <- partition(flights, flight)
#> Initialising 7 core cluster.
flights2 <- summarise(flights1, dep_delay = mean(dep_delay, na.rm = TRUE))
flights3 <- collect(flights2)
```

The dplyr code looks the same as usual, but behind the scenes things are very different. `flights1` and `flights2` are party dfs. These look like normal data frames, but have an additional attribute: the number of shards. In this example, it tells us that `flights2` is spread across seven nodes, and the size on each node varies from 503 to 609 rows. `partition()` always makes sure a group is kept together on one node.


```r
flights2
#> Source: party_df [3,844 x 2]
#> Shards: 7 [533--579 rows]
#> 
#> # S3: party_df
#>    flight dep_delay
#>     <int>     <dbl>
#> 1       9  16.57237
#> 2      15  10.26431
#> 3      16  -0.25000
#> 4      23  11.26519
#> 5      40  12.51667
#> 6      45   9.14400
#> 7      99  -3.00000
#> 8     103  13.00000
#> 9     109  12.35294
#> 10    112  11.81481
#> # ... with 3,834 more rows
```

## Performance

For this size of data, using a local cluster actually makes performance much slower!


```r
system.time({
  flights %>% 
    partition() %>%
    summarise(mean(dep_delay, na.rm = TRUE)) %>% 
    collect()
})
#>    user  system elapsed 
#>   0.372   0.040   0.843
system.time({
  flights %>% 
    group_by() %>%
    summarise(mean(dep_delay, na.rm = TRUE))
})
#>    user  system elapsed 
#>   0.012   0.000   0.009
```

That's because there's some overhead associated with sending the data to each node and retrieving the results at the end. For basic dplyr verbs, multidplyr is unlikely to give you significant speed ups unless you have 10s or 100s of millions of data points. It might however, if you're doing more complex things with `do()`. Let's see how that plays out.

We'll start by selecting a subset of flights that have at least 50 occurences, and we'll compute the day of the year from the date:


```r
common_dest <- flights %>%
  count(dest) %>%
  filter(n >= 365) %>%
  semi_join(flights, .) %>% 
  mutate(yday = lubridate::yday(ISOdate(year, month, day)))
#> Joining, by = "dest"
dim(common_dest)
#> [1] 332942     20
```

That leaves us with ~332,000 observations. 

This time, instead of allowing multidplyr to create a local cluster, we'll do it ourselves. The `create_cluster()` function provides a convenient wrapper around `parallel::makePSOCKcluster()`. The main different between the two functions is that `create_cluster()` setups your cluster in such a way that it will automatically close down when no objects refer to it.


```r
cluster <- create_cluster(2)
#> Initialising 2 core cluster.
cluster
#> socket cluster with 2 nodes on host 'localhost'
```

If you want, you can use `set_default_cluster()` so that `partition()` will use this cluster by default:


```r
set_default_cluster(cluster)
```

Let's partition our restricted flights data across this cluster:


```r
by_dest <- common_dest %>% 
  partition(dest, cluster = cluster)
by_dest
#> Source: party_df [332,942 x 21]
#> Groups: dest
#> Shards: 2 [166,399--166,543 rows]
#> 
#> # S3: party_df
#>     year month   day dep_time sched_dep_time dep_delay arr_time
#>    <int> <int> <int>    <int>          <int>     <dbl>    <int>
#> 1   2013     1     1     1315           1317        -2     1413
#> 2   2013     1     1     1655           1621        34     1804
#> 3   2013     1     1     2056           2004        52     2156
#> 4   2013     1     2     1332           1327         5     1419
#> 5   2013     1     2     1746           1621        85     1835
#> 6   2013     1     2     2148           2004       104     2234
#> 7   2013     1     3     1716           1619        57     1803
#> 8   2013     1     3     2031           2038        -7     2131
#> 9   2013     1     4     1618           1619        -1     1714
#> 10  2013     1     4     2031           2000        31     2131
#> # ... with 332,932 more rows, and 14 more variables: sched_arr_time <int>,
#> #   arr_delay <dbl>, carrier <chr>, flight <int>, tailnum <chr>,
#> #   origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
#> #   minute <dbl>, time_hour <dttm>, yday <dbl>, PARTITION_ID <dbl>
```

It's always a good idea to check the evenness of the partition - you'll get the most benefit when the rows are roughly even across all of the nodes.

Let's fit a smoothed generalised additive model to each destination, estimating how delays vary over the course of the year and within a day. Note that we need to use `cluster_library()` to load the mgcv package on every node. That takes 3.7s:


```r
cluster_library(by_dest, "mgcv")
system.time({
  models <- by_dest %>% 
    do(mod = gam(dep_delay ~ s(yday) + s(dep_time), data = .))
})
#>    user  system elapsed 
#>   0.000   0.000   2.432
```

Compared with ~5.6s doing it locally:


```r
system.time({
  models <- common_dest %>% 
    group_by(dest) %>% 
    do(mod = gam(dep_delay ~ s(yday) + s(dep_time), data = .))
})
#>    user  system elapsed 
#>   3.496   0.020   3.499
```

That's not a great speed up, but generally you don't care about parallesing things that only take a couple of seconds. The cost of transmitting messages to the nodes is roughly fixed, so the longer the task you're parallelising, the closer to a linear speed up you'll get.  It'll also speed up with more nodes, but unfortunately vignettes are only allowed to use 2 nodes max, so I can't show you that here.

## Limitations

*   For optimal speedup, each node needs to do about the same amount of
    work. That generally means you want to group by a variable that
    divides the data up into many pieces, so each node can get about the
    same amount of data. If multidplyr's default strategy isn't a good
    fit for you data, you may need to make your own grouping variable
    which takes values from 1 to the number of nodes.

*   Currently you have to load all the data into memory in one instance, and
    then it gets partitioned to the individual nodes. If you want to avoid 
    that, you'll need to split the data up yourself, and load it by "hand":

    
    ```r
    cluster <- create_cluster(4)
    cluster_assign_each(cluster, "filename",
      list("a.csv", "b.csv", "c.csv", "d.csv")
    )
    cluster_assign_expr(cluster, "my_data", quote(readr::read_csv(filename)))
    
    my_data <- src_cluster(cluster) %>% tbl("my_data")
    ```

*   Currently you can only use clusters created by the parallel package.
    It is possible to set up these clusters across multiple machines,
    but it is a bit tricky. Hopefully there will soon be a standard API
    for distributed R, and when that happens, multidplyr will be able to
    work with more types of cluster.
