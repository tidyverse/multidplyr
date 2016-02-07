# An introduction to multidplyr
Hadley Wickham  



multidplyr is a new backend for dplyr. You continue to use the dplyr verbs that you're familiar with, but instead of the computation happening on one core it's spread across multiple cores. You effectively create a local cluster on your computer, and then multidplyr takes care of telling each node what to do.

## Basics

multiplyr is built on the principle that moving data around is expensive so you want to do it as little as possible. The basic sequence of operations is:

1. Call `partition()` to split your dataset across multiple cores.
   This makes a partitioned data frame, or a party df for short.
   
1. Each dplyr verb applied to a party df performs the operation independently
   on each core. It leaves each result on each core, and returns another
   party df.
   
1. When you're done with the expensive operations that need to be 
   done on each core, you call `collect()` to retrieve the data and 
   bring it back to your local computer.
  
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
#> Shards: 7 [482--603 rows]
#> 
#>    flight  dep_delay
#>     (int)      (dbl)
#> 1       1  5.2932761
#> 2      16 -0.2500000
#> 3      19 10.0500000
#> 4      28 13.6000000
#> 5      32 11.7884615
#> 6      40 12.5166667
#> 7      46  0.0000000
#> 8      49 -0.4827586
#> 9      59  3.6527197
#> 10     65  7.6979167
#> ..    ...        ...
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
#>   0.471   0.069   0.725
system.time({
  flights %>% 
    group_by() %>%
    summarise(mean(dep_delay, na.rm = TRUE))
})
#>    user  system elapsed 
#>   0.007   0.000   0.007
```

That's because there's some overhead associated with sending the data to each node and retrieving the results at the end. For basic dplyr verbs, multidplyr is unlikely to give you significant speed ups unless you have 10s or 100s of millions of data points. It might however, if you're doing more complex things with `do()`. Let's see how that plays out.

We'll start by selecting a subset of flights that have at least 50 occurences, and we'll compute the day of the year from the date:


```r
common_dest <- flights %>%
  count(dest) %>%
  filter(n >= 365) %>%
  semi_join(flights, .) %>% 
  mutate(yday = lubridate::yday(ISOdate(year, month, day)))
#> Joining by: "dest"
dim(common_dest)
#> [1] 332942     17
```

That leaves us with ~332,000 observations. 

This time, instead of allowing multidplyr to create a local cluster, we'll do it ourselves. The `create_cluster()` function provides a convenient wrapper around `parallel::makePSOCKcluster()`. The main difference between the two functions is that `create_cluster()` sets up your cluster in such a way that it will automatically close down when no objects refer to it.


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
#> Source: party_df [332,942 x 17]
#> Groups: dest
#> Shards: 2 [154,965--177,977 rows]
#> 
#>     year month   day dep_time dep_delay arr_time arr_delay carrier tailnum
#>    (int) (int) (int)    (int)     (dbl)    (int)     (dbl)   (chr)   (chr)
#> 1   2013     1     1      554        -6      812       -25      DL  N668DN
#> 2   2013     1     1      600         0      837        12      MQ  N542MQ
#> 3   2013     1     1      606        -4      837        -8      DL  N3739P
#> 4   2013     1     1      615         0      833        -9      DL  N326NB
#> 5   2013     1     1      658        -2      944         5      DL  N6703D
#> 6   2013     1     1      754        -5     1039        -2      DL  N935DL
#> 7   2013     1     1      807        -3     1043         0      DL  N308DE
#> 8   2013     1     1      814         4     1047        17      FL  N977AT
#> 9   2013     1     1      830        -5     1052       -13      MQ  N513MQ
#> 10  2013     1     1      855        -4     1143        -2      DL  N646DL
#> ..   ...   ...   ...      ...       ...      ...       ...     ...     ...
#> Variables not shown: flight (int), origin (chr), dest (chr), air_time
#>   (dbl), distance (dbl), hour (dbl), minute (dbl), yday (dbl).
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
#>   0.001   0.000   3.973
```

Compared with ~5.6s doing it locally:


```r
system.time({
  models <- common_dest %>% 
    group_by(dest) %>% 
    do(mod = gam(dep_delay ~ s(yday) + s(dep_time), data = .))
})
#>    user  system elapsed 
#>   4.924   0.639   5.565
```

That's not a great speed up, but generally you don't care about parallelising things that only take a couple of seconds. The cost of transmitting messages to the nodes is roughly fixed, so the longer the task you're parallelising, the closer to a linear speed up you'll get.  It'll also speed up with more nodes, but unfortunately vignettes are only allowed to use 2 nodes max, so I can't show you that here.

## Limitations

*   For optimal speedup, each node needs to do about the same amount of
    work. That generally means you want to group by a variable that
    divides the data up into many pieces, so each node can get about the
    same amount of data. If multidplyr's default strategy isn't a good
    fit for your data, you may need to make your own grouping variable
    which takes values from 1 to the number of nodes.

*   Currently you have to load all the data into memory in one instance, and
    then it gets partitioned to the individual nodes. If you want to avoid 
    that, you'll need to split the data up yourself, and load it by "hand":

    
    ```r
    cluster <- create_cluster(4)
    cluster_assign_each(cluster, "filename",
      list("a.csv", "b.csv", "c.csv", "d.csv")
    )
    cluster_assign_expr(cluster, "my_data", readr::read_csv(filename))
    
    my_data <- src_cluster(cluster) %>% tbl("my_data")
    ```

*   Currently you can only use clusters created by the parallel package.
    It is possible to set up these clusters across multiple machines,
    but it is a bit tricky. Hopefully there will soon be a standard API
    for distributed R, and when that happens, multidplyr will be able to
    work with more types of clusters.
