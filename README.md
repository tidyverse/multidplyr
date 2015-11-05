# dparty

dparty is a backend for dplyr that partitions a data frame across multiple cores. You tell dparty how to split the data up with `partition()` and then the data stays on each node until you explicitly retrieve it with `collect()`.

Due to the overhead associated with communicating between the nodes, you won't expect to see much performance improvement on basic dplyr verbs with less than ~10 million observations. However, you'll see improvements much faster if you're doing more complex operations with `do()`.

## Installation

To install from GitHub:

```R
# install.packages("devtools")
devtools::install_github("hadley/dparty")
```

## Examples

```R
library(dplyr)
library(dparty)
library(nycflights13)

# This creates a local cluster and then evenly shares
# the dataframe over the nodes
flights2 <- flights %>% partition()
flights2

# Operations on a partitioned df (or party_df for short) 
# are executed simultaneously on each node. 
flights %>% mutate(delta = dep_delay - arr_delay)

# The data is left on nodes until you explicitly retrieve it
collect(flights2)
```
