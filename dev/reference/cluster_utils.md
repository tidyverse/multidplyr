# Cluster utitility functions

These functions provide useful helpers for performaning common
operations. \`cluster_assign()\` assigns the same value on each worker;
\`cluster_assign_each()\` assigns different values on each worker;
\`cluster_assign_partition()\` partitions vectors so that each worker
gets (approximately) the same number of pieces.

## Usage

``` r
cluster_assign(.cluster, ...)

cluster_assign_each(.cluster, ...)

cluster_assign_partition(.cluster, ...)

cluster_copy(cluster, names, env = caller_env())

cluster_rm(cluster, names)

cluster_library(cluster, packages)
```

## Arguments

- ...:

  Name-value pairs

- cluster, .cluster:

  Cluster to work on

- names:

  Name of variables to copy.

- env:

  Environment in which to look for varibles to copy.

- packages:

  Character vector of packages to load

## Value

Functions that modify the worker environment invisibly return
\`cluster\` so calls can be piped together. The other functions return
lists with one element for each worker.

## Examples

``` r
cl <- default_cluster()
cluster_assign(cl, a = runif(1))
cluster_call(cl, a)
#> [[1]]
#> [1] 0.007399441
#> 
#> [[2]]
#> [1] 0.007399441
#> 

# Assign different values on each cluster
cluster_assign_each(cl, b = c(1, 10))
cluster_call(cl, b)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 10
#> 

# Partition a vector so that each worker gets approximately the
# same amount of it
cluster_assign_partition(cl, c = 1:11)
cluster_call(cl, c)
#> [[1]]
#> [1] 1 2 3 4 5 6
#> 
#> [[2]]
#> [1]  7  8  9 10 11
#> 

# If you want different to compute different values on each
# worker, use `cluster_call()` directly:
cluster_call(cl, d <- runif(1))
#> [[1]]
#> [1] 0.6378081
#> 
#> [[2]]
#> [1] 0.6799094
#> 
cluster_call(cl, d)
#> [[1]]
#> [1] 0.6378081
#> 
#> [[2]]
#> [1] 0.6799094
#> 

# cluster_copy() is a useful shortcut
e <- 10
cluster_copy(cl, "e")

cluster_call(cl, ls())
#> [[1]]
#> [1] "a" "b" "c" "d" "e" "x"
#> 
#> [[2]]
#> [1] "a" "b" "c" "d" "e" "x"
#> 
cluster_rm(cl, letters[1:5])
cluster_call(cl, ls())
#> [[1]]
#> [1] "x"
#> 
#> [[2]]
#> [1] "x"
#> 

# Use cluster_library() to load packages
cluster_call(cl, search())
#> [[1]]
#>  [1] ".GlobalEnv"        "package:stats"     "package:graphics" 
#>  [4] "package:grDevices" "package:utils"     "package:datasets" 
#>  [7] "package:methods"   "Autoloads"         "tools:callr"      
#> [10] "package:base"     
#> 
#> [[2]]
#>  [1] ".GlobalEnv"        "package:stats"     "package:graphics" 
#>  [4] "package:grDevices" "package:utils"     "package:datasets" 
#>  [7] "package:methods"   "Autoloads"         "tools:callr"      
#> [10] "package:base"     
#> 
cluster_library(cl, "magrittr")
cluster_call(cl, search())
#> [[1]]
#>  [1] ".GlobalEnv"        "package:magrittr"  "package:stats"    
#>  [4] "package:graphics"  "package:grDevices" "package:utils"    
#>  [7] "package:datasets"  "package:methods"   "Autoloads"        
#> [10] "tools:callr"       "package:base"     
#> 
#> [[2]]
#>  [1] ".GlobalEnv"        "package:magrittr"  "package:stats"    
#>  [4] "package:graphics"  "package:grDevices" "package:utils"    
#>  [7] "package:datasets"  "package:methods"   "Autoloads"        
#> [10] "tools:callr"       "package:base"     
#> 
```
