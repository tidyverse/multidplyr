# multidplyr

[![Travis-CI Build Status](https://travis-ci.org/hadley/multidplyr.svg?branch=master)](https://travis-ci.org/hadley/multidplyr)
[![Coverage Status](https://img.shields.io/codecov/c/github/hadley/multidplyr/master.svg)](https://codecov.io/github/hadley/multidplyr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/multidplyr)](http://cran.r-project.org/package=multidplyr)

multidplyr is a backend for dplyr that partitions a data frame across multiple cores. You tell multidplyr how to split the data up with `partition()` and then the data stays on each node until you explicitly retrieve it with `collect()`. This minimises the amount of time spent moving data around, and maximises parallel performance. This idea is inspired by [partools](http://bit.ly/1Nve8v5) by Norm Matloff and [distributedR](http://bit.ly/1KZVAwK) by the Vertica Analytics team.

Due to the overhead associated with communicating between the nodes, you won't expect to see much performance improvement on basic dplyr verbs with less than ~10 million observations. However, you'll see improvements much faster if you're doing more complex operations with `do()`.

To learn more, read the [vignette](vignettes/multidplyr.md).

## Installation

To install from GitHub:

```R
# install.packages("devtools")
devtools::install_github("hadley/multidplyr")
```
