# multidplyr (development version)

* multidplyr has been rewritten on top of
  [mizu](https://github.com/shikokuchuo/mizu) (a work-stealing pool of R
  processes over shared memory) and [mori](https://cran.r-project.org/package=mori)
  (zero-copy sharing of R objects). `partition()` no longer copies data in
  the host session: the data is shared once and sliced in parallel on the
  workers. Verb results return as zero-copy views for ungrouped data with
  plain columns, and cross with a single serialization (no deep copy)
  otherwise. This branch cannot be submitted to CRAN until mizu is on CRAN.

* Breaking changes:

  * `cluster_call()`, `cluster_send()`, `cluster_assign_each()`,
    `cluster_assign_partition()`, and subsetting of clusters with `[` have
    been removed: a work-stealing pool gives no per-worker guarantees. Use
    the new `cluster_map()` to evaluate a function over a vector of inputs
    on the workers (e.g. reading one file per shard).
  * `party_df()` now takes a list of data frames (the shards) rather than
    the name of a data frame that exists on every worker.
  * `partition()` gains an `n_shards` argument; by default it creates up to
    4 shards per worker so the pool can balance load dynamically.
  * `cluster_assign()`, `cluster_copy()`, `cluster_library()`, and
    `cluster_rm()` no longer modify worker environments; they maintain a
    registry of values and packages that is made visible to every verb
    evaluation.
  * The `Cleaner`/`auto_rm` shard lifecycle is gone: shard memory is
    reclaimed by garbage collection.


# multidplyr 0.1.4

* Switch to qs2, as qs is being archived (@traversc, #159).

# multidplyr 0.1.3

* Fix for R CMD check failure.

# multidplyr 0.1.2

* `cluster_call()` gains a `simplify` argument - use this to request that
  the result should be simplified (#136).

# multidplyr 0.1.1

* Fixed problems identified as part of working on dplyr 1.0.8.

# multidplyr 0.1.0

* Added a `NEWS.md` file to track changes to the package.
