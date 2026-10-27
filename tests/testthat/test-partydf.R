test_that("can partition and re-collect", {
  skip_if_no_mizu()
  cl <- default_cluster()

  df1 <- tibble::tibble(x = 1:10, y = letters[1:10])
  df2 <- partition(df1, cl)

  expect_s3_class(df2, "multidplyr_party_df")
  expect_equal(df2$cluster, cl)

  # contiguous shards preserve row order
  expect_equal(collect(df2), df1)
  expect_equal(pull(df2, x), df1$x)
})

test_that("groups stay within a single shard", {
  skip_if_no_mizu()
  cl <- default_cluster()

  df1 <- tibble(g = rep(1:3, c(2, 1, 1)), x = 1:4)
  pf <- df1 %>% group_by(g) %>% partition(cl)

  expect_equal(length(unique(unlist(lapply(pf$shards, function(s) s$g)))), 3)
  expect_equal(
    collect(pf %>% summarise(m = mean(x))) %>% arrange(g),
    df1 %>% group_by(g) %>% summarise(m = mean(x)) %>% arrange(g)
  )
})

test_that("n_shards controls the shard count", {
  skip_if_no_mizu()
  cl <- default_cluster()
  df <- tibble(x = 1:100)

  expect_equal(length(partition(df, cl)$shards), 4 * length(cl))
  expect_equal(length(partition(df, cl, n_shards = 3)$shards), 3)
  expect_equal(length(partition(df, cl, n_shards = 1)$shards), 1)
  expect_equal(length(partition(df, cl, n_shards = 200)$shards), 100)
  expect_snapshot(partition(df, cl, n_shards = 0), error = TRUE)
})

test_that("zero-row frames keep their columns", {
  skip_if_no_mizu()
  cl <- default_cluster()
  df <- tibble(x = integer(), y = character())

  pf <- partition(df, cl)
  expect_equal(dim(pf), c(0, 2))
  expect_equal(names(collect(pf)), c("x", "y"))
})

test_that("party_df validates its shards", {
  skip_if_no_mizu()
  cl <- default_cluster()

  expect_snapshot(error = TRUE, {
    party_df(cl, list(1))
    party_df(cl, list(tibble(x = 1), tibble(y = 1)))
    party_df(cl, list(group_by(tibble(g = 1), g), tibble(g = 1)))
    party_df(cl, list(group_by(tibble(g = 1), g)), groups = "x")
    party_df(cl, list(tibble(x = 1)), groups = "g")
  })
})

test_that("party_df inherits grouping from grouped shards", {
  skip_if_no_mizu()
  cl <- default_cluster()

  pf <- party_df(cl, list(group_by(tibble(g = 1, x = 1), g)))
  expect_equal(group_vars(pf), "g")
})

test_that("metadata is computed locally", {
  skip_if_no_mizu()
  cl <- default_cluster()

  pf <- partition(tibble(x = 1:10, y = 2), cl)
  expect_equal(dim(pf), c(10, 2))
  expect_equal(as.character(tbl_vars(pf)), c("x", "y"))

  pf2 <- pf %>% group_by(x)
  expect_equal(group_vars(pf2), "x")
  expect_equal(group_vars(ungroup(pf2)), character())
  expect_equal(group_vars(ungroup(pf %>% group_by(x, y), y)), "x")
})

test_that("head slices shards before binding", {
  skip_if_no_mizu()
  cl <- default_cluster()
  df <- tibble(x = 1:100)

  pf <- partition(df, cl)
  expect_equal(head(pf), head(df))
})

test_that("can print partydf", {
  skip_if_no_mizu()
  cl <- default_cluster()
  pf <- partition(tibble(y = 1:20), cl)

  expect_snapshot(pf)
  expect_snapshot(pf %>% group_by(y))
})

test_that("factor, Date and POSIXct columns round-trip", {
  skip_if_no_mizu()
  cl <- default_cluster()

  df <- tibble(
    f = factor(rep(letters[1:2], 50)),
    d = rep(as.Date("2020-01-01") + 0:1, 50),
    t = rep(as.POSIXct("2020-01-01", tz = "UTC") + 0:1 * 3600, 50),
    x = rnorm(100)
  )
  pf <- partition(df, cl)
  expect_equal(collect(pf), df)
  expect_equal(collect(pf %>% mutate(y = x + 1)), df %>% mutate(y = x + 1))
})

test_that("character-keyed grouped pipeline matches local", {
  skip_if_no_mizu()
  cl <- default_cluster()

  df <- tibble(g = rep(letters[1:5], 20), x = rnorm(100))
  pf <- df %>% group_by(g) %>% partition(cl)
  expect_equal(
    collect(pf %>% mutate(y = x * 2) %>% summarise(m = mean(y))) %>% arrange(g),
    df %>%
      group_by(g) %>%
      mutate(y = x * 2) %>%
      summarise(m = mean(y)) %>%
      arrange(g)
  )
})

test_that("ungrouped numeric shards cross by reference", {
  skip_if_no_mizu()
  cl <- default_cluster()

  df <- data.frame(x = runif(1e5), y = runif(1e5))
  pf <- partition(df, cl)
  # view-backed shards serialise as tiny REF descriptors
  expect_lt(length(serialize(pf$shards, NULL)), 10000)

  pf2 <- pf %>% mutate(z = x + 1)
  expect_lt(length(serialize(pf2$shards, NULL)), 10000)
})

test_that("dropping a party_df leaves the pool healthy", {
  skip_if_no_mizu()
  cl <- default_cluster()

  pf <- partition(data.frame(x = runif(1e5)), cl)
  rm(pf)
  gc()

  pf2 <- partition(tibble(x = 1:4), cl) %>% mutate(y = x + 1)
  expect_equal(collect(pf2)$y, 2:5)
})

test_that("many live party_dfs still compute correctly", {
  skip_if_no_mizu()
  cl <- default_cluster()

  dfs <- lapply(1:6, function(i) data.frame(x = runif(1e3) + i))
  pfs <- lapply(dfs, function(d) partition(d, cl) %>% mutate(y = x * 2))
  for (i in seq_along(pfs)) {
    expect_equal(collect(pfs[[i]])$y, dfs[[i]]$x * 2)
  }
})
