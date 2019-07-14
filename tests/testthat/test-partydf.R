test_that("can construct and print partydf", {
  cl <- get_default_cluster()
  cl <- cluster_assign(cl, "x", data.frame(y = 1:10))

  df <- party_df(cl, "x")
  on.exit(cluster_rm(cl, "x"))

  expect_s3_class(df, "party_df")
  expect_equal(df$cluster, cl)
  expect_known_output(print(df), test_path("test-partydf-print.txt"))
})

test_that("name must be data frame with same names", {
  cl <- get_default_cluster()
  expect_error(party_df(cl, "x"), "does not exist")

  cluster_assign(cl, "x", 1)
  on.exit(cluster_rm(cl, "x"))
  expect_error(party_df(cl, "x"), "not a data frame")

  cluster_assign_each(cl, "x", list(tibble(x = 1), tibble(y = 2)))
  expect_error(party_df(cl, "x"), "same names")
})

# partitioning ------------------------------------------------------------

test_that("can partition and re-collect", {
  cl <- get_default_cluster()

  df1 <- data.frame(x = 1:2)
  df2 <- partition(df1, cl)

  expect_s3_class(df2, "party_df")
  expect_equal(df2$cluster, cl)

  expect_equal(collect(df2), df1)
  expect_equal(pull(df2, x), df1$x)
})

test_that("can partition by group", {
  cl <- get_default_cluster()
  df1 <- tibble(x = c(rep(1, 2), rep(2, 1), rep(3, 1)))
  df2 <- df1 %>% group_by(x) %>% partition(cl)

  dfs <- cluster_get(cl, as.character(df2$name))
  expect_equal(dfs, list(tibble(x = c(1, 1)), tibble(x = c(2, 3))))
})

test_that("reduce cluster size if needed", {
  cl <- get_default_cluster()
  df1 <- tibble(x = c(rep(1, 2)))
  expect_message(
    df2 <- df1 %>% group_by(x) %>% partition(cl),
    "partial cluster"
  )

  expect_equal(length(df2$cluster), 1)
})
