test_that("can construct and print partydf", {
  cl <- get_default_cluster()[1:2]
  cl <- cluster_assign(cl, "x", data.frame(y = 1))

  df <- new_party_df(cl, "x")

  expect_s3_class(df, "party_df")
  expect_equal(df$cluster, cl)
  expect_known_output(print(df), test_path("test-partydf-print.txt"))
})

test_that("can partition and re-collect", {
  cl <- get_default_cluster()[1:2]

  df1 <- data.frame(x = 1:2)
  df2 <- partition(df1, .cluster = cl)

  expect_s3_class(df2, "party_df")
  expect_equal(df2$cluster, cl)

  expect_equal(collect(df2), df1)
})

test_that("can partition by group", {
  cl <- get_default_cluster()[1:2]
  df1 <- tibble(x = c(rep(1, 2), rep(2, 1), rep(3, 1)))
  df2 <- partition(df1, x, .cluster = cl)

  dfs <- cluster_get(cl, as.character(df2$name))
  expect_equal(dfs, list(tibble(x = c(1, 1)), tibble(x = c(2, 3))))
})
