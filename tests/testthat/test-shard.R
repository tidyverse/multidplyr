test_that("can distribute across multiple workers", {
  cl <- get_default_cluster()[1:2]

  df1 <- data.frame(x = 1:2)
  df2 <- partition(df1, .cluster = cl)

  expect_s3_class(df2, "party_df")
  expect_equal(df2$cluster, cl)

  expect_equal(collect(df2), df1)
})

