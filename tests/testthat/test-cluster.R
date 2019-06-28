test_that("default cluster only creates once", {
  cl1 <- get_default_cluster()
  cl2 <- get_default_cluster()

  expect_identical(cl1, cl2)
})

test_that("subsetting cluster returns cluster", {
  expect_s3_class(get_default_cluster()[1], "multidplyr_cluster")
})
