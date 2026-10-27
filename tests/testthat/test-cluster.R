test_that("default cluster only creates once", {
  skip_if_no_mizu()
  cl1 <- default_cluster()
  cl2 <- default_cluster()

  expect_identical(cl1, cl2)

  # and errors if you attempt to set n the second time
  expect_snapshot_error(default_cluster(10))
})

test_that("length returns the number of workers", {
  skip_if_no_mizu()
  expect_equal(length(default_cluster()), 2)
})

test_that("cluster has useful print method", {
  skip_if_no_mizu()
  expect_snapshot(default_cluster())
})
