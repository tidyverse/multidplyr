test_that("can set/get/remove/list", {
  cl <- default_cluster()[1]
  cluster_assign(cl, x = 1)

  expect_true("x" %in% cluster_ls(cl)[[1]])
  expect_equal(cluster_call(cl, x), list(1))

  cluster_rm(cl, "x")
  expect_false("x" %in% cluster_ls(cl)[[1]])
})

test_that("can assign different values to different clusters", {
  cl <- default_cluster()

  cluster_assign_each(cl, x = 1:2)
  expect_equal(cluster_call(cl, x), list(1L, 2L))
  cluster_rm(cl, "x")
})

test_that("can copy from objects to cluster", {
  cl <- default_cluster()[1]
  x <- 1
  y <- 2

  cluster_copy(cl, c("x", "y"))
  expect_equal(cluster_call(cl, x)[[1]], x)
  expect_equal(cluster_call(cl, y)[[1]], y)

  cluster_rm(cl, c("x", "y"))
})

test_that("can load package", {
  cl <- new_cluster(1)

  cluster_library(cl, "covr")
  expect_true("package:covr" %in% cluster_call(cl, search())[[1]])
})
