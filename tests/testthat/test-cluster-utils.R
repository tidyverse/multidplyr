test_that("can set/get/remove/list", {
  cl <- get_default_cluster()
  cluster_assign(cl, "x", 1)

  expect_true("x" %in% cluster_ls(cl[1])[[1]])
  expect_equal(cluster_get(cl[1], "x")[[1]], 1)

  cluster_rm(cl, "x")
  expect_false("x" %in% cluster_ls(cl[1])[[1]])
})

test_that("can assign different values to different clusters", {
  cl <- get_default_cluster()
  vals <- as.list(seq_along(cl))

  cluster_assign_each(cl, "x", vals)
  expect_equal(cluster_get(cl, "x"), vals)
  cluster_rm(cl, "x")
})

test_that("can load package", {
  cl <- new_cluster(1)

  cluster_library(cl, "covr")
  expect_true("package:covr" %in% cluster_call(cl, search())[[1]])
})
