test_that("results come back in input order", {
  skip_if_no_mizu()
  cl <- default_cluster()
  expect_equal(cluster_map(cl, 4:1, function(x) x^2), list(16, 9, 4, 1))
})

test_that("errors carry the element index", {
  skip_if_no_mizu()
  cl <- default_cluster()
  expect_snapshot(
    cluster_map(cl, 1:3, function(x) if (x == 2) stop("boom") else x),
    error = TRUE
  )
})

test_that("registry values are visible inside .f", {
  skip_if_no_mizu()
  cl <- default_cluster()
  cluster_assign(cl, y = 10)
  on.exit(cluster_rm(cl, "y"))

  expect_equal(cluster_map(cl, 1:2, function(x) x + y), list(11, 12))
})

test_that("registry packages are visible inside .f", {
  skip_if_no_mizu()
  cl <- default_cluster()
  cluster_library(cl, "tools")

  expect_equal(
    cluster_map(cl, "hello world", function(x) toTitleCase(x)),
    list("Hello World")
  )
})
