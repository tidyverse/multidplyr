context("Object management")

test_that("assign_expr is idempotent", {
  if (in_check())
    skip("Fails in R CMD check")

  cluster <- create_cluster(2, quiet = TRUE)
  cluster_assign_each(cluster, "x", list("a", 1))
  try(cluster_assign_expr(cluster, "y", quote(log(x))), silent = TRUE)

  vars <- cluster_ls(cluster)
  expect_equal(vars, list("x", "x"))

})
