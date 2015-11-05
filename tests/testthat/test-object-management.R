context("Object management")

test_that("assign_expr is idempotent", {
  cl <- create_cluster(2)

  cluster_assign_each(cl, "x", list("a", 1))
  try(cluster_assign_expr(cl, "y", quote(log(x))), silent = TRUE)

  vars <- cluster_ls(cl)
  expect_equal(vars, list("x", "x"))
})
