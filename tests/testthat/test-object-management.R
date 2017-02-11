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

test_that("cluster usage is optimized", {

  comb <- tibble::tribble(
    ~clusters,          ~groups_n,
    seq_len(3),    c(1, 1, 1, 5),
    seq_len(4),    c(1, 1, 1, 4, 2),
    seq_len(5),    c(1, 1, 1, 4, 2),
    c(3, 4, 5, 6), c(1, 1, 1, 4, 2)
  )

  for (i in seq_along(comb$clusters)){
    expect_equal(
      grouping_part(comb$clusters[[i]], comb$groups_n[[i]]) %>%
        unique %>%
        length,
      min(comb$clusters[[i]] %>% length, comb$groups_n[[i]] %>% length)
    )

    expect_equal(
      grouping_part(comb$clusters[[i]], comb$groups_n[[i]]) %>%
        unique %>%
        sort,
      comb$clusters[[i]] %>%
        sort
    )

  }
})
