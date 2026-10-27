test_that("cluster_assign values are visible inside verbs", {
  skip_if_no_mizu()
  cl <- default_cluster()
  cluster_assign(cl, a = 100)
  on.exit(cluster_rm(cl, "a"))

  pf <- partition(tibble(x = 1:4), cl)
  expect_equal(collect(pf %>% mutate(y = x + a))$y, 101:104)
})

test_that("cluster_copy copies values from an environment", {
  skip_if_no_mizu()
  cl <- default_cluster()
  f <- function(x) x * 2
  cluster_copy(cl, "f")
  on.exit(cluster_rm(cl, "f"))

  pf <- partition(tibble(x = 1:4), cl)
  expect_equal(collect(pf %>% mutate(y = f(x)))$y, c(2, 4, 6, 8))
})

test_that("cluster_rm removes values again", {
  skip_if_no_mizu()
  cl <- default_cluster()
  cluster_assign(cl, a = 1)
  cluster_rm(cl, "a")

  pf <- partition(tibble(x = 1), cl)
  expect_snapshot(pf %>% mutate(y = x + a), error = TRUE)
})

test_that("cluster_library makes package functions available in verbs", {
  skip_if_no_mizu()
  cl <- default_cluster()
  cluster_library(cl, "tools")

  pf <- partition(tibble(x = "hello world"), cl)
  expect_equal(collect(pf %>% mutate(y = toTitleCase(x)))$y, "Hello World")
})
