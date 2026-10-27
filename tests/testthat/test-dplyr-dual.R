test_that("joining data frame requires explicit copy", {
  skip_if_no_mizu()
  pf <- partition(data.frame(x = 1:6), default_cluster())
  df <- data.frame(x = 1:3, y = 3:1)

  expect_snapshot(error = TRUE, {
    left_join(pf, df)
  })
  expect_error(left_join(pf, df, copy = TRUE), NA)
})

test_that("joins match local results", {
  skip_if_no_mizu()
  pf1 <- partition(tibble(x = c(1, 2)), default_cluster())
  pf2 <- partition(tibble(x = c(1, 3)), default_cluster())

  # primarily testing that basic infrastructure works and that
  # I haven't accidentally typed the wrong verb name somewhere
  expect_equal(pf1 %>% inner_join(pf2, by = "x") %>% pull(x), 1)
  expect_equal(pf1 %>% left_join(pf2, by = "x") %>% pull(x), c(1, 2))
  expect_equal(pf1 %>% right_join(pf2, by = "x") %>% pull(x), c(1, 3))
  expect_equal(pf1 %>% full_join(pf2, by = "x") %>% pull(x), c(1, 2, 3))

  expect_equal(pf1 %>% semi_join(pf2, by = "x") %>% pull(x), 1)
  expect_equal(pf1 %>% anti_join(pf2, by = "x") %>% pull(x), 2)
})

test_that("set operations match local results", {
  skip_if_no_mizu()
  pf1 <- partition(tibble(x = c(1, 2)), default_cluster())
  pf2 <- partition(tibble(x = c(1, 3)), default_cluster())

  expect_equal(pf1 %>% intersect(pf2) %>% pull(), 1)
  expect_setequal(pf1 %>% union(pf2) %>% pull(), c(1, 2, 3))
  expect_equal(pf1 %>% union_all(pf2) %>% pull() %>% sort(), c(1, 1, 2, 3))
  expect_equal(pf1 %>% setdiff(pf2) %>% pull(), 2)
})

test_that("joining party_dfs needs matching shard counts", {
  skip_if_no_mizu()
  pf1 <- partition(tibble(x = 1:6), default_cluster(), n_shards = 2)
  pf2 <- partition(tibble(x = 1:6), default_cluster(), n_shards = 3)

  expect_snapshot(error = TRUE, inner_join(pf1, pf2, by = "x"))
})
