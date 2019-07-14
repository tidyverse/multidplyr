test_that("test that pass through workds", {
  cl <- default_cluster()
  cluster_assign_each(cl, x = list(
    tibble(g = 1, x = 1:2),
    tibble(g = 2, x = 3:4)
  ))
  on.exit(cluster_rm(cl, "x"))
  pf <- party_df(cl, "x")

  expect_equal(pf %>% arrange(desc(x)) %>% collect() %>%  pull(x), c(2, 1, 4, 3))
  expect_equal(pf %>% filter(x %% 2 == 0) %>% collect() %>% pull(x), c(2, 4))
  expect_equal(pf %>% mutate(y = x + 1) %>% collect() %>% pull(y), 2:5)
  expect_equal(pf %>% rename(z = x) %>% collect() %>% pull(z), 1:4)
  expect_equal(pf %>% slice(1) %>% collect() %>% pull(x), c(1, 3))

  expect_equal(pf %>% select(x) %>% collect(), tibble(x = 1:4))

  expect_equal(
    pf %>% group_by(g) %>% summarise(x = sum(x)) %>% collect(),
    tibble(g = c(1, 2), x = c(3L, 7L))
  )
})
