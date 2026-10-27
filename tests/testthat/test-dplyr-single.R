test_that("verbs pass through to shards", {
  skip_if_no_mizu()
  cl <- default_cluster()
  cluster_library(cl, "dplyr")

  pf <- party_df(cl, list(tibble(g = 1, x = 1:2), tibble(g = 2, x = 3:4)))

  expect_equal(pf %>% arrange(desc(x)) %>% collect() %>% pull(x), c(2, 1, 4, 3))
  expect_equal(pf %>% filter(x %% 2 == 0) %>% collect() %>% pull(x), c(2, 4))
  expect_equal(pf %>% mutate(y = x + 1) %>% collect() %>% pull(y), 2:5)
  expect_equal(pf %>% rename(z = x) %>% collect() %>% pull(z), 1:4)
  expect_equal(pf %>% slice(1) %>% collect() %>% pull(x), c(1, 3))

  expect_equal(pf %>% select(x) %>% collect(), tibble(x = 1:4))

  expect_equal(pf %>% do(y = 1) %>% collect(), tibble(y = list(1, 1)))

  expect_equal(
    pf %>% group_by(g) %>% summarise(x = sum(x)) %>% collect(),
    tibble(g = c(1, 2), x = c(3L, 7L))
  )
})

test_that("ungroup is local and verbs follow", {
  skip_if_no_mizu()
  cl <- default_cluster()

  pf <- party_df(
    cl,
    list(tibble(g = 1, x = 1:2), tibble(g = 2, x = 3:4)),
    groups = "g"
  )
  pf2 <- ungroup(pf)
  expect_equal(group_vars(pf2), character())
  expect_equal(collect(pf2 %>% mutate(y = x + 1))$y, 2:5)
})

test_that("an error in one shard aborts the verb", {
  skip_if_no_mizu()
  cl <- default_cluster()

  pf <- partition(tibble(x = 1:10), cl)
  expect_snapshot(pf %>% mutate(y = stop("boom")), error = TRUE)
})

test_that("tryCatch inside verb expressions works", {
  skip_if_no_mizu()
  cl <- default_cluster()

  # regression: rlang errors inside a handler corrupted on workers before
  # mizu's srcref-strip fix (arrived as "bad value")
  pf <- partition(tibble(x = 1:4), cl)
  out <- pf %>%
    mutate(
      y = tryCatch(
        {
          rlang::abort("boom")
        },
        error = function(e) 0
      )
    )
  expect_equal(collect(out)$y, rep(0, 4))
})
