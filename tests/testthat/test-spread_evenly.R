context("Spread evenly")

m <- sample(4:32, 1)
n_groups <- sample(15:10000, 1)
groups <- dplyr::data_frame(
  id = seq_len(n_groups),
  n = sample(15:4000, n_groups, replace = TRUE),
  part_id = NA_real_
)
spread_even <- groups %>%
  spread_evenly(m)

test_that("Spreading evenly actually improves a random set of groups.", {
  random <- groups %>%
    dplyr::mutate(part_id = sample(1:m, NROW(.), replace = TRUE))
  objective <- random %>%
    group_by(part_id) %>%
    summarize(n = sum(n)) %>%
    dplyr::mutate(diff_from_average = n - mean(n)) %>%
    .$diff_from_average %>% abs() %>%
    sum()

  objective_spread <- spread_even %>%
    group_by(part_id) %>%
    summarize(n = sum(n)) %>%
    dplyr::mutate(diff_from_average = n - mean(n)) %>%
    .$diff_from_average %>% abs() %>%
    sum()
  expect_gt(objective, objective_spread)
})

test_that("Spreading evenly preserves groups and group sizes", {
  groups <- groups %>% ungroup() %>% select(-part_id) %>% arrange(id)
  spread_even <- spread_even %>% ungroup() %>% select(-part_id) %>% arrange(id)
  expect_equal(groups, spread_even)
})
