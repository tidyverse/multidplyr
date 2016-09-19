context("Spread evenly")

gen_groups <- function(){
  n_groups <- sample(15:10000, 1)
  dplyr::data_frame(
    id = seq_len(n_groups),
    n = sample(15:4000, n_groups, replace = TRUE),
    part_id = NA_real_
  )
}

test_that("spread_evenly runs for a variety of different situations", {
  for(seed in 1:20){
    set.seed(seed)
    m <- sample(4:32, 1)
    groups <- gen_groups()
    spread_even <- groups %>%
      spread_evenly(m)
  }
})

test_that("Spreading evenly actually improves a random set of groups.", {
  groups <- gen_groups()
  m <- sample(4:32, 1)
  spread_even <- groups %>%
    spread_evenly(m)

  random <- groups %>%
    dplyr::mutate(part_id = sample(1:m, NROW(.), replace = TRUE))
  objective <- random %>%
    dplyr::group_by(part_id) %>%
    dplyr::summarize(n = sum(n)) %>%
    dplyr::mutate(diff_from_average = n - mean(n)) %>%
    .$diff_from_average %>% abs() %>%
    sum()

  objective_spread <- spread_even %>%
    dplyr::group_by(part_id) %>%
    dplyr::summarize(n = sum(n)) %>%
    dplyr::mutate(diff_from_average = n - mean(n)) %>%
    .$diff_from_average %>% abs() %>%
    sum()
  expect_gt(objective, objective_spread)
})

test_that("Spreading evenly preserves groups and group sizes", {
  groups <- gen_groups()
  m <- sample(4:32, 1)
  spread_even <- groups %>%
    spread_evenly(m)

  groups <- groups %>%
    dplyr::ungroup() %>%
    dplyr::select(-part_id) %>%
    dplyr::arrange(id)
  spread_even <- spread_even %>%
    dplyr::ungroup() %>%
    dplyr::select(-part_id) %>%
    dplyr::arrange(id)
  expect_equal(groups, spread_even)
})
