test_that("joining data frame requires explicit copy", {
  pf <- partition(data.frame(x = 1:6))
  df <- data.frame(x = 1:3, y = 3:1)

  expect_error(left_join(pf, df), "same src")
  expect_error(left_join(pf, df, copy = TRUE), NA)
})
