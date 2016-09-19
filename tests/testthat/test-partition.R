context("partition")


test_that("Partition runs", {
  tmp <- iris %>%
    partition()
})

test_that("Partition runs with low number of groups", {
  cl <- create_cluster(3)
  iris %>%
    partition(Species, cluster = cl)
})

test_that("Partition runs with large number of groups", {
  nycflights13::flights %>%
    partition(flight)
})
