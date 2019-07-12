test_that("calls submitted in parallel", {
  cl <- get_default_cluster()
  out <- cluster_call(cl, {x <- Sys.time(); Sys.sleep(0.2); x})

  rng <- range(as.double(out))
  expect_true(rng[2] - rng[1] < 0.2)
})

test_that("calls submitted to each node", {
  cl <- get_default_cluster()
  pid <- cluster_call(cl, Sys.getpid())
  expect_equal(length(unique(pid)), length(cl))
})

test_that("errors are propagated", {
  cl <- get_default_cluster()
  cnd <- capture_condition(cluster_call(cl, stop("!!")))

  expect_s3_class(cnd, "rlang_error")
  expect_match(cnd$parent$message, "!!", fixed = TRUE)
})

test_that("string function executed on nodes", {
  cl <- get_default_cluster()
  identity <- function(x) 10

  local <- cluster_map(cl[1], identity, 1)
  expect_equal(local, list(10))

  remote <- cluster_map(cl[1], "identity", 1)
  expect_equal(remote, list(1))
})
