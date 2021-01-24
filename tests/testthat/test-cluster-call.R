test_that("calls submitted in parallel", {
  cl <- default_cluster()
  out <- cluster_call(cl, {x <- Sys.time(); Sys.sleep(0.2); x})

  rng <- range(as.double(out))
  expect_true(rng[2] - rng[1] < 0.2)
})

test_that("calls submitted to each node", {
  cl <- default_cluster()
  pid <- cluster_call(cl, Sys.getpid())
  expect_equal(length(unique(pid)), length(cl))
})

test_that("can collapse results", {
  cl <- default_cluster()
  out <- cluster_call(cl, 1, ptype = double())
  expect_identical(out, c(1, 1))
})


test_that("errors are propagated", {
  cl <- default_cluster()
  expect_snapshot(cluster_call(cl, stop("!!")), error = TRUE)
})

test_that("call_send() returns cluster", {
  cl <- default_cluster()
  expect_equal(cluster_send(cl, 10), cl)
})
