# mizu workers are fresh Rscript children that library(mizu) from the
# installed library; probe once whether pools work in this environment.
# The probe closure gets globalenv(): helpers are sourced into the package
# env under devtools, which would otherwise serialise the namespace into
# the map descriptor and warn
mizu_probe <- function(x) x
environment(mizu_probe) <- globalenv()

mizu_available <- tryCatch(
  {
    pool <- mizu::mizu_pool(
      1,
      launcher = mizu::mizu_launcher(stdout = FALSE, stderr = FALSE)
    )
    ok <- identical(
      mizu::mizu_collect(mizu::mizu_map(pool, 1, mizu_probe)),
      list(1)
    )
    mizu::mizu_pool_stop(pool)
    ok
  },
  error = function(e) FALSE
)

skip_if_no_mizu <- function() {
  testthat::skip_if_not(mizu_available, "mizu pool not available")
}

if (mizu_available) {
  # Ensure default cluster initiated silently
  suppressMessages(default_cluster())
}
