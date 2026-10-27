# Worker-side functions -----------------------------------------------------
#
# shard_slice(), shard_eval(), shard_eval_dual(), and cluster_map_eval() are
# evaluated on mizu workers, so their environments are reset to `globalenv()`
# in `.onLoad()`: a closure enclosed by the multidplyr namespace would make
# every worker load the namespace (or warn under `load_all()`), and misses
# mizu's compact closure codec. They may only use `::`-qualified calls; no
# multidplyr installation is needed on the workers.
#
# Errors must propagate bare: wrapping the eval in a handler (e.g. tryCatch)
# corrupts rlang/cli errors raised beneath it inside mizu_map_batch's
# evaluation loop (they arrive as "bad value").

shard_slice <- function(i, data, rows) {
  vctrs::vec_slice(data, rows[[i]])
}

shard_eval <- function(
  i,
  shards,
  groups,
  verb,
  quos,
  args,
  bindings,
  packages
) {
  for (pkg in packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  rlang::local_bindings(!!!bindings, .env = globalenv())

  df <- shards[[i]]
  if (length(groups)) {
    df <- dplyr::group_by(df, !!!rlang::syms(groups))
  }
  out <- rlang::eval_bare(rlang::call2(
    verb,
    df,
    !!!quos,
    !!!args,
    .ns = "dplyr"
  ))
  list(data = dplyr::ungroup(out), groups = dplyr::group_vars(out))
}

shard_eval_dual <- function(
  i,
  xs,
  ys,
  groups,
  verb,
  quos,
  args,
  bindings,
  packages
) {
  for (pkg in packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  rlang::local_bindings(!!!bindings, .env = globalenv())

  x <- xs[[i]]
  if (length(groups)) {
    x <- dplyr::group_by(x, !!!rlang::syms(groups))
  }
  y <- if (is.data.frame(ys)) ys else ys[[i]]
  out <- rlang::eval_bare(rlang::call2(
    verb,
    x,
    y,
    !!!quos,
    !!!args,
    .ns = "dplyr"
  ))
  list(data = dplyr::ungroup(out), groups = dplyr::group_vars(out))
}

cluster_map_eval <- function(x, f, args, bindings, packages) {
  for (pkg in packages) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  rlang::local_bindings(!!!bindings, .env = globalenv())

  rlang::exec(f, x, !!!args)
}

# Host-side engine ----------------------------------------------------------

shard_call <- function(.data, .verb, dots, ...) {
  cl <- .data$cluster
  map <- mizu_map_muffled(
    cl$pool,
    .x = seq_along(.data$shards),
    .f = shard_eval,
    shards = .data$shards,
    groups = .data$groups,
    verb = .verb,
    quos = dots,
    args = list(...),
    bindings = cluster_bindings(cl),
    packages = cluster_packages(cl)
  )
  res <- collect_map(map)
  new_party_df(cl, lapply(res, `[[`, "data"), groups = res[[1]]$groups)
}

shard_call_dual <- function(.verb, .x, .y, dots, ...) {
  cl <- .x$cluster
  if (is_party_df(.y)) {
    if (length(.x$shards) != length(.y$shards)) {
      cli::cli_abort(c(
        "`x` and `y` must have the same number of shards.",
        i = "`x` has {length(.x$shards)} shard{?s} and `y` has {length(.y$shards)}.",
        i = "Partition both data frames with the same `n_shards` and grouping."
      ))
    }
    ys <- .y$shards
  } else {
    # A mori-shared local data frame from auto_copy(): every worker maps the
    # same region
    ys <- .y
  }

  map <- mizu_map_muffled(
    cl$pool,
    .x = seq_along(.x$shards),
    .f = shard_eval_dual,
    xs = .x$shards,
    ys = ys,
    groups = .x$groups,
    verb = .verb,
    quos = dots,
    args = list(...),
    bindings = cluster_bindings(cl),
    packages = cluster_packages(cl)
  )
  res <- collect_map(map)
  new_party_df(cl, lapply(res, `[[`, "data"), groups = res[[1]]$groups)
}

# Quosures captured under `load_all()`/testthat chain to the multidplyr
# namespace, which makes R_Serialize warn "may not be available when loading"
# at stage time. Real users' quosures chain to the global environment and
# never trigger it.
mizu_map_muffled <- function(...) {
  withCallingHandlers(
    mizu::mizu_map(...),
    warning = function(w) {
      if (
        grepl(
          "may not be available when loading",
          conditionMessage(w),
          fixed = TRUE
        )
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

collect_map <- function(map) {
  tryCatch(
    mizu::mizu_collect(map),
    mizu_error_worker_died = function(e) {
      cli::cli_abort("Worker died while computing shards.", parent = e)
    },
    error = function(e) {
      idx <- e$mizu_map_index
      if (is.null(idx)) {
        cli::cli_abort("Remote computation failed.", parent = e)
      }
      cli::cli_abort("Remote computation failed in shard {idx}.", parent = e)
    }
  )
}
