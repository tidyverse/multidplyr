cluster_eval <- function(expr, env = globalenv(), .cl = cluster_get()) {
  cluster_call(eval, expr, env = env, .cl = .cl)
}

cluster_evalq <- function(expr, env = globalenv(), .cl = cluster_get()) {
  cluster_call(eval, substitute(expr), env = env, .cl = .cl)
}

# Always suceeds: only gives warning if object not found, and warnings
# are not syndicated back to master
cluster_rm <- function(name, .cl = cluster_get()) {
  stopifnot(is.character(name))

  cluster_call(rm, list = name, envir = globalenv(), .cl = cluster_get())
  invisible(TRUE)
}

cluster_ls <- function(.cl = cluster_get()) {
  cluster_call(ls, envir = globalenv(),  .cl = cluster_get())
}

cluster_mem <- function(.cl = cluster_get()) {
  l <- cluster_evalq(sum(gc()[, 2]), .cl = .cl)
  unlist(l)
}

# should be cluster - need different names for default cluster management
cluster_assign_expr <- function(name, expr, .cl = cluster_get()) {
  stopifnot(is.character(name))
  stopifnot(is.name(expr) || is.atomic(expr) || is.call(expr))

  # If any fail, need to delete results everywhere
  tryCatch(
    cluster_call(assign_eval, name, expr, .cl = .cl),
    error = function(e) {
      cluster_rm(name, .cl = .cl)
      stop(e)
    }
  )

  invisible(TRUE)
}

cluster_assign_value <- function(name, value, .cl = cluster_get()) {
  stopifnot(is.character(name))

  cluster_call(assign_silent, name = name, value = value, .cl = .cl)
  invisible(TRUE)
}

cluster_assign_each <- function(name, values, .cl = cluster_get()) {
  stopifnot(is.character(name))
  stopifnot(is.list(values), length(values) == length(.cl))

  for (i in seq_along(values)) {
    cluster_assign_value(name, values[[i]], .cl = .cl[i])
  }
  invisible(TRUE)
}

cluster_retrieve <- function(name, .cl = cluster_get()) {
  stopifnot(is.character(name))

  cluster_call(get, name, .cl = .cl)
}

cluster_library <- function(..., .cl = cluster_get()) {
  call <- substitute(library(...))
  invisible(cluster_eval(call, .cl = .cl))
}

# Wrapper for assign that doesn't return value
assign_silent <- function(name, value, env = globalenv()) {
  assign(name, value, envir = env)
  invisible()
}
assign_eval <- function(name, expr, env = globalenv()) {
  assign(name, eval(expr, envir = env), envir = env)
  invisible()
}

