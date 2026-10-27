# nocov start
.onLoad <- function(libname, pkgname) {
  # Worker-side functions must not carry the multidplyr namespace: workers
  # don't have the package installed (and under load_all() the namespace
  # can't be serialised without a warning). Assign into the namespace
  # explicitly: `environment(fn) <- globalenv()` alone would create a local
  # copy in .onLoad's frame
  ns <- asNamespace(pkgname)
  for (fn in c(
    "shard_slice",
    "shard_eval",
    "shard_eval_dual",
    "cluster_map_eval"
  )) {
    assign(fn, `environment<-`(get(fn, envir = ns), globalenv()), envir = ns)
  }

  register_s3_method("dplyr", "filter", "multidplyr_party_df")
  register_s3_method("dplyr", "intersect", "multidplyr_party_df")
  register_s3_method("dplyr", "union", "multidplyr_party_df")
  register_s3_method("dplyr", "setdiff", "multidplyr_party_df")
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
