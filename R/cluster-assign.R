#' Object management
#'
#' If \code{cluster_assign_expr} fails on one node, successfully created
#' objects will be automatically deleted on all the other nodes.
#'
#' @param cluster Cluster to work on
#' @param name Name of variable as string.
#' @param value,values,expr There are three ways to do assignment:
#'   you can set \code{name} to the same \code{value} on every node,
#'   you can set \code{name} to each of \code{values}, or you
#'   can evaluate \code{expr} once on each code.
#' @param obj An existing object to copy to the cluster.
#' @name objman
#' @return \code{cluster_assign_value}, \code{cluster_assign_each},
#'   \code{cluster_assign_expr}, and \code{cluster_rm} all (invisibly) return
#'   the cluster so they can be chained together.
#'
#'   \code{cluster_ls} and \code{cluster_get} return lists with one element
#'   for each node.
#' @examples
#' cl <- create_cluster(2)
#' cl %>%
#'   cluster_assign_value("x", 10) %>%
#'   cluster_get("x")
#'
#' cl %>%
#'   cluster_assign_expr("y", quote(runif(2))) %>%
#'   cluster_get("y")
#'
#' cl %>%
#'   cluster_assign_each("z", list(3, 4)) %>%
#'   cluster_get("z")
#'
#' a <- 1:10
#' cl %>%
#'   cluster_copy(a) %>%
#'   cluster_get("a")
#'
#' cl %>% cluster_ls()
#' cl %>% cluster_rm(c("a", "x", "y", "z"))
#' cl %>% cluster_ls()
NULL

#' @rdname objman
#' @export
cluster_assign_expr <- function(cluster, name, expr) {
  stopifnot(is.character(name))
  stopifnot(is.name(expr) || is.atomic(expr) || is.call(expr))

  # If any nodes fail, delete assignment on all nodes
  tryCatch(
    cluster_call(.cl = cluster, assign_eval, name, expr),
    error = function(e) {
      cluster_rm(cluster, name)
      stop(e)
    }
  )

  invisible(cluster)
}

#' @rdname objman
#' @export
cluster_assign_value <- function(cluster, name, value) {
  stopifnot(is.character(name))

  cluster_call(.cl = cluster, assign_silent, name = name, value = value)
  invisible(cluster)
}

#' @rdname objman
#' @export
cluster_assign_each <- function(cluster, name, values) {
  stopifnot(is.character(name))
  stopifnot(is.list(values), length(values) == length(cluster))

  for (i in seq_along(values)) {
    cluster_assign_value(cluster[i], name, values[[i]])
  }

  invisible(cluster)
}

#' @rdname objman
#' @export
cluster_copy <- function(cluster, obj) {
  name <- substitute(obj)
  if (!is.name(name)) {
    stop("`obj` must be a single existing object", call. = FALSE)
  }

  cluster_assign_value(cluster, as.character(name), obj)

}

# Wrapper for assign that doesn't return value
assign_silent <- function(name, value, env = globalenv()) {
  assign(name, value, envir = env)
  invisible()
}
environment(assign_silent) <- globalenv()

assign_eval <- function(name, expr, env = globalenv()) {
  assign(name, eval(expr, envir = env), envir = env)
  invisible()
}
environment(assign_eval) <- globalenv()


#' @rdname objman
#' @export
cluster_get <- function(cluster, name) {
  stopifnot(is.character(name))

  cluster_call(.cl = cluster, get, name)
}


# Always suceeds: only gives warning if object not found, and warnings
# are not syndicated back to master
#' @rdname objman
#' @export
cluster_rm <- function(cluster, name) {
  stopifnot(is.character(name))

  cluster_call(cluster, rm, list = name, envir = globalenv())
  invisible(cluster)
}

#' @rdname objman
#' @export
cluster_ls <- function(cluster) {
  cluster_call(cluster, ls, envir = globalenv())
}
