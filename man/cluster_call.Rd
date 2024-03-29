% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster-call.R
\name{cluster_call}
\alias{cluster_call}
\alias{cluster_send}
\title{Call a function on each node of a cluster}
\usage{
cluster_call(cluster, code, simplify = FALSE, ptype = NULL)

cluster_send(cluster, code)
}
\arguments{
\item{cluster}{A cluster.}

\item{code}{An expression to execute on each worker.}

\item{simplify}{Should the results be simplified from a list?
  * `TRUE`: simplify or die trying.
  * `NA`: simplify if possible.
  * `FALSE`: never try to simplify, always leaving as a list.

  `code` must return a vector of length one in order for simplification
  to succeed.}

\item{ptype}{If `simplify` is `TRUE`, use `ptype` to enforce the desired
output type.}
}
\value{
A list of results with one element for each worker in `cluster`.
}
\description{
`cluster_call()` executes the code on each worker and returns the results;
`cluster_send()` executes the code ignoring the result. Jobs are submitted
to workers in parallel, and then we wait until they're complete.
}
\examples{
cl <- default_cluster()

# Run code on each cluster and retrieve results
cluster_call(cl, Sys.getpid())
cluster_call(cl, runif(1))

# use ptype to simplify
cluster_call(cl, runif(1), simplify = TRUE)

# use cluster_send() to ignore results
cluster_send(cl, x <- runif(1))
cluster_call(cl, x, simplify = TRUE)
}
