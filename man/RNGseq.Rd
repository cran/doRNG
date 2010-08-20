\name{RNGseq}
\alias{RNGseq}
\title{Generate Sequence of Random Streams}
\description{
  Create a given number of rstream objects to be used as
  random number generators for each NMF run when performing
  multiple runs.
}
\usage{
  RNGseq(n, seed = NA, packed = TRUE, prefix = NULL, unlist
  = TRUE, verbose = FALSE)
}
\arguments{
  \item{n}{Number of streams to be created}

  \item{seed}{seed used to initialise the set of streams.
  If \code{NA}, then the streams are created using the
  current rstream seed. Otherwise, it is passed to
  \code{\link{doRNGseed}} to seed rstream.}

  \item{packed}{Logical. If TRUE the streams are returned
  packed (see \code{\link{rstream.packed}}).}

  \item{prefix}{a character string used as a prefix in the
  names of the \code{\link{rstream}} objects.}

  \item{unlist}{a logical that specifies if sequences of
  length 1 should be unlisted and returned as a single
  rstream object.}

  \item{verbose}{a logical to toggle verbosity.}
}
\value{
  a list of \code{\link{rstream}} objects (or a single
  rstream object if \code{n=1} and \code{unlist=TRUE}).
}
\details{
  This ensures complete reproducibility of the set of run.
  The streams are created using the RNGstream C++ package
  (from P. L'Ecuyer), using the interface provided by the R
  package rstream.

  If a seed is provided, the original rstream seed is
  restored on exit, so that the consistency of the global
  sequence of streams generated is not jeopardised.
}
\examples{
RNGseq(3)
RNGseq(3, packed=TRUE)
RNGseq(3, seed=1, packed=TRUE)
RNGseq(3, seed=1:6, verbose=TRUE)
RNGseq(3, prefix='myrng')
}

