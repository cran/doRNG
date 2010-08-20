\name{RNGseq}
\alias{RNGseq}
\title{Generate Sequence of Random Streams}
\usage{
  RNGseq(n, seed = NULL, unlist = TRUE, verbose = FALSE)
}
\arguments{
  \item{n}{Number of streams to be created}

  \item{seed}{seed used to initialise the set of streams
  using \code{\link{doRNGseed}}.}

  \item{unlist}{a logical that specifies if sequences of
  length 1 should be unlisted and returned as a single
  vector.}

  \item{verbose}{a logical to toggle verbose messages.}
}
\value{
  a list of integer vectors (or a single integer vector if
  \code{n=1} and \code{unlist=TRUE}).
}
\description{
  Create a given number of seeds for L'Ecuyer's RNG, that
  can be used to seed parallel computation, making them
  fully reproducible.
}
\details{
  This ensures complete reproducibility of the set of run.
  The streams are created using L'Ecuyer's RNG, implemented
  in R core since version 2.14.0 under the name
  \code{"L'Ecuyer-CMRG"} (see \code{\link{RNG}}).

  The generation of the sequence should not affect the
  current RNG settings.
}
\examples{
RNGseq(3)
RNGseq(3)
RNGseq(3, seed=1)
RNGseq(3, seed=1:6, verbose=TRUE)
}

