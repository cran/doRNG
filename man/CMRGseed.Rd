\name{CMRGseed}
\alias{CMRGseed}
\title{\code{CMRGseed} generates a 6-length numeric seed used for seeding
multiple random streams for L'Ecuyer's RNG.}
\usage{
  CMRGseed(seed, normal.kind = NULL)
}
\arguments{
  \item{seed}{a single or 6-length numeric. If missing a
  random seed is generated using the current RNG, but will
  not change its current state.}

  \item{normal.kind}{character string or \code{NULL}
  (default) that indicates the random normal generator to
  use. This argument is passed to \code{\link{RNGkind}} or
  \code{\link{set.seed}} depending if argument \code{seed}
  is missing or not.}
}
\value{
  \code{CMRGseed} returns the generated seed (i.e. a
  6-length numeric vector)
}
\description{
  \code{CMRGseed} generates a 6-length numeric seed used
  for seeding multiple random streams for L'Ecuyer's RNG.
}
\examples{
# generate random seed for L'Ecuyer's RNG
CMRGseed()

# generate random seed for L'Ecuyer's RNG using a seed for current RNG
# => this should not change the current value of .Random.seed
rs <- .Random.seed
CMRGseed(1)
identical(rs, .Random.seed)

\dontshow{

# Unit tests
rs <- .Random.seed
stopifnot( length(CMRGseed()) == 6 )
stopifnot( identical(rs, .Random.seed) )

rs <- .Random.seed
stopifnot( length(CMRGseed(1)) == 6 )
stopifnot( identical(rs, .Random.seed) )
stopifnot( all(!is.na(CMRGseed(1))) )

}
}

