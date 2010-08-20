\name{.rstream.get.seed}
\alias{.RNGgenSeed}
\alias{.rstream.get.seed}
\alias{.rstream.set.seed}
\title{Internal Functions Interfacing with the rstream Package}
\description{
  \code{.rstream.get.seed} returns the seed that rstream
  will use to generate the next random stream.

  \code{.rstream.set.seed} sets the seed that rstream will
  use to generate the next random stream.

  \code{.RNGgenSeed} generates a 6-length numeric seed used
  for seeding random streams.
}
\usage{
  .rstream.set.seed(seed)

  .RNGgenSeed(seed)
}
\arguments{
  \item{seed}{a single or 6-length numeric. If missing a
  random seed is generated using the current RNG.}
}
\value{
  \code{.rstream.get.seed} returns the current value of the
  seed (i.e. a 6-length numeric vector)

  \code{.rstream.set.seed} returns the old value of the
  seed (i.e. a 6-length numeric vector)

  \code{.RNGgenSeed} returns the generated seed (i.e. a
  6-length numeric vector)
}
\keyword{internal}
\examples{
# generate random seed for rstream
.RNGgenSeed()

# generate random seed for rstream using seed for current RNG
# => this should not change the current value of .Random.seed
rs <- .Random.seed
.RNGgenSeed(1)
identical(rs, .Random.seed)

\dontshow{

# Unit tests
rs <- .Random.seed
stopifnot( length(.RNGgenSeed()) == 6 )
stopifnot( !identical(rs, .Random.seed) )

rs <- .Random.seed
stopifnot( length(.RNGgenSeed(1)) == 6 )
stopifnot( identical(rs, .Random.seed) )

}
}

