\name{doRNGseed}
\alias{doRNGseed}
\title{Sets/Gets the Seed for Random Streams}
\description{
  Sets/Gets the Seed for Random Streams
}
\usage{
  doRNGseed(seed, verbose = FALSE)
}
\arguments{
  \item{seed}{a single or 6-length numeric. If missing then
  the current seed is returned.}

  \item{verbose}{logical to toggle verbose messages}
}
\value{
  the current seed (if argument), as a 6-length numeric
  vector.
}
\examples{
# store current (future old) doRNG seed
os <- doRNGseed()
\dontshow{ identical(os, CMRGseed()) }

# set doRNG seed generating the seed using the default R RNG
s <- doRNGseed(1)
# this returned the old seed
identical(os, s)
\dontshow{ identical(os, s) }

# set doRNG seed using current RNG
\dontshow{ os <- CMRGseed() }
doRNGseed(NULL)
\dontshow{ !identical(os, CMRGseed()) }

# directly set doRNG seed with a 6-length
doRNGseed(1:6)
identical(os, s)
\dontshow{ identical(1:6, CMRGseed()) }
}

