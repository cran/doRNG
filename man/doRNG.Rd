\name{doRNG}
\alias{\%dorng\%}
\alias{doRNG}
\alias{infoDoRNG}
\alias{registerDoRNG}
\title{Reproducible Parallel Foreach Backend}
\description{
  \code{infoDoRNG} returns information about the doRNG
  backend.

  \code{\%dorng\%} provides an alternative operator
  \code{\%dopar\%}, that ensures reproducible foreach
  loops.

  Note that (re-)registering a foreach backend other than
  doRNG, after a call to \code{registerDoRNG} disables
  doRNG -- which then needs to be registered.
}
\usage{
  infoDoRNG(data, item)

  obj \%dorng\% ex

  registerDoRNG(seed, once = TRUE)
}
\arguments{
  \item{data}{a list of data used by the backend}

  \item{item}{the data item requested, as a character
  string (e.g. 'name', 'workers', 'version')}

  \item{obj}{a foreach object as returned by a call to
  \code{\link{foreach}}.}

  \item{ex}{the \code{R} expression to evaluate.}

  \item{seed}{a numerical seed to use (as a single or
  6-length numerical value)}

  \item{once}{a logical to indicate if the RNG sequence
  should be seeded at the beginning of each loop or only at
  the first loop.}
}
\value{
  \code{infoDoRNG} returns the requested info (usually as a
  character string or a numeric value).

  \code{\%dorng\%} returns the result of the foreach loop.
  See \code{\link{\%dopar\%}}.
}
\examples{
if( require(doMC) ){

library(doMC)
registerDoMC()

# standard \%dopar\% loops are _not_ reproducible
set.seed(1234)
s1 <- foreach(i=1:4) \%dopar\% { runif(1) }
set.seed(1234)
s2 <- foreach(i=1:4) \%dopar\% { runif(1) }
identical(s1, s2)
\dontshow{ stopifnot( !identical(s1,s2) ) }

# single \%dorng\% loops are reproducible
s1 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
s2 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
identical(s1, s2)
\dontshow{ stopifnot( identical(s1,s2) ) }

# multiple \%dorng\% loops are reproducible
seed <- doRNGseed()
s1 <- foreach(i=1:4) \%dorng\% { runif(1) }
s2 <- foreach(i=1:4) \%dorng\% { runif(1) }

doRNGseed(seed)
s1.2 <- foreach(i=1:4) \%dorng\% { runif(1) }
s2.2 <- foreach(i=1:4) \%dorng\% { runif(1) }
identical(s1, s1.2) && identical(s2, s2.2)
\dontshow{ stopifnot( identical(s1,s1.2) && identical(s2,s2.2) ) }

}

# Works with doSNOW and doMPI

\dontrun{
library(doSNOW)
cl <- makeCluster(2)
registerDoSNOW(cl)

s1 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
s2 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
identical(s1, s2)
\dontshow{ stopifnot( identical(s1,s2) ) }

stopCluster(cl)
registerDoSEQ()

# Works with doMPI
library(doMPI)
cl <- startMPIcluster(2)
registerDoMPI(cl)

#' s1 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
s2 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
identical(s1, s2)
\dontshow{ stopifnot( identical(s1,s2) ) }

closeCluster(cl)
registerDoSEQ()
}

\dontshow{
if( Sys.info()['user'] == 'renaud' ){
library(doSNOW)
cl <- makeCluster(2)
registerDoSNOW(cl)

s1 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
s2 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
identical(s1, s2)
\dontshow{ stopifnot( identical(s1,s2) ) }

stopCluster(cl)
registerDoSEQ()

# Works with doMPI
library(doMPI)
cl <- startMPIcluster(2)
registerDoMPI(cl)

#' s1 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
s2 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
identical(s1, s2)
\dontshow{ stopifnot( identical(s1,s2) ) }

closeCluster(cl)
registerDoSEQ()
}
}
if( require(doMC) ){

library(doMC)
registerDoMC()

# One can make existing \%dopar\% loops reproducible using registerDoRNG
r1 <- foreach(i=1:4, .options.RNG=1234) \%dorng\% { runif(1) }
registerDoRNG(1234)
r2 <- foreach(i=1:4) \%dopar\% { runif(1) }
identical(r1, r2)
\dontshow{ stopifnot( identical(r1, r2) ) }

# Registering another foreach backend disables doRNG
registerDoMC()
s1 <- foreach(i=1:4) \%dopar\% { runif(1) }
s2 <- foreach(i=1:4) \%dopar\% { runif(1) }
identical(s1, s2)
\dontshow{ stopifnot( !identical(s1, s2) ) }

# doRNG is re-nabled by re-registering it
registerDoRNG(1234)
r2.2 <- foreach(i=1:4) \%dopar\% { runif(1) }
identical(r2, r2.2)
\dontshow{ stopifnot( identical(r2, r2.2) ) }

# argument `once=FALSE` reseed doRNG's seed at the begining each loop
registerDoRNG(1234, once=FALSE)
r1 <- foreach(i=1:4) \%dopar\% { runif(1) }
r2 <- foreach(i=1:4) \%dopar\% { runif(1) }
identical(r1, r2)
\dontshow{ stopifnot( identical(r1, r2) ) }

# Once doRNG is registered the seed can also be passed as an option to \%dopar\%
r1.2 <- foreach(i=1:4, .options.RNG=456) \%dopar\% { runif(1) }
r2.2 <- foreach(i=1:4, .options.RNG=456) \%dopar\% { runif(1) }
identical(r1.2, r2.2) && !identical(r1, r1.2)
\dontshow{ stopifnot( identical(r1.2, r2.2) && !identical(r1, r1.2) ) }

}
}
\author{
  Renaud Gaujoux
}
\seealso{
  \code{\link{foreach}}, \code{\link[doMC]{doMC}} ,
  \code{\link[doSNOW]{registerDoSNOW}},
  \code{\link[doMPI]{doMPI}}
}

