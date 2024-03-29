#' The \emph{doRNG} package provides functions to perform 
#' reproducible parallel foreach loops, using independent random streams
#' as generated by L'Ecuyer's combined multiple-recursive generator \citep{Lecuyer1999}. 
#' It enables to easily convert standard %dopar% loops into fully reproducible loops, 
#' independently of the number of workers, the task scheduling strategy, 
#' or the chosen parallel environment and associated foreach backend.
#' It has been tested with the following foreach backend: doMC, doSNOW, doMPI. 
#' 
#' @encoding UTF-8  
#' @name doRNG-package
#' @docType package
#' @title Generic Reproducible Parallel Backend for foreach Loops
#' @keywords package
#' @seealso \code{\link{doRNG}}, \code{\link{RNGseq}}
#' @bibliography ~/Documents/articles/library.bib
#' @cite Lecuyer1999
#' 
#' @import stats rngtools foreach
#' @examples
#' 
#' # register parallel backend
#' library(doParallel)
#' cl <- makeCluster(2)
#' registerDoParallel(cl)
#'
#' ## standard %dopar% loop are not reproducible
#' set.seed(123)
#' r1 <- foreach(i=1:4) %dopar%{ runif(1) }
#' set.seed(123)
#' r2 <- foreach(i=1:4) %dopar%{ runif(1) }
#' identical(r1, r2)
#' \dontshow{ stopifnot(!identical(r1, r2)) }
#' 
#' ## %dorng% loops _are_ reproducible
#' set.seed(123)
#' r1 <- foreach(i=1:4) %dorng%{ runif(1) }
#' set.seed(123)
#' r2 <- foreach(i=1:4) %dorng%{ runif(1) }
#' identical(r1, r2)
#' \dontshow{ stopifnot(identical(r1, r2)) }
#' 
#' # alternative way of seeding 
#' a1 <- foreach(i=1:4, .options.RNG=123) %dorng%{ runif(1) }
#' a2 <- foreach(i=1:4, .options.RNG=123) %dorng%{ runif(1) }
#' identical(a1, a2) && identical(a1, r1)
#' \dontshow{ stopifnot(identical(a1, a2) && identical(a1, r1)) }
#' 
#' ## sequences of %dorng% loops _are_ reproducible
#' set.seed(123)
#' s1 <- foreach(i=1:4) %dorng%{ runif(1) }
#' s2 <- foreach(i=1:4) %dorng%{ runif(1) }
#' identical(s1, r1) && !identical(s1, s2)
#' \dontshow{ stopifnot(identical(s1, r1) && !identical(s1, s2)) }
#'
#' set.seed(123)
#' s1.2 <- foreach(i=1:4) %dorng%{ runif(1) }
#' s2.2 <- foreach(i=1:4) %dorng%{ runif(1) }
#' identical(s1, s1.2) && identical(s2, s2.2)
#' \dontshow{ stopifnot(identical(s1, s1.2) && identical(s2, s2.2)) }
#'  
#' ## Non-invasive way of converting %dopar% loops into reproducible loops
#' registerDoRNG(123)
#' s3 <- foreach(i=1:4) %dopar%{ runif(1) }
#' s4 <- foreach(i=1:4) %dopar%{ runif(1) }
#' identical(s3, s1) && identical(s4, s2)
#' \dontshow{ stopifnot(identical(s3, s1) && identical(s4, s2)) }
#' 
#' stopCluster(cl) 
#'
NULL

`_PACKAGE` <- "doRNG"

