# Development of a dorng equivalent to dopar for reproducible loops
# 
# Author: Renaud Gaujoux
# Creation: 17 Aug 2011
###############################################################################

# Check if an object is NA
isNA <- function(x) identical(x, NA) || identical(x, as.numeric(NA)) || identical(x, as.integer(NA))

#' Internal Functions Interfacing with the rstream Package
#' 
#' \code{.rstream.get.seed} returns the seed that rstream will use to generate 
#' the next random stream.
#' 
#' @return \code{.rstream.get.seed} returns the current value of the seed 
#' (i.e. a 6-length numeric vector)
#' 
#' @export
#' @rdname rstream
#' @keywords internal
.rstream.get.seed <- function(){
	get(".rstream.mrg32k3a.DefaultSeed", envir=rstream:::.rstream.envir)
}

#' \code{.rstream.set.seed} sets the seed that rstream will use to generate 
#' the next random stream.
#' 
#' @return \code{.rstream.set.seed} returns the old value of the seed 
#' (i.e. a 6-length numeric vector) 
#' 
#' @export
#' @rdname rstream
.rstream.set.seed <- function(seed){
	
	# check sed validity
	seed <- rstream:::.rstream.mrg32k3a.CheckSeed(seed)
	# retrieve current value
	old <- get(".rstream.mrg32k3a.DefaultSeed", envir=rstream:::.rstream.envir)
	
	## save seed in rstream library
	.Call("R_RngStreams_SetPackageSeed", as.double(seed), PACKAGE="rstream")	
	## save seed as R variable
	assign(".rstream.mrg32k3a.DefaultSeed",	as.double(seed), envir=rstream:::.rstream.envir)
	assign(".rstream.mrg32k3a.HasSeed", TRUE, envir=rstream:::.rstream.envir)
	
	# return old seed
	invisible(old)
}

#' \code{.RNGgenSeed} generates a 6-length numeric seed used for seeding 
#' random streams.
#' 
#' @param seed a single or 6-length numeric. If missing a random seed is 
#' generated using the current RNG. 
#' 
#' @return \code{.RNGgenSeed} returns the generated seed 
#' (i.e. a 6-length numeric vector)
#' 
#' @rdname rstream
#' @export
#' 
#' @examples
#' 
#' # generate random seed for rstream
#' .RNGgenSeed()
#' 
#' # generate random seed for rstream using seed for current RNG
#' # => this should not change the current value of .Random.seed 
#' rs <- .Random.seed
#' .RNGgenSeed(1)
#' identical(rs, .Random.seed)
#' 
#' \dontshow{
#' 
#' # Unit tests
#' rs <- .Random.seed
#' stopifnot( length(.RNGgenSeed()) == 6 )
#' stopifnot( !identical(rs, .Random.seed) )
#' 
#' rs <- .Random.seed
#' stopifnot( length(.RNGgenSeed(1)) == 6 )
#' stopifnot( identical(rs, .Random.seed) )
#' 
#' }
#' 
#' 
.RNGgenSeed <- function(seed){
	
	ru <- if( !missing(seed) ){
				
				# only use first element
				seed <- seed[1]
				rng <- new('rstream.runif', kind='default', seed=seed)
				r(rng, 6)
			}else
				runif(6)
	
	# between 0 and 999999
	ceiling(ru * 999999)
}

#' Sets/Gets the Seed for Random Streams
#' 
#' @param seed a single or 6-length numeric. If missing then the current seed is 
#' returned.
#' @param verbose logical to toggle verbosity messages
#' 
#' @return the current seed (if argument), as a 6-length numeric vector.
#' 
#' @export
#' @examples 
#' 
#' # store current (future old) doRNG seed  
#' os <- doRNGseed()
#' \dontshow{ identical(os, .rstream.get.seed()) }
#' 
#' # set doRNG seed generating the seed using the default R RNG
#' s <- doRNGseed(1)
#' # this returned the old seed
#' identical(os, s)
#' \dontshow{ identical(os, s) }
#' 
#' # set doRNG seed using current RNG
#' \dontshow{ os <- .rstream.get.seed() }
#' doRNGseed(NULL)
#' \dontshow{ !identical(os, .rstream.get.seed()) }
#' 
#' # directly set doRNG seed with a 6-length
#' doRNGseed(1:6)
#' identical(os, s)
#' \dontshow{ identical(1:6, .rstream.get.seed()) }
#'  
doRNGseed <- function(seed, verbose=FALSE){
		
	# retrieve current seed
	oldseed <- .rstream.get.seed()
	# return current value if missing seed
	if( missing(seed) ) return(invisible(oldseed))
	# generate seed if necessary
	if( is.null(seed) ){
		if( verbose ) message("# Generate RNGstream random seed ... ", appendLF=FALSE)
		seed <- .RNGgenSeed()
		if( verbose ) message("OK")
	}else if( is.numeric(seed) ){
		if( length(seed) == 1 ){
			if( verbose ) message("# Generate RNGstream random seed from ", seed, " ... ", appendLF=FALSE)
			seed <- .RNGgenSeed(seed)
			if( verbose ) message("OK")
		}
		else if( length(seed) != 6 )
			stop("doRNGseed - Invalid numeric seed: should be a numeric of length 1 or 6")		
	}else if( !isNA(seed) )
		stop("doRNGseed - Invalid seed value: should be a single numeric, NULL or NA")
	
	if( verbose ) message("# Setting RNGstream random seed to: ", paste(seed, collapse=', '), " ... ", appendLF=FALSE)
	.rstream.set.seed(seed)
	invisible(oldseed)
	
}

#' Generate Sequence of Random Streams
#' 
#' Create a given number of rstream objects to be used as random number generators
#' for each NMF run when performing multiple runs.
#' 
#' This ensures complete reproducibility of the set of run. 
#' The streams are created using the RNGstream C++ package (from P. L'Ecuyer), 
#' using the interface provided by the R package rstream.
#' 
#' If a seed is provided, the original rstream seed is restored on exit, so that
#' the consistency of the global sequence of streams generated is not jeopardised.
#' 
#' @param n Number of streams to be created
#' @param seed seed used to initialise the set of streams. If \code{NA}, then the 
#' streams are created using the current rstream seed. Otherwise, it is passed to 
#' \code{\link{doRNGseed}} to seed rstream.
#' @param packed Logical. If TRUE the streams are returned packed 
#' (see \code{\link{rstream.packed}}).
#' @param prefix a character string used as a prefix in the names of the 
#' \code{\link{rstream}} objects.
#' @param unlist a logical that specifies if sequences of length 1 should be 
#' unlisted and returned as a single rstream object.
#' @param verbose a logical to toggle verbosity. 
#' 
#' @return a list of \code{\link{rstream}} objects (or a single rstream object if 
#' \code{n=1} and \code{unlist=TRUE}).
#' 
#' @export 
#' @examples
#' 
#' RNGseq(3)
#' RNGseq(3, packed=TRUE)
#' RNGseq(3, seed=1, packed=TRUE)
#' RNGseq(3, seed=1:6, verbose=TRUE)
#' RNGseq(3, prefix='myrng')
#' 
RNGseq <- function(n, seed=NA, packed=TRUE, prefix=NULL, unlist=TRUE, verbose=FALSE){
	
	# check parameters
	if( n <= 0 )
		stop("NMF::createStream - invalid value for 'n' [positive value expected]")
	
	# force the initial seed if provided
	if( !isNA(seed) ){
		oldseed <- doRNGseed(seed, verbose=verbose)
		on.exit({.rstream.set.seed(oldseed)}, add=TRUE)
	}
	
	# generate the sequence of streams
	res <- lapply(1:n, function(i){
				s <- new('rstream.mrg32k3a', name=if( !is.null(prefix) ) paste(prefix, i, sep="_") else NULL );
				rstream.packed(s) <- packed;
				s}
	)
	
	# return list or single RNG
	if( n==1 && unlist )
		res[[1]]
	else
		res
	
}


#' Reproducible Parallel Foreach Backend
#' 
#' \code{infoDoRNG} returns information about the doRNG backend.
#' 
#' @param data a list of data used by the backend
#' @param item the data item requested, as a character string 
#' (e.g. 'name', 'workers', 'version')
#' 
#' @return \code{infoDoRNG} returns the requested info (usually as a character 
#' string or a numeric value).
#' 
#' @export
#' @name doRNG
#' @aliases infoDoRNG
#' @rdname doRNG
#' @author Renaud Gaujoux
#' 
infoDoRNG <- function (data, item) 
{	
	switch(item
			, workers = data$backend$info(data$backend$data, "workers")
			, name = "doRNG"
			, version = "doRNG 1.0" 
			, NULL)
}

##% \code{doRNG} implements the generic reproducible foreach backend. It should 
##% not be called directly by the user.
##% 
##% @param obj a foreach description of the loop arguments
##% @param ex the lopp expression
##% @param envir the loop's evaluation environment
##% @param data configuration data of the doRNG backend
##% 
##% @rdname doRNG
doRNG <- function (obj, ex, envir, data){
		
			
	if( is.null(obj$options) )
		obj$options <- list()
	
	if( !'RNG' %in% names(obj$options) ){
		obj$options$RNG <- if( !data$once || data$nseed==0 ){
			#message("doRNG backend - use seed ", if( data$once ) "only once" else "for every loop", ":")
			data$seed
		}
		else
			NA
	}
	
	data$nseed <- data$nseed + 1					
	assign('data', data, pos=foreach:::.foreachGlobals)	
	
	# directly register (temporarly) the computing backend
	rngBackend <- getDoBackend()
	on.exit({setDoBackend(rngBackend)}, add=TRUE)
	setDoBackend(rngBackend$data$backend)
	do.call('%dorng%', list(obj, ex), envir=parent.frame())
}

##% Get/Sets the registered foreach backend's data
getDoBackend <- function(){	
	c(foreach:::getDoPar()
	, info= if( exists("info", where = foreach:::.foreachGlobals, inherits = FALSE) ) foreach:::.foreachGlobals$info else function(data, item) NULL)
}
setDoBackend <- function(backend){
	ob <- getDoBackend()
	do.call('setDoPar', backend)
	invisible(ob)
}

#' \code{\%dorng\%} provides an alternative operator \code{\%dopar\%}, that ensures reproducible 
#' foreach loops.
#' 
#' @param obj a foreach object as returned by a call to \code{\link{foreach}}.
#' @param ex the \code{R} expression to evaluate.
#' 
#' @return \code{\%dorng\%} returns the result of the foreach loop. See \code{\link{\%dopar\%}}. 
#' 
#' @export 
#' @rdname doRNG
#' @aliases doRNG
#' @usage obj \%dorng\% ex
#' @seealso \code{\link{foreach}}, \code{\link{rstream}}
#' , \code{\link[doMC]{doMC}}, \code{\link[doSNOW]{registerDoSNOW}}, \code{\link[doMPI]{doMPI}}
#' @examples 
#' 
#' if( require(doMC) ){
#' 
#' library(doMC)
#' registerDoMC()
#' 
#' # standard %dopar% loops are _not_ reproducible
#' set.seed(1234)
#' s1 <- foreach(i=1:4) %dopar% { runif(1) }
#' set.seed(1234)
#' s2 <- foreach(i=1:4) %dopar% { runif(1) }
#' identical(s1, s2)
#' \dontshow{ stopifnot( !identical(s1,s2) ) }
#' 
#' # single %dorng% loops are reproducible
#' s1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' s2 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' identical(s1, s2)
#' \dontshow{ stopifnot( identical(s1,s2) ) }
#' 
#' # multiple %dorng% loops are reproducible
#' seed <- doRNGseed()
#' s1 <- foreach(i=1:4) %dorng% { runif(1) }
#' s2 <- foreach(i=1:4) %dorng% { runif(1) }
#' 
#' doRNGseed(seed)
#' s1.2 <- foreach(i=1:4) %dorng% { runif(1) }
#' s2.2 <- foreach(i=1:4) %dorng% { runif(1) }
#' identical(s1, s1.2) && identical(s2, s2.2)
#' \dontshow{ stopifnot( identical(s1,s1.2) && identical(s2,s2.2) ) }
#' 
#' }
#' 
#' # Works with doSNOW and doMPI
#' 
#' \dontrun{
#' library(doSNOW)
#' cl <- makeCluster(2)
#' registerDoSNOW(cl)
#' 
#' s1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' s2 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' identical(s1, s2)
#' \dontshow{ stopifnot( identical(s1,s2) ) }
#' 
#' stopCluster(cl)
#' registerDoSEQ()
#' 
#' # Works with doMPI
#' library(doMPI)
#' cl <- startMPIcluster(2)
#' registerDoMPI(cl)
#' 
#' #' s1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' s2 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' identical(s1, s2)
#' \dontshow{ stopifnot( identical(s1,s2) ) }
#' 
#' closeCluster(cl)
#' registerDoSEQ()
#' }
#' 
#' \dontshow{
#' if( Sys.info()['user'] == 'renaud' ){
#' library(doSNOW)
#' cl <- makeCluster(2)
#' registerDoSNOW(cl)
#' 
#' s1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' s2 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' identical(s1, s2)
#' \dontshow{ stopifnot( identical(s1,s2) ) }
#' 
#' stopCluster(cl)
#' registerDoSEQ()
#' 
#' # Works with doMPI
#' library(doMPI)
#' cl <- startMPIcluster(2)
#' registerDoMPI(cl)
#' 
#' #' s1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' s2 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' identical(s1, s2)
#' \dontshow{ stopifnot( identical(s1,s2) ) }
#' 
#' closeCluster(cl)
#' registerDoSEQ()
#' }
#' }
#' 
`%dorng%` <- function(obj, ex){
	
	
	# if an RNG seed is provided then setup random streams 
	# and add the list of RNGs to use as an iterated arguments for %dopar%
	library(rstream)
	obj$argnames <- c(obj$argnames, '.RNG.stream')
	it <- iter(obj)
	argList <- as.list(it)
	
	# keep current RNG and restore it on exit (useful for the sequential backend doSEQ)
	RNG.old <- rstream.RNG()	
	on.exit({rstream.RNG(RNG.old)}, add=TRUE)
	
	# generate a sequence of streams
	obj$args$.RNG.stream <- RNGseq(length(argList), obj$options$RNG, packed=TRUE)
	if( is.null(obj$packages) || !('rstream' %in% obj$packages) )
		obj$packages <- c(obj$packages, 'rstream')
	
	# append code to the loop expression to set the RNG
	ex <- as.call(list(as.name('{'),
					quote({rstream.packed(.RNG.stream) <- FALSE; rstream.RNG(.RNG.stream);}),
					substitute(ex)))
	
	# call the standard %dopar% operator
	do.call('%dopar%', list(obj, ex), envir=parent.frame())
}

#' \code{registerDoRNG} registers the doRNG foreach backend.
#' Subsequent \%dopar\% loops are actually performed using the previously 
#' registered foreach backend, but the RNG is set, before each iteration, 
#' to an \code{\link{rstream}} object, that is part of a reproducible sequence 
#' of statistically independent random streams.
#' 
#' Note that (re-)registering a foreach backend other than doRNG, after a call 
#' to \code{registerDoRNG} disables doRNG -- which then needs to be registered.
#' 
#' @param seed a numerical seed to use (as a single or 6-length numerical value)
#' @param once a logical to indicate if the RNG sequence should be seeded at the 
#' beginning of each loop or only at the first loop. 
#' 
#' @export
#' @rdname doRNG
#' 
#' @examples 
#' 
#' if( require(doMC) ){
#' 
#' library(doMC)
#' registerDoMC()
#' 
#' # One can make existing %dopar% loops reproducible using registerDoRNG  
#' r1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
#' registerDoRNG(1234)
#' r2 <- foreach(i=1:4) %dopar% { runif(1) }
#' identical(r1, r2)
#' \dontshow{ stopifnot( identical(r1, r2) ) }
#' 
#' # Registering another foreach backend disables doRNG
#' registerDoMC()  
#' s1 <- foreach(i=1:4) %dopar% { runif(1) }
#' s2 <- foreach(i=1:4) %dopar% { runif(1) }
#' identical(s1, s2)
#' \dontshow{ stopifnot( !identical(s1, s2) ) }
#' 
#' # doRNG is re-nabled by re-registering it 
#' registerDoRNG(1234)
#' r2.2 <- foreach(i=1:4) %dopar% { runif(1) }
#' identical(r2, r2.2)
#' \dontshow{ stopifnot( identical(r2, r2.2) ) }
#' 
#' # argument `once=FALSE` reseed doRNG's seed at the begining each loop 
#' registerDoRNG(1234, once=FALSE)
#' r1 <- foreach(i=1:4) %dopar% { runif(1) }
#' r2 <- foreach(i=1:4) %dopar% { runif(1) }
#' identical(r1, r2)
#' \dontshow{ stopifnot( identical(r1, r2) ) }
#' 
#' # Once doRNG is registered the seed can also be passed as an option to %dopar%
#' r1.2 <- foreach(i=1:4, .options.RNG=456) %dopar% { runif(1) }
#' r2.2 <- foreach(i=1:4, .options.RNG=456) %dopar% { runif(1) }
#' identical(r1.2, r2.2) && !identical(r1, r1.2)
#' \dontshow{ stopifnot( identical(r1.2, r2.2) && !identical(r1, r1.2) ) }
#' 
#' }
#' 
registerDoRNG <- function(seed, once=TRUE){
	
	backend <- getDoBackend()
	
	# use stored backend if registerDoRNG was called repeatedly
	if( getDoParName() == 'doRNG' )
		backend <- backend$data$backend
	
	setDoPar(doRNG, list(seed=seed, once=once, nseed=0, backend=backend), infoDoRNG)	
	
}

###% Reproducibly Apply a Function over a List or Vector
###% 
###% @aliases xapply reproduce
###% 
###% \code{reproduce} and \code{xapply} are a reproducible versions 
###% of \code{\link{replicate}} and \code{\link{sapply}} respectively, 
###% that ensures the reproducibility of the results, when stochastic computations
###% are involved.
###% 
###% The reproducibility is achieved by using the interface provided by the package 
###% \code{link[package:rstream]{rstream}} to generate independent random streams 
###% that are used as the random number generator for each replicate.
###% 
###% @param n the number of replication as a single numeric (integer)
###% @param seed the main numerical seed used to initialize the sequence of random 
###% streams
###% @param expr the expression (language object, usually a call) to evaluate repeatedly
###% @param simplify logical; should the result be simplified to a vector or 
###% matrix if possible?
###% 
###% 
###%  
reproduce <- function (n, expr, seed=NULL, simplify = TRUE){
	f <- eval.parent(substitute(function(...) expr))
	xapply(integer(n), seed, f, simplify = simplify)
}

xapply <- function (X, FUN, seed=NULL, ..., simplify = TRUE, USE.NAMES = TRUE){
	
	# generate a sequence of streams
	.RNG.stream <- RNGseq(length(X), seed, packed=TRUE)
	
	# keep current RNG and restore it on exit (useful for the sequential backend doSEQ)
	RNG.old <- rstream.RNG()
	on.exit(rstream.RNG(RNG.old), add=TRUE)
	
	# append code to the loop expression to set the RNG	
	expr <- as.call(list(as.name('{'),
					quote({doRNGseed(.rng);}),
					quote(do.call(FUN, list(...)))))
	
	env <- environment(FUN)
	f <- eval(substitute(function(.rng, ..., FUN) expr), env)	
	mapply(f, .RNG.stream, X, MoreArgs=c(list(...), FUN=FUN), 
			SIMPLIFY = simplify, USE.NAMES= USE.NAMES)
}