# Development of a dorng equivalent to dopar for reproducible loops
# 
# Author: Renaud Gaujoux
# Creation: 17 Aug 2011
###############################################################################

# Check if an object is NA
isNA <- function(x) identical(x, NA) || identical(x, as.numeric(NA)) || identical(x, as.integer(NA))

#' Local RNG scope
#' 
#' Save and restore RNG settings enabling their local changes. 
#' 
#' @param seed numeric (integer) vector used to restore the RNG settings.
#' Typically the result of a call to \code{RNGscope()}.
#' @return a numeric (integer) vector  containing the current value of 
#' \code{\link{.Random.seed}} or \code{NULL}. 
#' 
#' @export
#' @seealso \code{\link{.Random.seed}}
#' 
#' @examples 
#' 
#' f <- function(){
#' 	orng <- RNGscope()
#'  on.exit(RNGscope(orng))
#' 	RNGkind('Marsaglia')
#' 	runif(10)
#' }
#' 
#' sample(NA)
#' s <- .Random.seed
#' f()
#' identical(s, .Random.seed)
#' \dontshow{ stopifnot(identical(s, .Random.seed)) }
#'  
RNGscope <- function(seed){
	
	res <- if( missing(seed) ){
				if( exists('.Random.seed', where = .GlobalEnv) )
					get('.Random.seed', .GlobalEnv)
			}else if( is.null(seed) ){
				if( exists('.Random.seed', where = .GlobalEnv) )
					rm('.Random.seed', envir = .GlobalEnv)
			}else{
				assign('.Random.seed', seed, .GlobalEnv)
			}
	invisible(res)
}

#' \code{CMRGseed} generates a 6-length numeric seed used for seeding 
#' multiple random streams for L'Ecuyer's RNG.
#' 
#' @param seed a single or 6-length numeric. If missing a random seed is 
#' generated using the current RNG, but will not change its current state.
#' @param normal.kind character string or \code{NULL} (default) that indicates 
#' the random normal generator to use. This argument is passed to 
#' \code{\link{RNGkind}} or \code{\link{set.seed}} depending if argument 
#' \code{seed} is missing or not.    
#' 
#' @return \code{CMRGseed} returns the generated seed (i.e. a 6-length numeric vector)
#' 
#' @export
#' 
#' @examples
#' 
#' # generate random seed for L'Ecuyer's RNG
#' CMRGseed()
#' 
#' # generate random seed for L'Ecuyer's RNG using a seed for current RNG
#' # => this should not change the current value of .Random.seed 
#' rs <- .Random.seed
#' CMRGseed(1)
#' identical(rs, .Random.seed)
#' 
#' \dontshow{
#' 
#' # Unit tests
#' rs <- .Random.seed
#' stopifnot( length(CMRGseed()) == 6 )
#' stopifnot( identical(rs, .Random.seed) )
#' 
#' rs <- .Random.seed
#' stopifnot( length(CMRGseed(1)) == 6 )
#' stopifnot( identical(rs, .Random.seed) )
#' stopifnot( all(!is.na(CMRGseed(1))) )
#' 
#' }
#' 
#' 
CMRGseed <- function(seed, normal.kind=NULL){
	
	orng <- RNGscope()
	on.exit(RNGscope(orng))
	if( !missing(seed) ){		
		# only use first element
		seed <- seed[1]
		set.seed(seed, kind="L'Ecuyer", normal.kind=normal.kind)		
	}else if( RNGkind()[1] != "L'Ecuyer-CMRG" ) 
		RNGkind(kind="L'Ecuyer", normal.kind=normal.kind)

	# return generated seed
	RNGscope()[2:7]
}

#' Sets/Gets the Seed for Random Streams
#' 
#' @param seed a single or 6-length numeric. If missing then the current seed is 
#' returned.
#' @param verbose logical to toggle verbose messages
#' 
#' @return the current seed (if argument), as a 6-length numeric vector.
#' 
#' @export
#' @examples 
#' 
#' # store current (future old) doRNG seed  
#' os <- doRNGseed()
#' \dontshow{ identical(os, CMRGseed()) }
#' 
#' # set doRNG seed generating the seed using the default R RNG
#' s <- doRNGseed(1)
#' # this returned the old seed
#' identical(os, s)
#' \dontshow{ identical(os, s) }
#' 
#' # set doRNG seed using current RNG
#' \dontshow{ os <- CMRGseed() }
#' doRNGseed(NULL)
#' \dontshow{ !identical(os, CMRGseed()) }
#' 
#' # directly set doRNG seed with a 6-length
#' doRNGseed(1:6)
#' identical(os, s)
#' \dontshow{ identical(1:6, CMRGseed()) }
#'  
doRNGseed <- function(seed, verbose=FALSE){
		
	# retrieve current seed
	oldseed <- CMRGseed()
	# return current value if missing seed
	if( missing(seed) ) return(invisible(oldseed))
	# generate seed if necessary
	if( is.null(seed) ){
		if( verbose ) message("# Generate RNGstream random seed ... ", appendLF=FALSE)
		seed <- CMRGseed()
		if( verbose ) message("OK")
	}else if( is.numeric(seed) ){
		if( length(seed) == 1 ){
			if( verbose ) message("# Generate RNGstream random seed from ", seed, " ... ", appendLF=FALSE)
			seed <- CMRGseed(seed)
			if( verbose ) message("OK")
		}
		else if( length(seed) != 6 )
			stop("doRNGseed - Invalid numeric seed: should be a numeric of length 1 or 6")		
	}else if( !isNA(seed) )
		stop("doRNGseed - Invalid seed value: should be a single numeric, NULL or NA")
	
	if( verbose ) message("# Setting RNGstream random seed to: ", paste(seed, collapse=', '), " ... ", appendLF=FALSE)
	RNGkind("L'Ecuyer")
	s <- RNGscope()
	s[2:7] <- seed
	RNGscope(s)
	if( verbose ) message("OK")
	invisible(oldseed)
	
}

#' Generate Sequence of Random Streams
#' 
#' Create a given number of seeds for L'Ecuyer's RNG, that can be used to seed 
#' parallel computation, making them fully reproducible.
#' 
#' This ensures complete reproducibility of the set of run. 
#' The streams are created using L'Ecuyer's RNG, implemented in R core since
#' version 2.14.0 under the name \code{"L'Ecuyer-CMRG"} (see \code{\link{RNG}}).
#' 
#' The generation of the sequence should not affect the current RNG settings. 
#' 
#' @param n Number of streams to be created
#' @param seed seed used to initialise the set of streams using \code{\link{doRNGseed}}.
#' @param unlist a logical that specifies if sequences of length 1 should be
#' unlisted and returned as a single vector.
#' @param verbose a logical to toggle verbose messages. 
#' 
#' @return a list of integer vectors (or a single integer  vector if 
#' \code{n=1} and \code{unlist=TRUE}).
#' 
#' @export 
#' @examples
#' 
#' RNGseq(3)
#' RNGseq(3)
#' RNGseq(3, seed=1)
#' RNGseq(3, seed=1:6, verbose=TRUE)
#' 
RNGseq <- function(n, seed=NULL, unlist=TRUE, verbose=FALSE){
	
	# check parameters
	if( n <= 0 )
		stop("NMF::createStream - invalid value for 'n' [positive value expected]")
	
	# restore RNG settings on exit
	orng <- RNGscope()
	on.exit(RNGscope(orng), add=TRUE)
	
	# force the initial seed
	doRNGseed(seed, verbose=verbose)
	
	# generate the sequence of streams
	s <- RNGscope()	
	res <- lapply(1:n, function(i){
		if( i == 1 ) s
		else s <<- nextRNGStream(s)				
	})
	
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
			, version = "doRNG 1.1" 
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
			NULL
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
#' @usage obj %dorng% ex
#' @seealso \code{\link{foreach}}, \code{\link[doMC]{doMC}}
#' , \code{\link[doSNOW]{registerDoSNOW}}, \code{\link[doMPI]{doMPI}}
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
	library(parallel)
	obj$argnames <- c(obj$argnames, '.RNG.stream')
	it <- iter(obj)
	argList <- as.list(it)
	
	# keep current RNG and restore it on exit (useful for the sequential backend doSEQ)
	RNG.old <- RNGscope()
	on.exit({RNGscope(RNG.old)}, add=TRUE)
	
	# generate a sequence of streams
	obj$args$.RNG.stream <- RNGseq(length(argList), obj$options$RNG)
	if( is.null(obj$packages) || !('doRNG' %in% obj$packages) )
		obj$packages <- c(obj$packages, 'doRNG')
	
	# append code to the loop expression to set the RNG
	ex <- as.call(list(as.name('{'),
					quote({RNGscope(.RNG.stream);}),
					substitute(ex)))
	
	# call the standard %dopar% operator
	do.call('%dopar%', list(obj, ex), envir=parent.frame())
}

#' \code{registerDoRNG} registers the doRNG foreach backend.
#' Subsequent \%dopar\% loops are actually performed using the previously 
#' registered foreach backend, but the RNG is set, before each iteration, 
#' with seeds, that generate a reproducible sequence of statistically 
#' independent random streams.
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
###% The reproducibility is achieved by using LEcuyer's RNG provided by R core
###% since R-2.14.0, to generate independent random streams 
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
#reproduce <- function (n, expr, seed=NULL, simplify = TRUE){
#	f <- eval.parent(substitute(function(...) expr))
#	xapply(integer(n), seed, f, simplify = simplify)
#}
#
#xapply <- function (X, FUN, seed=NULL, ..., simplify = TRUE, USE.NAMES = TRUE){
#	
#	# generate a sequence of streams
#	.RNG.stream <- RNGseq(length(X), seed, packed=TRUE)
#	
#	# keep current RNG and restore it on exit (useful for the sequential backend doSEQ)
#	RNG.old <- rstream.RNG()
#	on.exit(rstream.RNG(RNG.old), add=TRUE)
#	
#	# append code to the loop expression to set the RNG	
#	expr <- as.call(list(as.name('{'),
#					quote({doRNGseed(.rng);}),
#					quote(do.call(FUN, list(...)))))
#	
#	env <- environment(FUN)
#	f <- eval(substitute(function(.rng, ..., FUN) expr), env)	
#	mapply(f, .RNG.stream, X, MoreArgs=c(list(...), FUN=FUN), 
#			SIMPLIFY = simplify, USE.NAMES= USE.NAMES)
#}