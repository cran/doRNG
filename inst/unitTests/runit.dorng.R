# Unit test for doRNG
# 
# Author: Renaud Gaujoux
# Creation: 28 Mar 2012
###############################################################################


#test.CMRGseed <- function(){
#	
#	msg <- function(...) paste(.msg, ':', ...)
#	
#	# Unit tests
#	.msg <- "Call CMRGseed without argument"
#	rs <- .Random.seed
#	checkIdentical( length(CMRGseed()), 7L, msg("Seed is of length 7") )
#	checkIdentical(rs, .Random.seed, msg("does not change .Random.seed"))
#	
#	.msg <- "Call CMRGseed with a single argument"
#	rs <- .Random.seed
#	checkIdentical( length(CMRGseed(1)), 7L, msg("Seed is of length 7") )
#	checkIdentical(rs, .Random.seed, msg("does not change .Random.seed"))
#	checkTrue( all(!is.na(CMRGseed(1))), msg("No NA in the returned seed") )
#	
#}

test.RNGseq <- function(){

	# actual testing function
	.test_loc <- function(.msg, n, ..., .list=TRUE, .change=FALSE){
		msg <- function(...) paste(.msg, ':', ...)
		os <- RNGscope()
		on.exit(RNGscope(os))
		
		s <- RNGseq(n, ...)
		
		if( !.change ) checkIdentical(RNGscope(), os, msg("the value of .Random.seed is not changed"))
		else checkTrue( !identical(RNGscope(), os), msg("the value of .Random.seed does change"))
		
		if( .list )	checkTrue(is.list(s), msg("result is a list"))
		else{
			checkTrue(is.integer(s), msg("result is an integer vector"))
			s <- list(s)
		}
		
		checkTrue(length(s) == n, msg("result has correct length"))
		checkTrue(all(sapply(s, length) == 7L), msg("each element has length 7"))
		checkTrue(all(sapply(s, function(x) x[1] %% 100) == 7L), msg("each element has correct RNG kind"))
		s
	}
	
	.test <- function(msg, n, ...){
		set.seed(1)
		s1 <- .test_loc(paste(msg, '- no seed'), n, ..., .change=TRUE)
		runif(1)
		s2 <- .test_loc(paste(msg, '- seed=1'), n, 1, ...)
		checkIdentical(s1, s2, paste(msg, " - set.seed(1) + no seed is identical to seed=1"))
		.test_loc(paste(msg, '- seed=1:6'), n, 1:6, ...)
	}
	.test("n=1", 1, .list=FALSE)
	.test("n=2", 2)
	.test("n=5", 5)
}

test.doRNGseed <- function(){
	
#	checkIdentical(doRNGseed(), CMRGseed(), "doRNGseed() and CMRGseed() return identical results")
	
	# actual testing function
	.test_loc <- function(.msg, ..., .change=FALSE){
		msg <- function(...) paste(.msg, ':', ...)
		os <- RNGscope()
		on.exit(RNGscope(os))
		s <- doRNGseed(...)
		checkTrue(length(s) == 7L && s[1] %% 100 == 7L, msg("doRNGseed returns a value of .Random.seed for L'Ecuyer-CMRG"))
		checkIdentical(RNGscope()[1], os[1], msg("doRNGseed does not change the type of RNG"))
		
		if( !.change ) checkIdentical(RNGscope(), os, msg("doRNGseed does not change the value of .Random.seed"))
		else checkTrue( !identical(RNGscope(), os), msg("doRNGseed changes the value of .Random.seed"))
		s
	}
	
	# test in two RNG settings: default and L'Ecuyer
	.test <- function(...){
		os <- RNGscope()
		on.exit(RNGscope(os))
		
		# default RNG
		s1 <- .test_loc(...)
		RNGkind("L'Ecuyer")
		s2 <- .test_loc(...)
		
		list(s1, s2)
	}
	
	# test different arguments
	set.seed(1)
	s1 <- .test("seed=missing", .change=TRUE)
	runif(1)
	set.seed(1)
	s2 <- .test("seed=NULL", NULL, .change=TRUE)
	checkIdentical(s1, s2, "seed=missing and seed=NULL return identical results")
	# doRNG seed with single numeric
	runif(10)
	s3 <- .test("seed=single numeric", 1)
	checkIdentical(s1[[1]], s3[[1]], "set.seed(1) + seed=missing and seed=1 return identical results")
	.test("seed=single integer", 10L)
	# directly set doRNG seed with a 6-length
	.test("seed=6-length integer", 1:6)
	.test("seed=6-length numeric", as.numeric(1:6))
	s <- 1:6
	checkIdentical(doRNGseed(s)[2:7], s, "doRNGseed(6-length) sets next .Random.seed to the given value")
	
	# errors
	os <- RNGscope()
	checkException(doRNGseed(NA), "seed=NA throws an exception")
	checkIdentical(os, RNGscope(), "doRNGseed(NA) does not change the value of .Random.seed [error]")
	
}

test.dorng <- function(){
	
		test_dopar <- function(.msg, s.seq){
			
			msg <- function(...) paste(.msg, ':', ...)
			
			# standard %dopar% loops are _not_ reproducible
			set.seed(1234)
			s1 <- foreach(i=1:4) %dopar% { runif(1) }
			set.seed(1234)
			s2 <- foreach(i=1:4) %dopar% { runif(1) }
			if( !missing(s.seq) ) checkTrue( !identical(s1, s2), msg("Standard %dopar% loop is not reproducible"))
			
			# %dorng% loops ensure reproducibility
			set.seed(1234)
			s1 <- foreach(i=1:4) %dorng% { runif(1) }
			set.seed(1234)
			s2 <- foreach(i=1:4) %dorng% { runif(1) }
			checkIdentical(s1, s2, msg("%dorng% loop is reproducible with set.seed"))
			# or 
			s1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
			s2 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
			checkIdentical(s1, s2, msg("%dorng% loop is reproducible with .options.RNG"))
			
			# separate %dorng% loops are different
			set.seed(1234)
			s1 <- foreach(i=1:4) %dorng% { runif(1) }
			s2 <- foreach(i=1:4) %dorng% { runif(1) }
			checkTrue( !identical(s1, s2), msg("two consecutive (unseeded) %dorng% loops are not identical"))
			
			# But the whole sequence of loops is reproducible
			set.seed(1234)
			s1.2 <- foreach(i=1:4) %dorng% { runif(1) }
			s2.2 <- foreach(i=1:4) %dorng% { runif(1) }
			checkTrue( identical(s1, s1.2) && identical(s2, s2.2), msg("two consecutive seeded %dorng% loops are identical"))
			
			s <- list(s1, s2)
			if( !missing(s.seq) )
				checkIdentical(s, s.seq, msg("result is identical to sequential computation"))
			
			s
		}
		
		library(doParallel)
		
		# Sequential computation
		registerDoSEQ()
		s.seq <- test_dopar("Sequential")
		
		# Multicore cluster
		registerDoParallel(cores=2)
		s <- test_dopar("Multicore", s.seq)
		registerDoSEQ()
		
		# SNOW-like cluster
		cl <- makeCluster(2)
		registerDoParallel(cl)
		test_dopar("SNOW-like cluster", s.seq)
		stopCluster(cl)
		registerDoSEQ()
		
		# Works with doMPI
		if( require(doMPI) ){
			cl <- startMPIcluster(2)
			registerDoMPI(cl)
			test_dopar("MPI cluster", s.seq) 
			closeCluster(cl)
			registerDoSEQ()
		}
	
}

test.registerDoRNG <- function(){
	
	library(doParallel)
	cl <- makeCluster(2)
	registerDoParallel(cl)
	
	# One can make existing %dopar% loops reproducible using %dorng% loops or registerDoRNG  
	r1 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
	registerDoRNG(1234)
	r2 <- foreach(i=1:4) %dopar% { runif(1) }
	checkIdentical(r1, r2, "registerDoRNG makes a %dopar% loop behave like a %dorng% loop")
	stopCluster(cl)
	
	# Registering another foreach backend disables doRNG
	cl <- makeCluster(3)
	registerDoParallel(cl)
	set.seed(1234)
	s1 <- foreach(i=1:4) %dopar% { runif(1) }
	s2 <- foreach(i=1:4, .options.RNG=1234) %dorng% { runif(1) }
	checkTrue( !identical(s1, s2), "Registering another foreach backend disables doRNG")
	
	# doRNG is re-nabled by re-registering it 
	registerDoRNG(1234)
	r3 <- foreach(i=1:4) %dopar% { runif(1) }
	checkIdentical(r2, r3, "doRNG is re-nabled by re-registering it")
	r4 <- foreach(i=1:4) %dopar% { runif(1) }
	# NB: the results are identical independently of the task scheduling
	# (r2 used 2 nodes, while r3 used 3 nodes)
	
	# Reproducibility of sequences of loops
	# pass seed to registerDoRNG 
	runif(10)
	registerDoRNG(1234)
	s3 <- foreach(i=1:4) %dopar% { runif(1) }
	s4 <- foreach(i=1:4) %dopar% { runif(1) }
	checkIdentical(s3, r3, "registerDoRNG(1234) allow reproducing sequences of %dopar% loops (1)")
	checkIdentical(s4, r4, "registerDoRNG(1234) allow reproducing sequences of %dopar% loops (2)")	
	# use set.seed
	runif(5)
	registerDoRNG()
	set.seed(1234)
	s5 <- foreach(i=1:4) %dopar% { runif(1) }
	s6 <- foreach(i=1:4) %dopar% { runif(1) }
	checkIdentical(s5, r3, "registerDoRNG() + set.seed give same results as registerDoRNG(1234) (1)")
	checkIdentical(s6, r4, "registerDoRNG() + set.seed give same results as registerDoRNG(1234) (2)")
	
	# argument `once=FALSE` reseed doRNG's seed at the beginning of each loop 
	registerDoRNG(1234, once=FALSE)
	r1 <- foreach(i=1:4) %dopar% { runif(1) }
	r2 <- foreach(i=1:4) %dopar% { runif(1) }
	checkIdentical(r1, r2, "argument `once=FALSE` reseed doRNG's seed at the beginning of each loop")
	checkIdentical(r1, r3)
	
	# Once doRNG is registered the seed can also be passed as an option to %dopar%
	r1.2 <- foreach(i=1:4, .options.RNG=456) %dopar% { runif(1) }
	r2.2 <- foreach(i=1:4, .options.RNG=456) %dopar% { runif(1) }	
	checkIdentical(r1.2, r2.2, "Once doRNG is registered the seed can also be passed as an option to %dopar%")
	checkTrue(!identical(r1.2, r1), "The seed passed as an option is really taken into account")
	
	stopCluster(cl)
	
}

